%option c++ prefix="udf_scanner_" outfile="udf_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <vector>
#include <sstream>
#include <cmath>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "ast/tensor_node.h"
#include "udf_parser.tab.h"

static std::vector<int> deferred;  // deferred tokens

// output stream for building string literals
static std::ostringstream strlit;

// don't change this
#define yyerror LexerError
%}

SPACE     ([ \t\r]|\n)
INT       [[:digit:]]+

LETTER  [A-Za-z]
IDENTIFIER   {LETTER}({LETTER}|[[:digit:]]|_)*

%x X_COMMENT

%x X_STRING X_HEX_INT X_BACKSLASH
%%

  /* ====================================================================== */
  /* ====[                      3.2 - COMMENTS                        ]==== */
  /* ====================================================================== */

"//".*$                /* match the whole line (it is ignored) */

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"/*"        yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>.|\n        ;

  /* ====================================================================== */
  /* ====[                  1.1   - Tipos de dados                    ]==== */
  /* ====================================================================== */

"auto"                   return tTYPE_AUTO; 
"int"                    return tTYPE_INT;
"real"                   return tTYPE_REAL;
"string"                 return tTYPE_STRING;
"tensor"                 return tTYPE_TENSOR;
"ptr"                    return tTYPE_POINTER;
"void"                   return tTYPE_VOID;

  /* ====================================================================== */
  /* ====[                  4.5 - Símbolos globais                    ]==== */
  /* ====================================================================== */

"forward"                return tFORWARD;
"public"                 return tPUBLIC;

  /* ====================================================================== */
  /* ====[                 6.2 - Instrução condicional                ]==== */
  /* ====================================================================== */

"if"                   return tIF;
"elif"                 return tELIF;
"else"                 return tELSE;

  /* ====================================================================== */
  /* ====[                 6.3 - Instrução de iteração                ]==== */
  /* ====[                 6.4 - Instrução de terminação              ]==== */
  /* ====[                 6.5 - Instrução de continuação             ]==== */
  /* ====[                 6.6 - Instrução de retorno                 ]==== */
  /* ====[                 6.8 - Instruções de impressão              ]==== */
  /* ====================================================================== */

"for"                   return tFOR;
"break"                 return tBREAK;
"continue"              return tCONTINUE;
"return"                return tRETURN;
"write"                 return tWRITE;
"writeln"               return tWRITELN;

  /* ====================================================================== */
  /* ====[                 Operadores especiais                       ]==== */
  /* ====================================================================== */

[()\[\]{},.;:]         return *yytext;

  /* ====================================================================== */
  /* ====[                 7 - Operadores de expressões               ]==== */
  /* ====[                 7 - Expressões especiais                   ]==== */
  /* ====================================================================== */

[-+*/%~<>@?!=]         return *yytext;
"=="                   return tEQ;
"!="                   return tNE;
"<="                   return tLE;
">="                   return tGE;
"&&"                   return tAND;
"||"                   return tOR;
"**"                   return tCONTRACT; 

"input"                return tINPUT;
"nullptr"              return tNULLPTR;
"objects"              return tOBJECTS;
"sizeof"               return tSIZEOF;

  /* ====================================================================== */
  /* ====[           7.3 Expressões dependentes de tensores           ]==== */
  /* ====================================================================== */

"capacity"             return tCAPACITY;
"rank"                 return tRANK;
"dims"                 return tDIMS;
"dim"                  return tDIM;
"reshape"              return tRESHAPE;

  /* ====================================================================== */
  /* ====[                   3.8.1 - inteiros                         ]==== */
  /* ====================================================================== */

"0x"                   yy_push_state(X_HEX_INT);

<X_HEX_INT>[[:xdigit:]]+    {
                              errno = 0;
                              unsigned long val = strtoul(yytext, NULL, 16);
                              if ((errno == ERANGE) || val > INT_MAX) {
                                yyerror("integer literal overflow");
                              }
                              yylval.i = val;
                              yy_pop_state();
                              return tINTEGER;
                            }

<X_HEX_INT>.|\n             { std::cerr << "WARNING: line " << lineno() << ": bad hexadecimal data!" << std::endl; exit(1);}

 {INT}                      {   
                              errno = 0;
                              unsigned long val = strtoul(yytext, NULL, 10);
                              if ((errno == ERANGE) || val > INT_MAX) {
                                yyerror("integer literal overflow");
                              }
                              yylval.i = val;
                              return tINTEGER;
                            }

  /* ====================================================================== */
  /* ====[                   3.8.2 - reais                            ]==== */
  /* ====================================================================== */

([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)?|[0-9]+[eE][-+]?[0-9]+ {
  errno = 0;
  yylval.d = strtod(yytext, NULL);
  if ((yylval.d == HUGE_VAL || yylval.d == -HUGE_VAL) && errno == ERANGE) {
    yyerror("real literal overflow");
  }
  return tREAL; 
}
  
  /* ====================================================================== */
  /* ====[                3.7 - Identificadores (nomes)               ]==== */
  /* ====================================================================== */

{IDENTIFIER}  yylval.s = new std::string(yytext); return tIDENTIFIER;

  /* ====================================================================== */
  /* ====[              3.8.3 - Cadeias de caracteres                 ]==== */
  /* ====================================================================== */

"\""                           yy_push_state(X_STRING);
<X_STRING>\\                   yy_push_state(X_BACKSLASH);
<X_STRING>"\""                 {
                                 yylval.s = new std::string(strlit.str());
                                 strlit.str("");
                                 yy_pop_state();
                                 return tSTRING;
                               }
<X_STRING>.                    strlit << *yytext;

<X_BACKSLASH>n                 strlit << '\n'; yy_pop_state();
<X_BACKSLASH>r                 strlit << '\r'; yy_pop_state();
<X_BACKSLASH>t                 strlit << '\t'; yy_pop_state();
<X_BACKSLASH>\"                strlit << '"'; yy_pop_state();
<X_BACKSLASH>\\                strlit << '\\'; yy_pop_state();

<X_BACKSLASH>[[:xdigit:]]{1,2} {
                                 strlit << (char)(unsigned char)strtoul(yytext, NULL, 16);
                                 yy_pop_state();
                               }
<X_BACKSLASH>.                 strlit << *yytext; yy_pop_state();

  /* ====================================================================== */
  /* ====[                    (other characters)                      ]==== */
  /* ====================================================================== */

[[:space:]]         /* ignore remaining white space inside the program */
.                      yyerror(yytext);    /* all other chars: error! */

  /* ====================================================================== */
  /* ====[                           THE END                          ]==== */
  /* ====================================================================== */

%%
