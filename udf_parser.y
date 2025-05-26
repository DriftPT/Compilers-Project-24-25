%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;          /* integer value */
  double                d;          /* double value */
  std::string          *s;          /* symbol name or string literal */

  cdk::basic_node      *node;       /* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;

  udf::block_node      *block;
  std::vector<std::string> *ids;
};

%token tAND tOR tNE tLE tGE tSIZEOF tOBJECTS //tOBJECTS?
%token tINPUT tWRITE tWRITELN
%token tPUBLIC tPRIVATE tFORWARD
%token tTYPE_STRING tTYPE_INT tTYPE_REAL tTYPE_POINTER tTYPE_AUTO tTYPE_VOID tTYPE_TENSOR
%token tIF tELIF tELSE
%token tFOR
%token tBREAK tCONTINUE tRETURN

%token <i> tINTEGER
%token <d> tREAL
%token <s> tIDENTIFIER tSTRING
%token <expression> tNULLPTR

/*%type <node> stmt //program
%type <sequence> stmts exprs
%type <expression> expr
%type <lvalue> lval*/

//TODO: NOT FINISHED TYPES
%type<node> instruction return iffalse
%type<sequence> file instructions opt_instructions 
%type<sequence> expressions opt_expressions
%type<expression> expression integer real opt_initializer
%type<lvalue> lvalue
%type<block> block

%type<node>     declaration  argdec  fordec  vardec fundec fundef
%type<sequence> declarations argdecs fordecs vardecs opt_vardecs
%type<node>     opt_forinit

%type<s> string
%type<type> data_type void_type
%type<ids> identifiers

%nonassoc tIF
%nonassoc tELIF tELSE

%right '='
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY



%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%


file         : /* empty */  { compiler->ast($$ = new cdk::sequence_node(LINE)); }
             | declarations { compiler->ast($$ = $1); }
             ;

declarations :              declaration { $$ = new cdk::sequence_node(LINE, $1);     }
             | declarations declaration { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

declaration  : vardec ';' { $$ = $1; }
             | fundec     { $$ = $1; }
             | fundef     { $$ = $1; }
             ;

vardec       : tFORWARD data_type  tID                         { $$ = new og::variable_declaration_node(LINE, tPUBLIC,  $2, *$3, nullptr); }
             | tPUBLIC  data_type  tID         opt_initializer { $$ = new og::variable_declaration_node(LINE, tPUBLIC,  $2, *$3, $4); }
             |          data_type  tID         opt_initializer { $$ = new og::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $3); }
             ; 
/*stmt : expr ';'                                     { $$ = new udf::evaluation_node(LINE, $1); }
     | tWRITE exprs ';'                             { $$ = new udf::print_node(LINE, $2, false); }
     | tWRITELN exprs ';'                           { $$ = new udf::print_node(LINE, $2, true); }
     | tFOR '(' exprs ';' exprs ';' exprs ')' stmt  { $$ = new udf::for_node(LINE, $3, $5, $7, $9); }
     | tIF '(' expr ')' stmt %prec tIFX             { $$ = new udf::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt tELSE stmt             { $$ = new udf::if_else_node(LINE, $3, $5, $7); }
     | '{' stmts '}'                                { $$ = $2; }
     ;

expr : tINTEGER              { $$ = new cdk::integer_node(LINE, $1); }
     | tSTRING               { $$ = new cdk::string_node(LINE, $1); }
     | tINPUT                { $$ = new udf::input_node(LINE); }
     | '-' expr %prec tUNARY { $$ = new cdk::unary_minus_node(LINE, $2); }
     | '+' expr %prec tUNARY { $$ = new cdk::unary_plus_node(LINE, $2); }
     | expr '+' expr         { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr         { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr         { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr         { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr         { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr         { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr         { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr         { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr         { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr         { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr         { $$ = new cdk::eq_node(LINE, $1, $3); }
     | '(' expr ')'          { $$ = $2; }
     | lval                  { $$ = new cdk::rvalue_node(LINE, $1); }
     | lval '=' expr         { $$ = new cdk::assignment_node(LINE, $1, $3); }
     ;

exprs : expr                 { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs ',' expr       { $$ = new cdk::sequence_node(LINE, $3, $1); }
      ;

lval : tIDENTIFIER             { $$ = new cdk::variable_node(LINE, $1); }
     ;
*/
%%
