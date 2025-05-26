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

#define NIL (new cdk::nil_node(LINE))
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
  udf::tensor_node     *tensor;
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

/*%type <node> stmt //prudfram
%type <sequence> stmts exprs
%type <expression> expr
%type <lvalue> lval*/

//TODO: NOT FINISHED TYPES
%type<node> instruction return iffalse 
%type<sequence> file instructions opt_instructions 
%type<sequence> expressions opt_expressions dims
%type<expression> expression integer real opt_initializer dim
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

vardec       : tFORWARD data_type  tIDENTIFIER                    { $$ = new udf::variable_declaration_node(LINE, tPUBLIC,  $2, *$3, nullptr); }
             | tPUBLIC  data_type  tIDENTIFIER  opt_initializer   { $$ = new udf::variable_declaration_node(LINE, tPUBLIC,  $2, *$3, $4); }
             |          data_type  tIDENTIFIER  opt_initializer   { $$ = new udf::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $3); }
             ; 

opt_vardecs  : /* empty */ { $$ = NULL; }
             | vardecs     { $$ = $1; }
             ;

vardecs      : vardec ';'          { $$ = new cdk::sequence_node(LINE, $1);     }
             | vardecs vardec ';'  { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

identifiers  : tIDENTIFIER                   { $$ = new std::vector<std::string>(); $$->push_back(*$1); delete $1; }
	        | identifiers ',' tIDENTIFIER   { $$ = $1; $$->push_back(*$3); delete $3; }
             ;

data_type    : tTYPE_STRING                     { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING);  }
             | tTYPE_INT                        { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT);     }
             | tTYPE_REAL                       { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE);  }
             | tTYPE_TENSOR '<' dims '>'        { $$ = new udf::tensor_node(LINE, $3) }//TODO: IDK
             | tTYPE_POINTER '<' data_type '>'  { $$ = cdk::reference_type::create(4, $3); }
             | tTYPE_POINTER '<' tTYPE_AUTO '>' { $$ = cdk::reference_type::create(4, nullptr); }
             ;

dims        : dim                  { $$ = new cdk::sequence_node(LINE, $1); }
            | dims ',' dim         { $$ = new cdk::sequence_node(LINE, $3, $1); }
            ;

dim         : tINTEGER             { $$ = $1; }
            ;

void_type   : tTYPE_VOID { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID);   }
            ;

fundec   :          data_type  tIDENTIFIER '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPRIVATE, $1, *$2, $4); }
         | tFORWARD data_type  tIDENTIFIER '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  $2, *$3, $5); }
         | tPUBLIC  data_type  tIDENTIFIER '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  $2, *$3, $5); }
         |          tTYPE_AUTO tIDENTIFIER '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPRIVATE, nullptr, *$2, $4); }
         | tFORWARD tTYPE_AUTO tIDENTIFIER '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  nullptr, *$3, $5); }
         | tPUBLIC  tTYPE_AUTO tIDENTIFIER '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  nullptr, *$3, $5); }
         |          void_type  tIDENTIFIER '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPRIVATE, $1, *$2, $4); }
         | tFORWARD void_type  tIDENTIFIER '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  $2, *$3, $5); }
         | tPUBLIC  void_type  tIDENTIFIER '(' argdecs ')' { $$ = new udf::function_declaration_node(LINE, tPUBLIC,  $2, *$3, $5); }
         ;

fundef   :         data_type  tIDENTIFIER '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPRIVATE, $1, *$2, $4, $6); }
         | tPUBLIC data_type  tIDENTIFIER '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPUBLIC,  $2, *$3, $5, $7); }
         |         tTYPE_AUTO tIDENTIFIER '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPRIVATE, nullptr, *$2, $4, $6); }
         | tPUBLIC tTYPE_AUTO tIDENTIFIER '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPUBLIC,  nullptr, *$3, $5, $7); }
         |         void_type  tIDENTIFIER '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPRIVATE, $1, *$2, $4, $6); }
         | tPUBLIC void_type  tIDENTIFIER '(' argdecs ')' block { $$ = new udf::function_definition_node(LINE, tPUBLIC,  $2, *$3, $5, $7); }
         ;

argdecs  : /* empty */         { $$ = new cdk::sequence_node(LINE);  }
         | argdec              { $$ = new cdk::sequence_node(LINE, $1);     }
         | argdecs ',' argdec  { $$ = new cdk::sequence_node(LINE, $3, $1); }
         ;

argdec   : data_type tIDENTIFIER { $$ = new udf::variable_declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); }
         ;

block    : '{' opt_vardecs opt_instructions '}' { $$ = new udf::block_node(LINE, $2, $3); }
         ;

fordec    : data_type tIDENTIFIER '=' expression { $$ = new udf::variable_declaration_node(LINE, tPRIVATE,  $1, *$2, $4); }
          ;
              
fordecs     : fordec             { $$ = new cdk::sequence_node(LINE, $1);     }
            | fordecs ',' fordec { $$ = new cdk::sequence_node(LINE, $3, $1); }
            ;

opt_forinit     : /* empty */     { $$ = new cdk::sequence_node(LINE, NIL); }
                | fordecs         { $$ = $1; }
                ;

return          : tRETURN             ';' { $$ = new udf::return_node(LINE, nullptr); }
                | tRETURN expression  ';' { $$ = new udf::return_node(LINE, new udf::tuple_node(LINE, $2)); }
                ;

opt_instructions  : /* empty */  { $$ = new cdk::sequence_node(LINE); }
                  | instructions { $$ = $1; }
                  ;

instruction     : tIF '(' expression ')' instruction                                            { $$ = new udf::if_node(LINE, $3, $5); }
                | tIF '(' expression ')' instruction iffalse                                    { $$ = new udf::if_else_node(LINE, $3, $5, $6); }
                | tFOR '(' opt_forinit ';' opt_expressions ';' opt_expressions ')' instruction  { $$ = new udf::for_node(LINE, $3, $5, $7, $9); }
                | expression ';'                                                                { $$ = new udf::evaluation_node(LINE, $1); }
                | tWRITE   expressions ';'                                                      { $$ = new udf::print_node(LINE, $2, false); }
                | tWRITELN expressions ';'                                                      { $$ = new udf::print_node(LINE, $2, true); }
                | tBREAK                                                                        { $$ = new udf::break_node(LINE);  }
                | tCONTINUE                                                                     { $$ = new udf::continue_node(LINE); }
                | return                                                                        { $$ = $1; }
                | block                                                                         { $$ = $1; }
                ;

iffalse         : tELSE instruction                               { $$ = $2; }
                | tELIF '(' expression ')' instruction            { $$ = new udf::if_node(LINE, $3, $5); }
                | tELIF '(' expression ')' instruction iffalse    { $$ = new udf::if_else_node(LINE, $3, $5, $6); }
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
