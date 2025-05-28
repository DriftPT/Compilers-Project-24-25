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

//TODO: melhor forma de se fazer?
static std::vector<size_t> convert_sequence_to_vector(cdk::sequence_node *seq) {
  std::vector<size_t> dims;
  for (size_t i = 0; i < seq->size(); ++i) {
    auto int_node = dynamic_cast<cdk::integer_node*>(seq->node(i));
    dims.push_back(int_node->value());
  }
  return dims;
}
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

%token tAND tOR tNE tLE tGE tSIZEOF tOBJECTS 
%token tINPUT tWRITE tWRITELN
%token tPUBLIC tPRIVATE tFORWARD
%token tTYPE_STRING tTYPE_INT tTYPE_REAL tTYPE_POINTER tTYPE_AUTO tTYPE_VOID tTYPE_TENSOR
%token tIF tELIF tELSE
%token tFOR
%token tBREAK tCONTINUE tRETURN
%token tCAPACITY tRANK tDIMS tDIM tRESHAPE

%token <i> tINTEGER
%token <d> tREAL
%token <s> tIDENTIFIER tSTRING
%token <expression> tNULLPTR

%type<node> instruction return iffalse 
%type<sequence> file instructions opt_instructions 
%type<sequence> expressions opt_expressions expressions_int
%type<expression> expression integer real opt_initializer
%type<lvalue> lvalue
%type<block> block

%type<node>     declaration  argdec  fordec  vardec fundec fundef
%type<sequence> declarations argdecs fordecs vardecs opt_vardecs
%type<sequence> opt_forinit

%type<tensor> tensor_literal
%type<sequence> tensor_elements
/*
%type <expression> tensor_literal tensor_row
%type <sequence> tensor_rows*/

%type<s> string
%type<type> data_type void_type

%nonassoc tIF
%nonassoc tELIF tELSE

%right '='
%left tOR
%left tAND
%right '~'
%left tNE tEQ
%left '<' tLE tGE '>'
%left '+' '-'
%left tCONTRACT '*' '/' '%' //TODO: tCONTRACT Esta operação tem uma precedência imediatamente superior à da multiplicação habitual.
%left '.' '@'
%right tUMINUS


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


data_type    : tTYPE_STRING                          { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING);  }
             | tTYPE_INT                             { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT);     }
             | tTYPE_REAL                            { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE);  }
             | tTYPE_TENSOR '<' expressions_int '>'  { $$ = cdk::tensor_type::create(convert_sequence_to_vector($3)); }
             | tTYPE_POINTER '<' data_type '>'       { $$ = cdk::reference_type::create(4, $3); }
             | tTYPE_POINTER '<' tTYPE_AUTO '>'      { $$ = cdk::reference_type::create(4, nullptr); }
             ;

opt_initializer  : /* empty */         { $$ = nullptr; /* must be nullptr, not NIL */ }
                 | '=' expression      { $$ = $2; }
                 ;

expressions_int
    : integer                             { $$ = new cdk::sequence_node(LINE, $1); }
    | expressions_int ',' integer         { $$ = new cdk::sequence_node(LINE, $3, $1); }
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
                | tRETURN expression  ';' { $$ = new udf::return_node(LINE, $2); }
                ;

opt_instructions  : /* empty */  { $$ = new cdk::sequence_node(LINE); }
                  | instructions { $$ = $1; }
                  ;

instructions    : instruction                { $$ = new cdk::sequence_node(LINE, $1);     }
                | instructions instruction   { $$ = new cdk::sequence_node(LINE, $2, $1); }
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

opt_expressions : /* empty */         { $$ = new cdk::sequence_node(LINE); }
                | expressions         { $$ = $1; }
                ;

expressions     : expression                     { $$ = new cdk::sequence_node(LINE, $1);     }
                | expressions ',' expression     { $$ = new cdk::sequence_node(LINE, $3, $1); }
                ;


expression      : integer                       { $$ = $1; }
                | real                          { $$ = $1; }
                | tensor_literal                { $$ = $1; }
                | string                        { $$ = new cdk::string_node(LINE, $1); }
                | tNULLPTR                      { $$ = new udf::nullptr_node(LINE); }
                /* LEFT VALUES */
                | lvalue                        { $$ = new cdk::rvalue_node(LINE, $1); }
                /* ASSIGNMENTS */
                | lvalue '=' expression         { $$ = new cdk::assignment_node(LINE, $1, $3); }
                /* ARITHMETIC EXPRESSIONS */
                | expression '+' expression    { $$ = new cdk::add_node(LINE, $1, $3); }
                | expression '-' expression    { $$ = new cdk::sub_node(LINE, $1, $3); }
                | expression '*' expression    { $$ = new cdk::mul_node(LINE, $1, $3); }
                | expression '/' expression    { $$ = new cdk::div_node(LINE, $1, $3); }
                | expression '%' expression    { $$ = new cdk::mod_node(LINE, $1, $3); }
                /* LOGICAL EXPRESSIONS */
                | expression  '<' expression    { $$ = new cdk::lt_node(LINE, $1, $3); }
                | expression tLE  expression    { $$ = new cdk::le_node(LINE, $1, $3); }
                | expression tEQ  expression    { $$ = new cdk::eq_node(LINE, $1, $3); }
                | expression tGE  expression    { $$ = new cdk::ge_node(LINE, $1, $3); }
                | expression  '>' expression    { $$ = new cdk::gt_node(LINE, $1, $3); }
                | expression tNE  expression    { $$ = new cdk::ne_node(LINE, $1, $3); }
                /* LOGICAL EXPRESSIONS */
                | expression tAND  expression    { $$ = new cdk::and_node(LINE, $1, $3); }
                | expression tOR   expression    { $$ = new cdk::or_node (LINE, $1, $3); }
                /* UNARY EXPRESSION */
                | '-' expression %prec tUMINUS  { $$ = new cdk::unary_minus_node(LINE, $2); }
                | '+' expression %prec tUMINUS  { $$ = $2; }
                | '~' expression                { $$ = new cdk::not_node(LINE, $2); }
                /* TENSORS EXPRESSION*/
                | expression '.' tCAPACITY                          { $$ = new udf::tensor_capacity_node(LINE, $1); }
                | expression '.' tRANK                              { $$ = new udf::tensor_rank_node(LINE, $1); }
                | expression '.' tDIMS                              { $$ = new udf::tensor_dims_node(LINE, $1); }
                | expression '.' tDIM '(' expression ')'            { $$ = new udf::tensor_dim_node(LINE, $1, $5); }
                | expression '.' tRESHAPE '(' expressions_int ')'   { $$ = new udf::tensor_reshape_node(LINE, $1, $5); }                                                    
                | expression tCONTRACT expression                   { $$ = new udf::tensor_contraction_node(LINE, $1, $3); }                                                                                     
                /* OTHER EXPRESSION */
                | tINPUT                        { $$ = new udf::input_node(LINE); }
                /* OTHER EXPRESSION */
                | tIDENTIFIER '(' opt_expressions ')'   { $$ = new udf::function_call_node(LINE, *$1, $3); delete $1; }
                | tSIZEOF '(' expression ')'            { $$ = new udf::sizeof_node(LINE, $3); }
                /* OTHER EXPRESSION */
                | '(' expression ')'                     { $$ = $2; }
                | tOBJECTS '(' expression ')'            { $$ = new udf::objects_alloc_node(LINE, $3); }
                | lvalue '?'                             { $$ = new udf::address_of_node(LINE, $1); }
                ;

lvalue          : tIDENTIFIER                                            { $$ = new cdk::variable_node(LINE, *$1); delete $1; }
                | lvalue             '[' expression ']'                  { $$ = new udf::index_node(LINE, new cdk::rvalue_node(LINE, $1), $3); }
                | '(' expression ')' '[' expression ']'                  { $$ = new udf::index_node(LINE, $2, $5); }
                | tIDENTIFIER '(' opt_expressions ')' '[' expression ']' { $$ = new udf::index_node(LINE, new udf::function_call_node(LINE, *$1, $3), $6); }
                | expression '@' '(' expressions_int ')'                 { $$ = new udf::tensor_index_node(LINE, $1, $4); } 
                ;

integer         : tINTEGER                      { $$ = new cdk::integer_node(LINE, $1); };
real            : tREAL                         { $$ = new cdk::double_node(LINE, $1); };
tensor_literal  : '[' tensor_elements ']'       { $$ = new udf::tensor_node(LINE, $2); };
string          : tSTRING                       { $$ = $1; }
                | string tSTRING                { $$ = $1; $$->append(*$2); delete $2; }
                ;


tensor_elements   : /* empty */                                      { $$ = new cdk::sequence_node(LINE); }
                  | '[' expressions ']'                              { $$ = new cdk::sequence_node(LINE, $2); }
                  | tensor_elements ',' '[' expressions ']'          { $$ = new cdk::sequence_node(LINE, $4, $1); }
                  ;

%%
