#pragma once

#include <cdk/ast/expression_node.h>

namespace udf {

  /**
   * Class for describing tensor contraction nodes (t1 ** t2).
   */
  class tensor_contraction_node : public cdk::expression_node {
    cdk::expression_node *_tensor1;
    cdk::expression_node *_tensor2;

  public:
    tensor_contraction_node(int lineno, cdk::expression_node *tensor1, cdk::expression_node *tensor2) :
        cdk::expression_node(lineno), _tensor1(tensor1), _tensor2(tensor2) {
    }

    cdk::expression_node *tensor1() { return _tensor1; }
    cdk::expression_node *tensor2() { return _tensor2; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_tensor_contraction_node(this, level); }
    
  };

} // udf