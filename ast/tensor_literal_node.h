#pragma once

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {
    
    /**
     * Class for describing tensor literal nodes.
     */

  class tensor_literal_node : public cdk::expression_node {
    cdk::sequence_node *_values;

  public:
    tensor_literal_node(int lineno, cdk::sequence_node *values) :
        cdk::expression_node(lineno), _values(values) {
    }

    cdk::sequence_node *values() const { return _values; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_literal_node(this, level);
    }
  };
} // udf