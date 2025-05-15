#pragma once

#include <cdk/ast/unary_operation_node.h>

namespace udf {

  /**
   * Class for describing objects allocation nodes.
   */
  class objects_alloc_node: public cdk::unary_operation_node {
    
  public:
    objects_alloc_node(int lineno, cdk::expression_node *argument) :
        cdk::unary_operation_node(lineno, argument) {
    }

    void accept(basic_ast_visitor *sp, int level) { sp->do_objects_alloc_node(this, level); }

  };

} // udf