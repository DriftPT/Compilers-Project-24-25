#pragma once

#include <cdk/ast/unary_operation_node.h>

namespace cdk {

  /**
   * Class for describing the unary '+' operator
   */
  class unary_plus_node : public unary_operation_node {
  public:
    unary_plus_node(int lineno, expression_node *arg) noexcept:
        unary_operation_node(lineno, arg) {
    }

    void accept(basic_ast_visitor *sp, int level) override { sp->do_unary_plus_node(this, level); }

  };

} // cdk
