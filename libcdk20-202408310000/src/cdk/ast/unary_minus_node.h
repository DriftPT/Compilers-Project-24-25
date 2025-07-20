#pragma once

#include <cdk/ast/unary_operation_node.h>

namespace cdk {

  /**
   * Class for describing the negation operator
   */
  class unary_minus_node : public unary_operation_node {
  public:
    unary_minus_node(int lineno, expression_node *arg) noexcept:
        unary_operation_node(lineno, arg) {
    }

    void accept(basic_ast_visitor *sp, int level) override { sp->do_unary_minus_node(this, level); }

  };

} // cdk
