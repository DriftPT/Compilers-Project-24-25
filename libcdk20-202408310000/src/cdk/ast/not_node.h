#pragma once

#include <cdk/ast/unary_operation_node.h>

namespace cdk {

  /**
   * Class for describing the not operator
   */
  class not_node : public unary_operation_node {
  public:
    not_node(int lineno, expression_node *argument) noexcept:
        unary_operation_node(lineno, argument) {
    }

    void accept(basic_ast_visitor *sp, int level) override { sp->do_not_node(this, level); }

  };

} // cdk
