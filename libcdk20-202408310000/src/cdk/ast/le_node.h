#pragma once

#include <cdk/ast/binary_operation_node.h>

namespace cdk {

  /**
   * Class for describing the lower-than-or-equal operator
   */
  class le_node : public binary_operation_node {
  public:
    le_node(int lineno, expression_node *left, expression_node *right) noexcept:
        binary_operation_node(lineno, left, right) {
    }

    void accept(basic_ast_visitor *sp, int level) override { sp->do_le_node(this, level); }

  };

} // cdk
