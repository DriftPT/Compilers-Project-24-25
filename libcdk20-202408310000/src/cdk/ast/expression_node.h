#pragma once

#include <cdk/ast/typed_node.h>

namespace cdk {

  /**
   * Expressions are typed nodes that have a value.
   */
  class expression_node : public typed_node {
  protected:
    expression_node(int lineno) noexcept:
        typed_node(lineno) {
    }

  };

} // cdk
