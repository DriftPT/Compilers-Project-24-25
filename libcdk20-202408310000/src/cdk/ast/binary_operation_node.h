#pragma once

#include <cdk/ast/expression_node.h>

namespace cdk {

  /**
   * Class for describing binary operations.
   */
  class binary_operation_node : public expression_node {
    expression_node *_left, *_right;

  public:
    /**
     * @param lineno source code line number for this node
     * @param left first operand
     * @param right second operand
     */
    binary_operation_node(int lineno, expression_node *left, expression_node *right) noexcept:
        expression_node(lineno), _left(left), _right(right) {
    }

    expression_node *left() noexcept { return _left; }

    expression_node *right() noexcept { return _right; }

  };

} // cdk
