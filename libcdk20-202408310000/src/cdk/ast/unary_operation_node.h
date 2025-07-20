#pragma once

#include <cdk/ast/expression_node.h>

namespace cdk {

  /** Class for describing unary operations. */
  class unary_operation_node : public expression_node {
    expression_node *_argument;

  public:
    unary_operation_node(int lineno, expression_node *arg) noexcept:
        expression_node(lineno), _argument(arg) {
    }

    expression_node *argument() noexcept { return _argument; }

  };

} // cdk
