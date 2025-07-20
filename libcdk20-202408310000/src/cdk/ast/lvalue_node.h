#pragma once

#include <cdk/ast/typed_node.h>
#include <string>

namespace cdk {

  /**
   * Class for describing syntactic tree leaves for lvalues.
   */
  class lvalue_node : public typed_node {
  protected:
    lvalue_node(int lineno) noexcept:
        typed_node(lineno) {
    }

  };

} // cdk
