#pragma once

#include <cdk/ast/literal_node.h>

namespace cdk {

  /**
   * Class for describing syntactic tree leaves for holding
   * integer values. This class is a specific instantiation
   * of the literal_node template for the int type.
   */
  class integer_node : public virtual literal_node<int> {
  public:
    integer_node(int lineno, int i) noexcept:
        literal_node<int>(lineno, i) {
    }

    void accept(basic_ast_visitor *sp, int level) override { sp->do_integer_node(this, level); }

  };

} // cdk
