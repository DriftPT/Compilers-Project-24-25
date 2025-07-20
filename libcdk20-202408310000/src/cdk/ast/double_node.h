#pragma once

#include <cdk/ast/literal_node.h>

namespace cdk {

  /**
   * Class for describing syntactic tree leaves for holding
   * floating point values. This class is a specific instantiation
   * of the literal_node template for the double type.
   */
  class double_node : public virtual literal_node<double> {
  public:
    double_node(int lineno, double d) noexcept:
        literal_node<double>(lineno, d) {
    }

    void accept(basic_ast_visitor *sp, int level) override { sp->do_double_node(this, level); }

  };

} // cdk
