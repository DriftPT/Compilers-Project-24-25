#pragma once

#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>

namespace cdk {

  /**
   * Class for describing assignment nodes.
   */
  class assignment_node : public expression_node {
    lvalue_node *_lvalue;
    expression_node *_rvalue;

  public:
    assignment_node(int lineno, lvalue_node *lvalue, expression_node *rvalue) noexcept:
        expression_node(lineno), _lvalue(lvalue), _rvalue(rvalue) {
    }

    lvalue_node *lvalue() noexcept { return _lvalue; }

    expression_node *rvalue() noexcept { return _rvalue; }

    void accept(basic_ast_visitor *sp, int level) override { sp->do_assignment_node(this, level); }

  };

} // cdk
