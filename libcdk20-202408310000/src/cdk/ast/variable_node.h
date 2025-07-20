#pragma once

#include <cdk/ast/lvalue_node.h>
#include <string>

namespace cdk {

  /**
   * Class for describing syntactic tree leaves for holding variables..
   */
  class variable_node : public lvalue_node {
    std::string _name;

  public:
    variable_node(int lineno, const char *s) noexcept: lvalue_node(lineno), _name(s) {}

    variable_node(int lineno, const std::string &s) noexcept: lvalue_node(lineno), _name(s) {}

    variable_node(int lineno, const std::string *s) noexcept: lvalue_node(lineno), _name(*s) {}

  public:
    const std::string &name() const { return _name; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_variable_node(this, level); }

  };

} // cdk
