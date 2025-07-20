#pragma once

#include <cdk/ast/literal_node.h>
#include <string>

namespace cdk {

  /**
   * Class for describing syntactic tree leaves for holding string
   * values. This class is a specific instantiation of the
   * literal_node template for the string type.
   */
  class string_node : public virtual literal_node<std::string> {
  public:
    string_node(int lineno, const char *s) noexcept: literal_node<std::string>(lineno, s) {}

    string_node(int lineno, const std::string &s) noexcept: literal_node<std::string>(lineno, s) {}

    string_node(int lineno, const std::string *s) noexcept: literal_node<std::string>(lineno, *s) {}

    void accept(basic_ast_visitor *sp, int level) { sp->do_string_node(this, level); }

  };

} // cdk
