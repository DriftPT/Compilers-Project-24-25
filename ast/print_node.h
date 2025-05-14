#pragma once

#include <cdk/ast/basic_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {

  /**
   * Class for describing print nodes.
   */
  class print_node : public cdk::basic_node {
    cdk::sequence_node *_arguments;
    bool _newline;

  public:
    print_node(int lineno, cdk::sequence_node *arguments, bool newline) :
        cdk::basic_node(lineno), _arguments(arguments), _newline(newline) {
    }

    cdk::sequence_node *arguments() { return _arguments; }

    bool newline() const { return _newline; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_print_node(this, level); }

  };

} // udf
