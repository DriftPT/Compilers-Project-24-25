#pragma once

#include <cdk/ast/basic_node.h>

namespace udf {
  
  /**
   * Class for describing continue nodes.
   */
  class continue_node: public cdk::basic_node {
    int _depth;

  public:
    continue_node(int lineno, int depth = 1) :
        cdk::basic_node(lineno), _depth(depth) {
    }

    int depth() const { return _depth; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_continue_node(this, level); }

  };

} // udf