#pragma once

#include <cdk/ast/basic_node.h>

namespace cdk {

  /**
   * Class for describing empty nodes (leaves).
   * The only data recorded by this type of node is the node's
   * attribute (i.e., the mnemonic) and the source code line
   * number.
   */
  class nil_node : public basic_node {
  public:
    nil_node(int lineno) noexcept: basic_node(lineno) {}

    void accept(basic_ast_visitor *sp, int level) { sp->do_nil_node(this, level); }

  };

} // cdk
