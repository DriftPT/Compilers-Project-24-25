#pragma once

#include <cdk/ast/sequence_node.h>

namespace udf {

  class block_node: public cdk::basic_node {
    cdk::sequence_node *_instructions;
    cdk::sequence_node *_declarations;

  public:
    block_node(int lineno, cdk::sequence_node *declarations, cdk::sequence_node *instructions) :
        cdk::basic_node(lineno), _instructions(instructions), _declarations(declarations) {
    }

  public:
    cdk::sequence_node *instructions() { return _instructions; }
    cdk::sequence_node *declarations() { return _declarations; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_block_node(this, level); }
  };
} // udf