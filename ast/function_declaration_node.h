#pragma once

#include <string>
#include <cdk/ast/typed_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {

  /**
   * Class for describing function declaration nodes.
   * 
   * - declaration: type qualifier id '(' args ')'
   */
  class function_declaration_node: public cdk::typed_node {
    int _qualifier;
    std::string _identifier;
    cdk::sequence_node *_arguments;

  public:
    function_declaration_node(int lineno, int qualifier, const std::string &identifier, cdk::sequence_node *arguments) :
        cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _arguments(arguments) {
      type(cdk::primitive_type::create(0, cdk::TYPE_VOID));
    }

    function_declaration_node(int lineno, int qualifier, const std::string &identifier, cdk::sequence_node *arguments,
                              std::shared_ptr<cdk::basic_type> return_type) :
        cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _arguments(arguments) {
      type(return_type);
    }

    int qualifier() { return _qualifier; }

    const std::string &identifier() const { return _identifier; }

    cdk::typed_node *argument(size_t ax) {
      return dynamic_cast<cdk::typed_node*>(_arguments->node(ax));
    }

    cdk::sequence_node *arguments() { return _arguments; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_function_declaration_node(this, level); }

  };

} //  udf