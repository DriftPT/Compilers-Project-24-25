#pragma once

#include <cdk/ast/basic_node.h>
#include <cdk/types/types.h>
#include <memory>

namespace cdk {

  /**
   * Typed nodes store a type description.
   */
  class typed_node : public basic_node {
  protected:
    // This must be a shared pointer, so that we can anchor a dynamic
    // object and be able to change/delete it afterward.
    std::shared_ptr <basic_type> _type;

  public:
    typed_node(int lineno) noexcept: basic_node(lineno), _type(nullptr) {}

    std::shared_ptr <basic_type> type() noexcept { return _type; }

    void type(std::shared_ptr <basic_type> type) noexcept { _type = type; }

    bool is_typed(typename_type name) const { return _type->name() == name; }

  };

} // cdk
