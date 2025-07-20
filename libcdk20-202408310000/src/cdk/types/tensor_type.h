#pragma once

#include <vector>
#include <numeric>
#include <cdk/types/basic_type.h>

namespace cdk {

  /**
   * This class represents a tensor type concept.
   */
  class tensor_type: public basic_type {
    std::vector<size_t> _dims;

  public:

    // DAVID size 4 is because this is actually just a pointer
    explicit tensor_type(explicit_call_disabled, const std::vector<size_t> &dims) :
        basic_type(4, TYPE_TENSOR), _dims(dims) {
      // EMPTY
    }

    ~tensor_type() = default;

  public:

    size_t dim(size_t ix) const {
      return _dims.at(ix);
    }
    const std::vector<size_t> &dims() const {
      return _dims;
    }
    size_t n_dims() const {
      return _dims.size();
    }
    size_t size() const {
      size_t size = 0;
      if (_dims.size() >= 1) size = _dims.at(0);
      for (size_t ix = 1; ix < _dims.size(); ix++)
        size *= _dims.at(ix);
      return size;
    }

  public:

    std::string to_string() const {
      std::string s = "[";
      if (_dims.size() >= 1) s += std::to_string(_dims.at(0));
      for (size_t ix = 1; ix < _dims.size(); ix++)
        s += ',' + std::to_string(_dims.at(ix));
      return s + "]";
    }

  public:

    static auto create(const std::vector<size_t> &dims) {
      return std::make_shared<tensor_type>(explicit_call_disabled(), dims);
    }

    static auto cast(std::shared_ptr<basic_type> type) {
      return std::dynamic_pointer_cast<tensor_type>(type);
    }

  };

} // cdk
