#pragma once

#include <cstdint>

namespace cdk {

  enum typename_type : uint64_t {
    TYPE_UNSPEC = 0,

    TYPE_INT = 1UL << 0,
    TYPE_DOUBLE = 1UL << 1,
    TYPE_BOOLEAN = 1UL << 2,
    TYPE_STRING = 1UL << 3,
    TYPE_POINTER = 1UL << 4,
    TYPE_STRUCT = 1UL << 5,
    TYPE_FUNCTIONAL = 1UL << 6,
    TYPE_TENSOR = 1UL << 7,
    TYPE_VOID = 1UL << 30,

    TYPE_ERROR = 1UL << 31,
  };

} // cdk
