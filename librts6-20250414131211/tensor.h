#pragma once

typedef unsigned long size_t;

/**
 * Tensor data structure
 * Contains dimensions information and data storage
 */
typedef struct Tensor {
  size_t n_dims;            // Number of dimensions
  size_t *dims;             // Size of each dimension
  double *data;             // Actual data storage
  size_t size;              // Total size (number of elements)
} Tensor;

/**
 * Creates a new tensor with variable number of dimensions
 * @param n_dims number of dimensions
 * @param ... dimension sizes (one integer for each dimension)
 * @returns pointer to newly allocated tensor, 0 on failure
 */
Tensor* tensor_create(size_t n_dims, ...);

/**
 * Frees memory allocated for a tensor
 * @param tensor pointer to tensor to be deallocated
 */
void tensor_free(Tensor *tensor);

/**
 * Gets the total number of elements in a tensor
 * @param tensor pointer to tensor
 * @returns number of elements
 */
size_t tensor_size(const Tensor *const tensor);

/**
 * Creates a tensor initialized with zeros
 * @param n_dims number of dimensions
 * @param ... dimension sizes (one integer for each dimension)
 * @returns pointer to newly allocated zero-filled tensor, 0 on failure
 */
Tensor* tensor_zeros(size_t n_dims, ...);

/**
 * Creates a tensor initialized with ones
 * @param n_dims number of dimensions
 * @param ... dimension sizes (one integer for each dimension)
 * @returns pointer to newly allocated one-filled tensor, 0 on failure
 */
Tensor* tensor_ones(size_t n_dims, ...);

/**
 * Creates a tensor initialized with specific value
 * @param n_dims number of dimensions
 * @param value value to use as initializer
 * @param ... dimension sizes (one integer for each dimension)
 * @returns pointer to newly allocated one-filled tensor, 0 on failure
 */
Tensor* tensor_full(size_t n_dims, double value, ...);

/**
 * In-place fills a tensor with a specific value
 * @param tensor pointer to tensor to be filled
 * @param value the value to fill the tensor with
 */
void tensor_fill(Tensor *tensor, double value);

/**
 * Gets an element position from a tensor at the specified indices
 * @param tensor pointer to tensor
 * @param ... indices: one for each dimension
 * @returns value at the specified indices
 */
double *tensor_getptr(const Tensor *tensor, ...);

/**
 * Gets an element from a tensor at the specified indices
 * @param tensor pointer to tensor
 * @param ... indices: one for each dimension
 * @returns value at the specified indices
 */
double tensor_get(const Tensor *const tensor, ...);

/**
 * Sets an element in a tensor at the specified indices
 * @param tensor pointer to tensor
 * @param value value to set at the specified indices
 * @param ... indices: one for each dimension
 */
void tensor_set(Tensor *tensor, double value, ...);

/**
 * Sets an element in a tensor at the specified indices.
 * This is like tensor_set, but more convenient in addressing terms.
 * @param value value to put at the specified indices
 * @param index linear index for the memory region
 * @param tensor pointer to tensor
 */
void tensor_put(double value, size_t offset, Tensor *tensor);

/**
 * Adds two tensors element-wise: result in new tensor
 * @param a pointer to first tensor
 * @param b pointer to second tensor
 * @returns pointer to new tensor, nullptr if shapes don't match or on failure
 */
Tensor* tensor_add(const Tensor *const a, const Tensor *const b);

/**
 * Subtracts two tensors element-wise: result in new tensor
 * @param a pointer to first tensor
 * @param b pointer to second tensor
 * @returns pointer to new tensor, nullptr if shapes don't match or on failure
 */
Tensor* tensor_sub(const Tensor *const a, const Tensor *const b);

/**
 * Multiplies two tensors element-wise: result in new tensor
 * @param a pointer to first tensor
 * @param b pointer to second tensor
 * @returns pointer to new tensor, nullptr if shapes don't match or on failure
 */
Tensor* tensor_mul(const Tensor *const a, const Tensor *const b);

/**
 * Divide two tensors element-wise: result in new tensor
 * @param a pointer to first tensor
 * @param b pointer to second tensor
 * @returns pointer to new tensor, nullptr if shapes don't match or on failure
 */
Tensor* tensor_div(const Tensor *const a, const Tensor *const b);

/**
 * Adds scalar to each tensor tensor element: result in new tensor
 * @param tensor pointer to tensor
 * @param scalar value to add
 * @returns pointer to new tensor, nullptr on failure
 */
Tensor* tensor_add_scalar(const Tensor *const tensor, double scalar);

/**
 * Subtract scalar from each tensor element: result in new tensor
 * @param tensor pointer to tensor
 * @param scalar value to add
 * @returns pointer to new tensor, nullptr on failure
 */
Tensor* tensor_sub_scalar(const Tensor *const tensor, double scalar);

/**
 * Multiply scalar by each tensor element: result in new tensor
 * @param tensor pointer to tensor
 * @param scalar value to add
 * @returns pointer to new tensor, nullptr on failure
 */
Tensor* tensor_mul_scalar(const Tensor *const tensor, double scalar);

/**
 * Divide scalar by each tensor element: result in new tensor
 * @param tensor pointer to tensor
 * @param scalar value to add
 * @returns pointer to new tensor, nullptr on failure
 */
Tensor* tensor_div_scalar(const Tensor *const tensor, double scalar);

/**
 * Performs tensor multiplication (contraction) between two tensors
 * Contracts the last dimension of the first tensor with the first dimension of the second tensor
 * @param a pointer to first tensor
 * @param b pointer to second tensor
 * @returns pointer to new tensor, nullptr on failure
 */
Tensor* tensor_matmul(const Tensor *const a, const Tensor *const b);

/**
 * Reshapes a tensor to new dimensions with the same total number of elements
 * @param tensor pointer to tensor to reshape
 * @param new_n_dims number of new dimensions
 * @param ... new dimension sizes (one integer for each dimension)
 * @returns pointer to newly allocated reshaped tensor, nullptr on failure
 */
Tensor* tensor_reshape(const Tensor *const tensor, size_t new_n_dims, ...);

/**
 * Gets the number of dimensions in a tensor
 * @param tensor pointer to tensor
 * @returns number of dimensions
 */
size_t tensor_get_n_dims(const Tensor *const tensor);

/**
 * Gets the dimensions array of a tensor
 * @param tensor pointer to tensor
 * @returns pointer to dimensions array
 */
const size_t *tensor_get_dims(const Tensor *const tensor);

/**
 * Gets the size of a specific dimension
 * @param tensor pointer to tensor
 * @param dim dimension index
 * @returns size of the specified dimension, -1 if invalid
 */
size_t tensor_get_dim_size(const Tensor *const tensor, size_t dim);

/**
 * Prints a tensor to stdout
 * @param tensor pointer to tensor to be printed
 */
void tensor_print(const Tensor *const tensor);

/**
 * Compares two tensors element‑wise for exact equality.
 * @param a  pointer to the first tensor
 * @param b  pointer to the second tensor
 * @returns true (non‑zero) if they have the same shape and all elements compare equal, false (0) otherwise
 */
bool tensor_equals(const Tensor *const a, const Tensor *const b);

