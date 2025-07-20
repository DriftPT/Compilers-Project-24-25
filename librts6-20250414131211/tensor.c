#include "tensor.h"
#include "string.h"
#include "float.h"
#include "memory.h"
#include "integer.h"
#include <stdarg.h>

static size_t get_linear_index(const Tensor *const tensor, const size_t *indices) {
    size_t flat_index = 0;
    size_t stride     = 1;

    // Iterate from the last dimension back to the first
    // to build up the final flat index.
    for (int dim = tensor->n_dims - 1; dim >= 0; dim--) {
        flat_index += indices[dim] * stride;
        stride     *= tensor->dims[dim];
    }
    return flat_index;
}

/**
 * Helper function to check if two tensors have the same shape
 * @param a pointer to first tensor
 * @param b pointer to second tensor
 * @returns true if shapes match, false otherwise
 */
static bool shapes_match(const Tensor *const a, const Tensor *const b) {
    if (a->n_dims != b->n_dims) return false;
    
    for (size_t i = 0; i < a->n_dims; i++) {
        if (a->dims[i] != b->dims[i]) return false;
    }
    
    return true;
}

/**
 * Creates a new tensor with variable number of dimensions
 * @param n_dims number of dimensions
 * @param ... dimension sizes (one integer for each dimension)
 * @returns pointer to newly allocated tensor, nullptr on failure
 */
Tensor* tensor_create(size_t n_dims, ...) {
    Tensor *tensor = (Tensor*)mem_alloc(sizeof(Tensor));
    if (!tensor) return nullptr;
    
    tensor->n_dims = n_dims;
    tensor->dims = (size_t*)mem_alloc(n_dims * sizeof(size_t));
    
    if (!tensor->dims) {
        mem_free(tensor);
        return nullptr;
    }
    
    va_list args;
    va_start(args, n_dims);
    
    size_t total_size = 1;
    for (size_t i = 0; i < n_dims; i++) {
        size_t dim_size = va_arg(args, size_t);
        if (dim_size <= 0) {
            va_end(args);
            mem_free(tensor->dims);
            mem_free(tensor);
            return nullptr;
        }
        tensor->dims[i] = dim_size;
        total_size *= dim_size;
    }
    
    va_end(args);
    
    tensor->size = total_size;
    tensor->data = (double*)mem_alloc(total_size * sizeof(double));
    if (!tensor->data) {
        mem_free(tensor->dims);
        mem_free(tensor);
        return nullptr;
    }
    
    return tensor;
}

/**
 * Frees memory allocated for a tensor
 * @param tensor pointer to tensor to be deallocated
 */
void tensor_free(Tensor *tensor) {
    if (!tensor) return;
    if (tensor->dims) mem_free(tensor->dims);
    if (tensor->data) mem_free(tensor->data);
    mem_free(tensor);
}

/**
 * Gets the total number of elements in a tensor
 * @param tensor pointer to tensor
 * @returns number of elements
 */
size_t tensor_size(const Tensor *const tensor) {
    if (!tensor) return 0;
    return tensor->size;
}

/**
 * Create tensor of given dims and fill every element with `value`.
 * @param n_dims  number of dimensions
 * @param args    va_list of dimension sizes (one size_t per dim)
 * @param value   constant to fill into every element
 * @returns newly allocated tensor, or nullptr on failure
 */
static Tensor* tensor_fill_va(size_t n_dims, double value, va_list args) {
    Tensor *t = (Tensor*)mem_alloc(sizeof(Tensor));
    if (!t) return nullptr;
    t->n_dims = n_dims;

    t->dims = (size_t*)mem_alloc(n_dims * sizeof(size_t));
    if (!t->dims) { mem_free(t); return nullptr; }

    size_t total = 1;
    for (size_t i = 0; i < n_dims; ++i) {
        size_t d = va_arg(args, size_t);
        if (d == 0) {
            mem_free(t->dims);
            mem_free(t);
            return nullptr;
        }
        t->dims[i] = d;
        total     *= d;
    }
    t->size = total;

    t->data = (double*)mem_alloc(total * sizeof(double));
    if (!t->data) {
        mem_free(t->dims);
        mem_free(t);
        return nullptr;
    }

    for (size_t i = 0; i < total; ++i)
        t->data[i] = value;

    return t;
}

/**
 * Create a new tensor of all zeros.
 */
Tensor* tensor_zeros(size_t n_dims, ...) {
    va_list args;
    va_start(args, n_dims);
    Tensor *t = tensor_fill_va(n_dims, 0.0, args);
    va_end(args);
    return t;
}

/**
 * Create a new tensor of all ones.
 */
Tensor* tensor_ones(size_t n_dims, ...) {
    va_list args;
    va_start(args, n_dims);
    Tensor *t = tensor_fill_va(n_dims, 1.0, args);
    va_end(args);
    return t;
}

/**
 * Create a new tensor filled with an arbitrary value.
 */
Tensor* tensor_full(size_t n_dims, double value, ...) {
    va_list args;
    va_start(args, value);
    Tensor *t = tensor_fill_va(n_dims, value, args);
    va_end(args);
    return t;
}

/**
 * Fills a tensor with a specific value
 * @param tensor pointer to tensor to be filled
 * @param value the value to fill the tensor with
 */
void tensor_fill(Tensor *tensor, double value) {
    if (!tensor) return;
    for (size_t i = 0; i < tensor->size; ++i)
        tensor->data[i] = value;
}

/**
 * Gets an element position from a tensor at the specified indices
 * @param tensor pointer to tensor
 * @param args indices: one for each dimension
 * @returns position of value at the specified indices, nullptr if invalid
 */
static double *_tensor_getptr_va(const Tensor *tensor, va_list args) {
    if (!tensor) return nullptr;

    size_t index = 0;
    size_t stride = 1;

    for (int d = tensor->n_dims - 1; d >= 0; d--) {
        size_t idx = va_arg(args, size_t);
        index += idx * stride;
        stride *= tensor->dims[d];
    }

    return &tensor->data[index];
}

/**
 * Gets an element position from a tensor at the specified indices
 * @param tensor pointer to tensor
 * @param ... indices: one for each dimension
 * @returns position of value at the specified indices, nullptr if invalid
 */
double *tensor_getptr(const Tensor *tensor, ...) {
    if (!tensor) return nullptr;
    va_list args;
    va_start(args, tensor);
    double *ptr = _tensor_getptr_va(tensor, args);
    va_end(args);
    return ptr;
}

/**
 * Gets an element from a tensor at the specified indices
 * @param tensor pointer to tensor
 * @param ... array of indices, one for each dimension
 * @returns value at the specified indices, 0.0 if invalid
 */
double tensor_get(const Tensor *const tensor, ...) {
    if (!tensor) return 0.0;
    va_list args;
    va_start(args, tensor);
    double *ptr = _tensor_getptr_va(tensor, args);
    va_end(args);
    if (!ptr) return 0.0;
    return *ptr;
}

/**
 * Sets an element in a tensor at the specified indices
 * @param tensor pointer to tensor
 * @param value value to set at the specified indices
 * @param ... array of indices, one for each dimension
 */
void tensor_set(Tensor *tensor, double value, ...) {
    if (!tensor) return;
    va_list args;
    va_start(args, value);
    double *ptr = _tensor_getptr_va(tensor, args);
    va_end(args);
    if (!ptr) return;
    *ptr = value;
}

/**
 * Sets an element in a tensor at the specified indices.
 * This is like tensor_set, but more convenient in addressing terms.
 * @param value value to put at the specified indices
 * @param linear_index linear index for the memory region
 * @param tensor pointer to tensor
 */
void tensor_put(double value, size_t linear_index, Tensor *tensor) {
    if (!tensor) return;
    tensor->data[linear_index] = value;
}

/**
 * Generic element-wise binary operation on two same‑shaped tensors.
 * @param a        first tensor
 * @param b        second tensor
 * @param combine  function that combines two doubles
 * @returns new tensor a⊙b element‑wise, or nullptr on shape mismatch or alloc failure
 */
static Tensor* tensor_elementwise_binary(
    const Tensor *const a,
    const Tensor *const b,
    double (*combine)(double, double)
) {
    if (!a || !b || !shapes_match(a, b)) return nullptr;

    // allocate and copy shape info
    Tensor *result = mem_alloc(sizeof(Tensor));
    if (!result) return nullptr;
    result->n_dims = a->n_dims;
    result->dims   = mem_alloc(a->n_dims * sizeof(size_t));
    if (!result->dims) { mem_free(result); return nullptr; }

    size_t total_size = 1;
    for (size_t i = 0; i < a->n_dims; ++i) {
        result->dims[i] = a->dims[i];
        total_size     *= a->dims[i];
    }
    result->size = total_size;

    // allocate data
    result->data = mem_alloc(total_size * sizeof(double));
    if (!result->data) {
        mem_free(result->dims);
        mem_free(result);
        return nullptr;
    }

    // perform the combine
    for (size_t i = 0; i < total_size; ++i) {
        result->data[i] = combine(a->data[i], b->data[i]);
    }
    return result;
}

static double add_op(double x, double y) { return x + y; }
static double sub_op(double x, double y) { return x - y; }
static double mul_op(double x, double y) { return x * y; }
static double div_op(double x, double y) { return x / y; }

Tensor* tensor_add(const Tensor *const a, const Tensor *const b) {
    return tensor_elementwise_binary(a, b, add_op);
}

Tensor* tensor_sub(const Tensor *const a, const Tensor *const b) {
    return tensor_elementwise_binary(a, b, sub_op);
}

Tensor* tensor_mul(const Tensor *const a, const Tensor *const b) {
    return tensor_elementwise_binary(a, b, mul_op);
}

Tensor* tensor_div(const Tensor *const a, const Tensor *const b) {
    return tensor_elementwise_binary(a, b, div_op);
}

/**
 * Generic element‑wise tensor ∘ scalar operation.
 * @param tensor  input tensor
 * @param scalar  the scalar value
 * @param combine function that combines (element, scalar)
 * @returns new tensor with combine(tensor[i], scalar), or nullptr on alloc failure
 */
static Tensor* tensor_elementwise_scalar(
    const Tensor *tensor,
    double scalar,
    double (*combine)(double, double)
) {
    if (!tensor) return nullptr;

    Tensor *result = mem_alloc(sizeof(Tensor));
    if (!result) return nullptr;
    result->n_dims = tensor->n_dims;
    result->dims   = mem_alloc(tensor->n_dims * sizeof(size_t));
    if (!result->dims) { mem_free(result); return nullptr; }

    size_t total_size = 1;
    for (size_t d = 0; d < tensor->n_dims; ++d) {
        result->dims[d] = tensor->dims[d];
        total_size     *= tensor->dims[d];
    }
    result->size = total_size;

    result->data = mem_alloc(total_size * sizeof(double));
    if (!result->data) {
        mem_free(result->dims);
        mem_free(result);
        return nullptr;
    }

    for (size_t i = 0; i < total_size; ++i) {
        result->data[i] = combine(tensor->data[i], scalar);
    }

    return result;
}

static double add_scalar_op(double x, double y) { return x + y; }
static double sub_scalar_op(double x, double y) { return x - y; }
static double mul_scalar_op(double x, double y) { return x * y; }
static double div_scalar_op(double x, double y) { return x / y; }

Tensor* tensor_add_scalar(const Tensor *const tensor, double scalar) {
    return tensor_elementwise_scalar(tensor, scalar, add_scalar_op);
}

Tensor* tensor_sub_scalar(const Tensor *const tensor, double scalar) {
    return tensor_elementwise_scalar(tensor, scalar, sub_scalar_op);
}

Tensor* tensor_mul_scalar(const Tensor *const tensor, double scalar) {
    return tensor_elementwise_scalar(tensor, scalar, mul_scalar_op);
}

Tensor* tensor_div_scalar(const Tensor *const tensor, double scalar) {
    return tensor_elementwise_scalar(tensor, scalar, div_scalar_op);
}

/**
 * Performs tensor multiplication (contraction) between two tensors
 * Contracts the last dimension of the first tensor with the first dimension of the second tensor
 * @param a pointer to first tensor
 * @param b pointer to second tensor
 * @returns pointer to newly allocated result tensor, nullptr on failure
 */
Tensor* tensor_matmul(const Tensor *const a, const Tensor *const b) {
    if (!a || !b) return nullptr;
    if (a->n_dims < 1 || b->n_dims < 1) return nullptr;

    // contraction dimension must match
    size_t K = a->dims[a->n_dims - 1];
    if (K != b->dims[0]) return nullptr;

    // --- build result shape: [ a.dims[0..n-2], b.dims[1..m-1] ]
    size_t Rn = a->n_dims + b->n_dims - 2;
    size_t *Rd = (size_t*)mem_alloc(Rn * sizeof(size_t));
    if (!Rd) return nullptr;
    for (size_t i = 0; i < a->n_dims - 1; i++)           // copy A’s leading dims
        Rd[i] = a->dims[i];
    for (size_t j = 1; j < b->n_dims; j++)               // copy B’s trailing dims
        Rd[a->n_dims - 1 + (j - 1)] = b->dims[j];

    // total # of elements in result
    size_t Rsize = 1;
    for (size_t i = 0; i < Rn; i++) Rsize *= Rd[i];

    // allocate result tensor
    Tensor *r = (Tensor*)mem_alloc(sizeof(Tensor));
    if (!r) { mem_free(Rd); return nullptr; }
    r->n_dims = Rn;
    r->dims   = Rd;
    r->size   = Rsize;
    r->data   = (double*)mem_alloc(Rsize * sizeof(double));
    if (!r->data) {
        mem_free(Rd);
        mem_free(r);
        return nullptr;
    }
    // zero‑init
    for (size_t i = 0; i < Rsize; i++) r->data[i] = 0.0;

    // --- precompute strides for A, B, and result
    size_t *sA = (size_t*)mem_alloc(a->n_dims * sizeof(size_t));
    size_t *sB = (size_t*)mem_alloc(b->n_dims * sizeof(size_t));
    size_t *sR = (size_t*)mem_alloc(Rn        * sizeof(size_t));
    if (!sA || !sB || !sR) {
        mem_free(r->data);
        mem_free(r->dims);
        mem_free(r);
        if (sA) mem_free(sA);
        if (sB) mem_free(sB);
        if (sR) mem_free(sR);
        return nullptr;
    }
    // last stride = 1, then backwards
    sA[a->n_dims - 1] = 1;
    for (int i = (int)a->n_dims - 2; i >= 0; i--)
        sA[i] = sA[i+1] * a->dims[i+1];
    sB[b->n_dims - 1] = 1;
    for (int j = (int)b->n_dims - 2; j >= 0; j--)
        sB[j] = sB[j+1] * b->dims[j+1];
    sR[Rn - 1] = 1;
    for (int i = (int)Rn - 2; i >= 0; i--)
        sR[i] = sR[i+1] * Rd[i+1];

    // index buffers
    size_t *idxR = (size_t*)mem_alloc(Rn               * sizeof(size_t));
    size_t *idxA = (size_t*)mem_alloc(a->n_dims        * sizeof(size_t));
    size_t *idxB = (size_t*)mem_alloc(b->n_dims        * sizeof(size_t));
    if (!idxR || !idxA || !idxB) {
        // cleanup all
        mem_free(r->data); mem_free(r->dims); mem_free(r);
        mem_free(sA); mem_free(sB); mem_free(sR);
        if (idxR) mem_free(idxR);
        if (idxA) mem_free(idxA);
        if (idxB) mem_free(idxB);
        return nullptr;
    }

    // --- main flat loop over result
    for (size_t flat = 0; flat < Rsize; flat++) {
        // decode flat -> idxR[0..Rn-1]
        size_t tmp = flat;
        for (size_t d = 0; d < Rn; d++) {
            idxR[d] = tmp / sR[d];
            tmp    %= sR[d];
        }
        // map into A and B indices
        // A: first n_dims-1 positions come from idxR[0..]
        for (size_t i = 0; i < a->n_dims - 1; i++)
            idxA[i] = idxR[i];
        // B: last b_dims-1 positions come from idxR[a_n-1 ..]
        for (size_t j = 1; j < b->n_dims; j++)
            idxB[j] = idxR[a->n_dims - 1 + (j - 1)];

        // contract over K
        double sum = 0.0;
        for (size_t k = 0; k < K; k++) {
            idxA[a->n_dims - 1] = k;   // last dim of A
            idxB[0]             = k;   // first dim of B

            // compute offsets via strides
            size_t offA = 0, offB = 0;
            for (size_t x = 0; x < a->n_dims; x++)
                offA += idxA[x] * sA[x];
            for (size_t y = 0; y < b->n_dims; y++)
                offB += idxB[y] * sB[y];

            sum += a->data[offA] * b->data[offB];
        }

        r->data[flat] = sum;
    }

    // free scratch arrays
    mem_free(sA); mem_free(sB); mem_free(sR);
    mem_free(idxR); mem_free(idxA); mem_free(idxB);

    return r;
}

/**
 * Reshapes a tensor to new dimensions with the same total number of elements
 * @param tensor pointer to tensor to reshape
 * @param new_n_dims number of new dimensions
 * @param ... new dimension sizes (one integer for each dimension)
 * @returns pointer to newly allocated reshaped tensor, nullptr on failure
 */
Tensor* tensor_reshape(const Tensor *const tensor, size_t new_n_dims, ...) {
    if (!tensor) return nullptr;
    
    Tensor *new_tensor = (Tensor*)mem_alloc(sizeof(Tensor));
    if (!new_tensor) return nullptr;
    
    new_tensor->n_dims = new_n_dims;
    new_tensor->dims = (size_t*)mem_alloc(new_n_dims * sizeof(size_t));
    if (!new_tensor->dims) {
        mem_free(new_tensor);
        return nullptr;
    }
    
    va_list args;
    va_start(args, new_n_dims);
    
    size_t new_total_size = 1;
    for (size_t i = 0; i < new_n_dims; i++) {
        int dim_size = va_arg(args, int);
        if (dim_size <= 0) {
            va_end(args);
            mem_free(new_tensor->dims);
            mem_free(new_tensor);
            return nullptr;
        }
        new_tensor->dims[i] = dim_size;
        new_total_size *= dim_size;
    }
    
    va_end(args);
    
    if (new_total_size != tensor->size) {
        mem_free(new_tensor->dims);
        mem_free(new_tensor);
        return nullptr;
    }
    
    new_tensor->size = new_total_size;
    new_tensor->data = (double*)mem_alloc(new_total_size * sizeof(double));
    if (!new_tensor->data) {
        mem_free(new_tensor->dims);
        mem_free(new_tensor);
        return nullptr;
    }
    
    for (size_t i = 0; i < tensor->size; i++)
        new_tensor->data[i] = tensor->data[i];
    
    return new_tensor;
}

/**
 * Gets the number of dimensions in a tensor
 * @param tensor pointer to tensor
 * @returns number of dimensions, 0 if invalid
 */
size_t tensor_get_n_dims(const Tensor *const tensor) {
    if (!tensor) return 0;
    return tensor->n_dims;
}

/**
 * Gets the dimensions array of a tensor
 * @param tensor pointer to tensor
 * @returns pointer to dimensions array
 */
const size_t *tensor_get_dims(const Tensor *const tensor) {
    if (!tensor) return nullptr;
    return tensor->dims;
}

/**
 * Gets the size of a specific dimension
 * @param tensor pointer to tensor
 * @param dim dimension index
 * @returns size of the specified dimension, -1 if invalid
 */
size_t tensor_get_dim_size(const Tensor *tensor, size_t dim) {
    if (!tensor || dim >= tensor->n_dims) return -1;
    return tensor->dims[dim];
}

static void print_brackets(const Tensor *const tensor, size_t *indices, size_t current_dim) {
    if (current_dim == tensor->n_dims) {
        // We’ve arrived at a single element; print it.
        double value = tensor->data[get_linear_index(tensor, indices)];
        printd(value);
        return;
    }

    prints("[");

    for (size_t i = 0; i < tensor->dims[current_dim]; i++) {
        indices[current_dim] = i;
        if (i > 0) prints(", ");
        print_brackets(tensor, indices, current_dim + 1);
    }

    prints("]");
}

// A simpler public-facing print function that shows the shape and
// then prints bracketed elements with the above helper.
void tensor_print(const Tensor *const tensor) {
    if (!tensor) {
        prints("null");
        return;
    }

    prints("Tensor<");
    for (size_t i = 0; i < tensor->n_dims; i++) {
        printi(tensor->dims[i]);
        if (i < tensor->n_dims - 1) prints(",");
    }
    prints(">");

    size_t indices[tensor->n_dims];
    for (size_t i = 0; i < tensor->n_dims; i++) {
        indices[i] = 0;
    }

    print_brackets(tensor, indices, 0);
}

/**
 * Compares two tensors element‑wise for exact equality.
 * Returns false (0) as soon as any pair of elements differs, or shapes don’t match.
 */
bool tensor_equals(const Tensor *const a, const Tensor *const b) {
    if (!a || !b) return false;
    if (!shapes_match(a, b)) return false;
    size_t total = a->size;
    for (size_t i = 0; i < total; ++i) {
        if (a->data[i] != b->data[i])
            return false;
    }
    return true;
}

/**
 * Returns a new tensor with dimensions dim1 and dim2 swapped.
 * @param tensor  pointer to the input tensor
 * @param dim1    first dimension to swap
 * @param dim2    second dimension to swap
 * @returns newly allocated tensor with axes swapped, or nullptr on failure
 */
Tensor* tensor_transpose(const Tensor *const tensor, size_t dim1, size_t dim2) {
    if (!tensor) return nullptr;
    size_t n = tensor->n_dims;
    if (dim1 >= n || dim2 >= n)      // invalid axes
        return nullptr;

    // allocate result tensor struct
    Tensor *res = (Tensor*)mem_alloc(sizeof(Tensor));
    if (!res) return nullptr;
    res->n_dims = n;

    // copy dims and swap the two
    res->dims = (size_t*)mem_alloc(n * sizeof(size_t));
    if (!res->dims) { mem_free(res); return nullptr; }
    for (size_t i = 0; i < n; i++)
        res->dims[i] = tensor->dims[i];
    // swap in the shape array
    {
        size_t tmp = res->dims[dim1];
        res->dims[dim1] = res->dims[dim2];
        res->dims[dim2] = tmp;
    }

    // total size is unchanged
    res->size = tensor->size;
    res->data = (double*)mem_alloc(res->size * sizeof(double));
    if (!res->data) {
        mem_free(res->dims);
        mem_free(res);
        return nullptr;
    }

    // build stride arrays for input and output
    size_t *in_strides  = (size_t*)mem_alloc(n * sizeof(size_t));
    size_t *out_strides = (size_t*)mem_alloc(n * sizeof(size_t));
    if (!in_strides || !out_strides) {
        if (in_strides)  mem_free(in_strides);
        if (out_strides) mem_free(out_strides);
        mem_free(res->data);
        mem_free(res->dims);
        mem_free(res);
        return nullptr;
    }
    // input strides: last dim stride = 1
    in_strides[n-1] = 1;
    for (int i = (int)n-2; i >= 0; --i)
        in_strides[i] = in_strides[i+1] * tensor->dims[i+1];
    // output strides on the swapped shape
    out_strides[n-1] = 1;
    for (int i = (int)n-2; i >= 0; --i)
        out_strides[i] = out_strides[i+1] * res->dims[i+1];

    // temp index buffers
    size_t *idx = (size_t*)mem_alloc(n * sizeof(size_t));
    size_t *in_idx = (size_t*)mem_alloc(n * sizeof(size_t));
    if (!idx || !in_idx) {
        mem_free(in_strides);
        mem_free(out_strides);
        if (idx)    mem_free(idx);
        if (in_idx) mem_free(in_idx);
        mem_free(res->data);
        mem_free(res->dims);
        mem_free(res);
        return nullptr;
    }

    // perform the data copy by flat‐index → multi‐index mapping
    for (size_t out_flat = 0; out_flat < res->size; ++out_flat) {
        // decode output flat index into idx[0..n-1]
        size_t tmp = out_flat;
        for (size_t d = 0; d < n; ++d) {
            idx[d] = tmp / out_strides[d];
            tmp   %= out_strides[d];
        }
        // map to input indices by swapping back the two axes
        for (size_t d = 0; d < n; ++d)
            in_idx[d] = idx[d];
        {
            size_t t = in_idx[dim1];
            in_idx[dim1] = in_idx[dim2];
            in_idx[dim2] = t;
        }
        // compute input flat index
        size_t in_flat = 0;
        for (size_t d = 0; d < n; ++d)
            in_flat += in_idx[d] * in_strides[d];
        // copy element
        res->data[out_flat] = tensor->data[in_flat];
    }

    // clean up
    mem_free(in_strides);
    mem_free(out_strides);
    mem_free(idx);
    mem_free(in_idx);

    return res;
}

