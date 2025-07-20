#pragma once

#include <cdk/ast/basic_node.h>

namespace cdk {

  /**
   * Class for describing syntactic tree leaves for holding data
   * buffers. This class does not inherit from the <tt>Simple</tt>
   * template.
   */
  class data_node : public basic_node {
    void *_bucket; // pointer to data
    size_t _size; // size of data

  public:
    /**
     * Constructor for nodes that hold opaque data buffers.
     * Each buffer is characterized by its data and the
     * corresponding data size.
     *
     * @param lineno the source code line number corresponding to this node
     * @param data the opaque data buffer
     * @param nbytes the size (bytes) of the data buffer
     */
    data_node(int lineno, void *data, size_t nbytes) noexcept:
        basic_node(lineno), _bucket(data), _size(nbytes) {
    }

    /**
     * The destructor. We have defined it explicitly (even though
     * it was not needed) to emphasize that the data buffer is
     * <b>not</b> destroyed when the node itself dies.
     */
    ~data_node() noexcept = default;

    void *bucket() noexcept { return _bucket; }

    size_t size() noexcept { return _size; }

    void accept(basic_ast_visitor *sp, int level) override { sp->do_data_node(this, level); }

  };

} // cdk
