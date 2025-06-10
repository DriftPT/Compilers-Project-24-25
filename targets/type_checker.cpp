#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>
#include <cdk/types/types.h>
#include "udf_parser.tab.h"

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

void udf::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void udf::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void udf::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (node->argument()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong type in unary logical expression");
  }
}

//---------------------------------------------------------------------------

void udf::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++)
    node->node(i)->accept(this, lvl);
}

void udf::type_checker::do_block_node(udf::block_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void udf::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}
void udf::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}
void udf::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}
void udf::type_checker::do_nullptr_node(udf::nullptr_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4,  nullptr));
}


//---------------------------------------------------------------------------

void udf::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in argument of unary expression");
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void udf::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

void udf::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void udf::type_checker::processBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in right argument of binary expression");

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void udf::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void udf::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void udf::type_checker::processEqualityExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if ((node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_DOUBLE)) &&
      (node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE))) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    return;
  }

  if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    return;
  }

  if (node->left()->is_typed(cdk::TYPE_TENSOR) && node->right()->is_typed(cdk::TYPE_TENSOR)) {
    auto ltype = cdk::tensor_type::cast(node->left()->type());
    auto rtype = cdk::tensor_type::cast(node->right()->type());
    if (!ltype || !rtype)
      throw std::string("invalid tensor type in equality expression");
    if (ltype->dims() != rtype->dims())
      throw std::string("tensor shapes must match for coefficient-wise equality");
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    return;
  }

  throw std::string("wrong type in argument of equality expression");
}

void udf::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processEqualityExpression(node, lvl);
}
void udf::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processEqualityExpression(node, lvl);
}

//---------------------------------------------------------------------------

void udf::type_checker::processAdditiveExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if ((node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_DOUBLE)) &&
      (node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE))) {
    if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE))
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    else
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    return;
  }

  if (dynamic_cast<cdk::add_node*>(node)) {
    if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
      node->type(node->left()->type());
      return;
    }
    if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_POINTER)) {
      node->type(node->right()->type());
      return;
    }
  }

  if (dynamic_cast<cdk::sub_node*>(node)) {
    if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
      node->type(node->left()->type());
      return;
    }

    if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) {
      auto lref = cdk::reference_type::cast(node->left()->type());
      auto rref = cdk::reference_type::cast(node->right()->type());
      if (lref && rref && lref->referenced() && rref->referenced() &&
          lref->referenced()->name() == rref->referenced()->name()) {
        node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        return;
      }
    }
  }

  if (node->left()->is_typed(cdk::TYPE_TENSOR) && node->right()->is_typed(cdk::TYPE_TENSOR)) {
    auto ltype = cdk::tensor_type::cast(node->left()->type());
    auto rtype = cdk::tensor_type::cast(node->right()->type());
    if (!ltype || !rtype)
      throw std::string("invalid tensor type in additive expression");
    if (ltype->dims() != rtype->dims())
      throw std::string("tensor shapes must match for coefficient-wise operation");
    node->type(ltype);
    return;
  }

  if (node->left()->is_typed(cdk::TYPE_TENSOR) &&
      (node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE))) {
    node->type(node->left()->type());
    return;
  }

  if ((node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_DOUBLE)) &&
      node->right()->is_typed(cdk::TYPE_TENSOR)) {
    node->type(node->right()->type());
    return;
  }

  throw std::string("wrong type in argument of additive expression");
}

void udf::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processAdditiveExpression(node, lvl);
}
void udf::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processAdditiveExpression(node, lvl);
}

//---------------------------------------------------------------------------

void udf::type_checker::processMultiplicativeExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (dynamic_cast<cdk::mod_node*>(node)) {
    if (!node->left()->is_typed(cdk::TYPE_INT) || !node->right()->is_typed(cdk::TYPE_INT))
      throw std::string("wrong type in argument of mod expression");
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    return;
  }

  if ((node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_DOUBLE)) &&
      (node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE))) {
    if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE))
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    else
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    return;
  }

  if ((node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_DOUBLE)) &&
      (node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE))) {
    if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE))
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    else
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    return;
  }

  if (node->left()->is_typed(cdk::TYPE_TENSOR) && node->right()->is_typed(cdk::TYPE_TENSOR)) {
    auto ltype = cdk::tensor_type::cast(node->left()->type());
    auto rtype = cdk::tensor_type::cast(node->right()->type());
    if (!ltype || !rtype)
      throw std::string("invalid tensor type in multiplicative expression");
    if (ltype->dims() != rtype->dims())
      throw std::string("tensor shapes must match for coefficient-wise operation");
    node->type(ltype);
    return;
  }

  if (node->left()->is_typed(cdk::TYPE_TENSOR) &&
      (node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE))) {
    node->type(node->left()->type());
    return;
  }

  if ((node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_DOUBLE)) &&
      node->right()->is_typed(cdk::TYPE_TENSOR)) {
    node->type(node->right()->type());
    return;
  }

  throw std::string("wrong type in argument of multiplicative expression");
}

void udf::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processMultiplicativeExpression(node, lvl);
}
void udf::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processMultiplicativeExpression(node, lvl);
}
void udf::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processMultiplicativeExpression(node, lvl);
}

//---------------------------------------------------------------------------

void udf::type_checker::processComparativeExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if ((node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_DOUBLE)) &&
      (node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE))) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    return;
  }
  throw std::string("wrong type in argument of comparative expression");
}

void udf::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processComparativeExpression(node, lvl);
}
void udf::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processComparativeExpression(node, lvl);
}
void udf::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processComparativeExpression(node, lvl);
}
void udf::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processComparativeExpression(node, lvl);
}

//---------------------------------------------------------------------------

void udf::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<udf::symbol> symbol = _symtab.find(id);
  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw std::string("undeclared variable " + id);
  }
}

void udf::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl);
  node->type(node->lvalue()->type());
}

void udf::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 4);
  node->rvalue()->accept(this, lvl + 4);

  auto ltype = node->lvalue()->type();
  auto rtype = node->rvalue()->type();

  if (!ltype || !rtype)
    throw std::string("cannot determine type in assignment");

  // INT to INT
  if (ltype->name() == cdk::TYPE_INT) {
    if (rtype->name() == cdk::TYPE_INT) {
      node->type(ltype);
    } else if (rtype->name() == cdk::TYPE_UNSPEC) {
      node->type(ltype);
      node->rvalue()->type(ltype);
    } else {
      throw std::string("wrong assignment to integer");
    }
    return;
  }

  // DOUBLE to DOUBLE or INT
  if (ltype->name() == cdk::TYPE_DOUBLE) {
    if (rtype->name() == cdk::TYPE_DOUBLE || rtype->name() == cdk::TYPE_INT) {
      node->type(ltype);
    } else if (rtype->name() == cdk::TYPE_UNSPEC) {
      node->type(ltype);
      node->rvalue()->type(ltype);
    } else {
      throw std::string("wrong assignment to real");
    }
    return;
  }

  // STRING to STRING
  if (ltype->name() == cdk::TYPE_STRING) {
    if (rtype->name() == cdk::TYPE_STRING) {
      node->type(ltype);
    } else if (rtype->name() == cdk::TYPE_UNSPEC) {
      node->type(ltype);
      node->rvalue()->type(ltype);
    } else {
      throw std::string("wrong assignment to string");
    }
    return;
  }

  // PTR<T> to PTR<T> or PTR<auto> (or nullptr)
  if (ltype->name() == cdk::TYPE_POINTER) {
    if (rtype->name() == cdk::TYPE_POINTER) {
      auto lref = std::static_pointer_cast<cdk::reference_type>(ltype);
      auto rref = std::static_pointer_cast<cdk::reference_type>(rtype);
      //TODO: Rever pois sera que PTR<double> = PTR<int> ?
      if (!lref->referenced() || !rref->referenced()) {
        node->type(ltype);
      } else if (lref->referenced()->name() == rref->referenced()->name()) {
        node->type(ltype);
      } else {
        throw std::string("incompatible pointer types in assignment");
      }
    } else if (rtype->name() == cdk::TYPE_UNSPEC) {
      node->type(ltype);
      node->rvalue()->type(ltype);
    } else {
      throw std::string("wrong assignment to pointer");
    }
    return;
  }

  // TENSOR to TENSOR (mesmo shape)
  if (ltype->name() == cdk::TYPE_TENSOR) {
    if (rtype->name() == cdk::TYPE_TENSOR) {
      node->type(ltype);
    } else if (rtype->name() == cdk::TYPE_UNSPEC) {
      node->type(ltype);
      node->rvalue()->type(ltype);
    } else {
      throw std::string("wrong assignment to tensor");
    }
    return;
  }

  throw std::string("wrong types in assignment");
}

//---------------------------------------------------------------------------

void udf::type_checker::do_evaluation_node(udf::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void udf::type_checker::do_print_node(udf::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
}

void udf::type_checker::do_input_node(udf::input_node *const node, int lvl) {
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void udf::type_checker::do_for_node(udf::for_node *const node, int lvl) {
  //TODO: rever
   _symtab.push();
  if (node->init()) {
    node->init()->accept(this, lvl + 2);

    // Verificação da regra do 'auto'
    int auto_count = 0, decl_count = 0;
    for (size_t i = 0; i < node->init()->size(); ++i) {
      auto decl = dynamic_cast<udf::variable_declaration_node*>(node->init()->node(i));
      if (decl) {
        ++decl_count;
        if (decl->type()->name() == cdk::TYPE_UNSPEC)
          ++auto_count;
      }
    }
    if (auto_count > 0 && (decl_count > 1 || auto_count > 1))
      throw std::string("only one auto declaration is allowed in the for initialization");
  }

  // Condição (expressões)
  if (node->condition()) {
    node->condition()->accept(this, lvl + 2);
    for (size_t i = 0; i < node->condition()->size(); ++i) {
      auto cond = dynamic_cast<cdk::expression_node*>(node->condition()->node(i));
      if (cond && !cond->is_typed(cdk::TYPE_INT))
        throw std::string("for condition must be integer");
    }
  }

  // Incremento (expressões)
  if (node->increment())
    node->increment()->accept(this, lvl + 2);

  // Corpo do for
  if (node->instruction())
    node->instruction()->accept(this, lvl + 2);
  
  _symtab.pop();
}

void udf::type_checker::do_break_node(udf::break_node *const node, int lvl) {
  // EMPTY
}
void udf::type_checker::do_continue_node(udf::continue_node *const node, int lvl) {
  // EMPTY
}
void udf::type_checker::do_return_node(udf::return_node *const node, int lvl) {
  if (node->retval()) {
    auto func_type = _function->type();
    if (func_type != nullptr && func_type->name() == cdk::TYPE_VOID) {
      throw std::string("cannot return a value from a void function");
    }
    node->retval()->accept(this, lvl + 2);

    if (func_type == nullptr) {
      _function->set_type(node->retval()->type());
      return;
    }

    if (func_type->name() == cdk::TYPE_INT) {
      if (!node->retval()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type for initializer (integer expected).");
    } else if (func_type->name() == cdk::TYPE_DOUBLE) {
      if (!node->retval()->is_typed(cdk::TYPE_INT) && !node->retval()->is_typed(cdk::TYPE_DOUBLE)) {
        throw std::string("wrong type for initializer (integer or double expected).");
      }
    } else if (func_type->name() == cdk::TYPE_STRING) {
      if (!node->retval()->is_typed(cdk::TYPE_STRING)) {
        throw std::string("wrong type for initializer (string expected).");
      }
    } else if (func_type->name() == cdk::TYPE_POINTER) {
      auto fptr = cdk::reference_type::cast(func_type);
      auto rptr = cdk::reference_type::cast(node->retval()->type());
      if (!node->retval()->is_typed(cdk::TYPE_POINTER)) {
        throw std::string("wrong type for initializer (pointer expected).");
      }
      if (fptr->referenced() && rptr->referenced() &&
        fptr->referenced()->name() != rptr->referenced()->name()) {
        throw std::string("incompatible pointer types in return");
      }
    } else if (func_type->name() == cdk::TYPE_TENSOR) {
      if (!node->retval()->is_typed(cdk::TYPE_TENSOR)) {
        throw std::string("wrong type for initializer (tensor expected).");
      }
    } else {
      throw std::string("unknown type for initializer.");
    }
  }
}

//---------------------------------------------------------------------------

void udf::type_checker::do_if_node(udf::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) throw std::string("expected integer condition");
}

void udf::type_checker::do_if_else_node(udf::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) throw std::string("expected integer condition");
}

//---------------------------------------------------------------------------

void udf::type_checker::do_sizeof_node(udf::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->expression()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void udf::type_checker::do_address_of_node(udf::address_of_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

void udf::type_checker::do_index_node(udf::index_node *const node, int lvl) {
  ASSERT_UNSPEC;
  std::shared_ptr < cdk::reference_type > btype;

  if (node->ptr()) {
    node->ptr()->accept(this, lvl + 2);
    btype = cdk::reference_type::cast(node->ptr()->type());
    if (!node->ptr()->is_typed(cdk::TYPE_POINTER)) throw std::string("pointer expression expected in index left-value");
  } else {
    btype = cdk::reference_type::cast(_function->type());
    if (!_function->is_typed(cdk::TYPE_POINTER)) throw std::string("return pointer expression expected in index left-value");
  }

  node->index()->accept(this, lvl + 2);
  if (!node->index()->is_typed(cdk::TYPE_INT)) throw std::string("integer expression expected in left-value index");

  node->type(btype->referenced());
}

void udf::type_checker::do_objects_alloc_node(udf::objects_alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in objects allocation");
  }
  node->type(cdk::reference_type::create(4, nullptr));
}

void udf::type_checker::do_function_call_node(udf::function_call_node *const node, int lvl) {
  ASSERT_UNSPEC;

  const std::string &id = node->identifier();
  auto symbol = _symtab.find(id);
  if (!symbol) throw std::string("symbol '" + id + "' is undeclared.");
  if (!symbol->isFunction()) throw std::string("symbol '" + id + "' is not a function.");
  
  /*
  TODO:??
  if (symbol->is_typed(cdk::TYPE_STRUCT)) {
    // declare return variable for passing to function call
    const std::string return_var_name = "$return_" + id;
    auto return_symbol = udf::make_symbol(false, symbol->qualifier(), symbol->type(), return_var_name, false, false);
    if (_symtab.insert(return_var_name, return_symbol)) {
    } else {
      // if already declared, ignore new insertion
    }
  }
  */
  node->type(symbol->type());

  if (node->arguments()->size() == symbol->number_of_arguments()) {
    node->arguments()->accept(this, lvl + 4);
    for (size_t ax = 0; ax < node->arguments()->size(); ax++) {
      auto expected = symbol->argument_type(ax);
      auto actual = node->argument(ax)->type();
      if (actual == expected) continue;
      if (expected && expected->name() == cdk::TYPE_DOUBLE && node->argument(ax)->is_typed(cdk::TYPE_INT)) continue;
      throw std::string("type mismatch for argument " + std::to_string(ax + 1) + " of '" + id + "'.");
    }
  } else {
    throw std::string(
      "number of arguments in call (" + std::to_string(node->arguments()->size()) + ") must match declaration ("
      + std::to_string(symbol->number_of_arguments()) + ")."
    );
  }
}

void udf::type_checker::do_function_definition_node(udf::function_definition_node *const node, int lvl) {
  std::string id = node->identifier();
  if (id == "udf")
    id = "_main";
  else if (id == "_main")
    id = "._main";
  
  auto function = udf::make_symbol(false, node->qualifier(), node->type(), id, false, true);

  std::vector < std::shared_ptr < cdk::basic_type >> argtypes;
  for (size_t ax = 0; ax < node->arguments()->size(); ax++)
    argtypes.push_back(node->argument(ax)->type());
  function->set_argument_types(argtypes);

  std::shared_ptr<udf::symbol> previous = _symtab.find(function->name());
  if (previous) {
    if (previous->forward()
        && ((previous->qualifier() == tPUBLIC && node->qualifier() == tPUBLIC)
            || (previous->qualifier() == tPRIVATE && node->qualifier() == tPRIVATE))) {
      _symtab.replace(function->name(), function);
      _parent->set_new_symbol(function);
    } else {
      throw std::string("conflicting definition for '" + function->name() + "'");
    }
  } else {
    _symtab.insert(function->name(), function);
    _parent->set_new_symbol(function);
  }
}

void udf::type_checker::do_function_declaration_node(udf::function_declaration_node *const node, int lvl) {
  std::string id = node->identifier();

  if (id == "udf")
    id = "_main";
  else if (id == "_main")
    id = "._main";

  auto function = udf::make_symbol(false, node->qualifier(), node->type(), id, false, true, true);

  std::vector<std::shared_ptr<cdk::basic_type>> argtypes;
  if (node->arguments()) {
    for (size_t ax = 0; ax < node->arguments()->size(); ax++)
      argtypes.push_back(node->argument(ax)->type());
  }
  function->set_argument_types(argtypes);

  std::shared_ptr<udf::symbol> previous = _symtab.find(function->name());
  if (previous) {
    // TODO: Comparar tipos/assinaturas
    throw std::string("conflicting declaration for '" + function->name() + "'");
  } else {
    _symtab.insert(function->name(), function);
    _parent->set_new_symbol(function);
  }
}

void udf::type_checker::do_variable_declaration_node(udf::variable_declaration_node *const node, int lvl) {
  if (node->initializer() != nullptr) {
    node->initializer()->accept(this, lvl + 2);

    if (node->is_typed(cdk::TYPE_INT)) {
      if (!node->initializer()->is_typed(cdk::TYPE_INT))
        throw std::string("wrong type for initializer (integer expected).");
    } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
      if (!node->initializer()->is_typed(cdk::TYPE_INT) && !node->initializer()->is_typed(cdk::TYPE_DOUBLE))
        throw std::string("wrong type for initializer (integer or double expected).");
    } else if (node->is_typed(cdk::TYPE_STRING)) {
      if (!node->initializer()->is_typed(cdk::TYPE_STRING))
        throw std::string("wrong type for initializer (string expected).");
    } else if (node->is_typed(cdk::TYPE_POINTER)) {
      if (!node->initializer()->is_typed(cdk::TYPE_POINTER)) {//TODO: rever FIXME
        auto in = dynamic_cast<cdk::literal_node<int>*>(node->initializer());
        if (in == nullptr || in->value() != 0)
          throw std::string("wrong type for initializer (pointer expected).");
      }
    } else if (node->is_typed(cdk::TYPE_TENSOR)) {
      if (!node->initializer()->is_typed(cdk::TYPE_TENSOR))
        throw std::string("wrong type for initializer (tensor expected).");
    } else if (node->is_typed(cdk::TYPE_UNSPEC)) {
      auto init_type = node->initializer()->type();
      if (!init_type || init_type->name() == cdk::TYPE_UNSPEC)
        throw std::string("could not deduce type for auto variable");
      node->type(init_type);
    } else {
      throw std::string("unknown type for initializer.");
    }
  }

  const std::string &id = node->identifier();
  auto symbol = udf::make_symbol(false, node->qualifier(), node->type(), id, (bool)node->initializer(), false);
  if (_symtab.insert(id, symbol)) {
    _parent->set_new_symbol(symbol); 
  } else {
    throw std::string("variable '" + id + "' redeclared");
  }
}

void udf::type_checker::do_tensor_reshape_node(udf::tensor_reshape_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->tensor()->accept(this, lvl + 2);
  if (!node->tensor()->is_typed(cdk::TYPE_TENSOR))
    throw std::string("reshape only allowed on tensors");
  
  auto tensor_type = cdk::tensor_type::cast(node->tensor()->type());
  if (!tensor_type)
    throw std::string("invalid tensor type in reshape");

  size_t original_capacity = tensor_type->size();

  size_t new_capacity = 1;
  for (size_t i = 0; i < node->dimensions()->size(); ++i) {
    auto dim_node = dynamic_cast<cdk::integer_node*>(node->dimensions()->node(i));
    if (!dim_node || dim_node->value() <= 0)
      throw std::string("reshape dimensions must be positive integer literals");
    new_capacity *= dim_node->value();
  }

  if (original_capacity != new_capacity)
    throw std::string("reshape dimensions do not match tensor capacity");
  //TODO:retornar?
}

void udf::type_checker::do_tensor_index_node(udf::tensor_index_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->tensor()->accept(this, lvl + 2);

  if (!node->tensor()->is_typed(cdk::TYPE_TENSOR))
    throw std::string("indexing only allowed on tensors");

  auto tensor_type = cdk::tensor_type::cast(node->tensor()->type());
  if (!tensor_type)
    throw std::string("invalid tensor type in indexing");

  if (node->indexes()->size() != tensor_type->dims().size())
    throw std::string("number of indices does not match tensor rank");

  for (size_t i = 0; i < node->indexes()->size(); ++i) {
    node->indexes()->node(i)->accept(this, lvl + 2);
    auto expr = dynamic_cast<cdk::expression_node*>(node->indexes()->node(i));
    if (!expr || !expr->is_typed(cdk::TYPE_INT))
      throw std::string("tensor indices must be integer expressions");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void udf::type_checker::do_tensor_rank_node(udf::tensor_rank_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->tensor()->accept(this, lvl + 2);
  if (!node->tensor()->is_typed(cdk::TYPE_TENSOR))
    throw std::string("rank only allowed on tensors");
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

static void validate_tensor_shape_and_type(cdk::sequence_node *seq, const std::vector<size_t> &shape, size_t depth, udf::type_checker *checker, int lvl) {
  if (depth >= shape.size())
    throw std::string("tensor shape mismatch: too many nesting levels");

  if (seq->size() != shape[depth])
    throw std::string("tensor shape mismatch: wrong number of elements at depth " + std::to_string(depth));

  for (size_t i = 0; i < seq->size(); ++i) {
    auto node = seq->node(i);
    if (depth + 1 == shape.size()) {
      node->accept(checker, lvl + 2);
      auto expr = dynamic_cast<cdk::expression_node*>(node);
      if (!expr || (!expr->is_typed(cdk::TYPE_INT) && !expr->is_typed(cdk::TYPE_DOUBLE)))
        throw std::string("tensor elements must be int or double at innermost depth");
    } else {
      // Other tensor_node in tensor
      auto tensor_child = dynamic_cast<udf::tensor_node*>(node);
      if (!tensor_child)
        throw std::string("tensor shape mismatch: expected nested tensor at depth " + std::to_string(depth));
      validate_tensor_shape_and_type(tensor_child->values(), shape, depth + 1, checker, lvl);
    }
  }
}

void udf::type_checker::do_tensor_node(udf::tensor_node *const node, int lvl) {
  ASSERT_UNSPEC;

  std::vector<size_t> shape;
  const udf::tensor_node *current = node;

  while (true) {
    cdk::sequence_node *seq = current->values();
    size_t sz = seq->size();
    if (sz == 0) throw std::string("tensor dimensions must be > 0");
    shape.push_back(sz);
    auto first = seq->node(0);
    auto tensor_child = dynamic_cast<udf::tensor_node*>(first);
    if (!tensor_child) break;
    current = tensor_child;
  }

  validate_tensor_shape_and_type(node->values(), shape, 0, this, lvl);
  node->type(cdk::tensor_type::create(shape));
}

void udf::type_checker::do_tensor_contraction_node(udf::tensor_contraction_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->tensor1()->accept(this, lvl + 2);
  node->tensor2()->accept(this, lvl + 2);

  if (!node->tensor1()->is_typed(cdk::TYPE_TENSOR) || !node->tensor2()->is_typed(cdk::TYPE_TENSOR))
    throw std::string("tensor contraction only allowed between tensors");

  auto t1_type = cdk::tensor_type::cast(node->tensor1()->type());
  auto t2_type = cdk::tensor_type::cast(node->tensor2()->type());
  if (!t1_type || !t2_type)
    throw std::string("invalid tensor type in contraction");

  const auto &dims1 = t1_type->dims();
  const auto &dims2 = t2_type->dims();

  //TODO: será?
  if (dims1.empty() || dims2.empty())
    throw std::string("cannot contract tensors with zero dimensions");

  if (dims1.back() != dims2.front())
    throw std::string("tensor contraction: last dimension of first tensor must match first dimension of second tensor");
}

void udf::type_checker::do_tensor_capacity_node(udf::tensor_capacity_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->tensor()->accept(this, lvl + 2);
  if (!node->tensor()->is_typed(cdk::TYPE_TENSOR))
    throw std::string("capacity only allowed on tensors");
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void udf::type_checker::do_tensor_dim_node(udf::tensor_dim_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->tensor()->accept(this, lvl + 2);
  node->index()->accept(this, lvl + 2);
  if (!node->tensor()->is_typed(cdk::TYPE_TENSOR))
    throw std::string("dim only allowed on tensors");
  if (!node->index()->is_typed(cdk::TYPE_INT))
    throw std::string("tensor dimension index must be integer");
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void udf::type_checker::do_tensor_dims_node(udf::tensor_dims_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->tensor()->accept(this, lvl + 2);
  if (!node->tensor()->is_typed(cdk::TYPE_TENSOR))
    throw std::string("dims only allowed on tensors");
  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_INT)));
}