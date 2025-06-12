#include <string>
#include <sstream>
#include <functional>
//#include <memory> TODO: check if needed
#include <cdk/types/types.h>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include "targets/symbol.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated
#include "udf_parser.tab.h"

//---------------------------------------------------------------------------

void udf::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void udf::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void udf::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  if (_inFunctionBody) {
    _pf.DOUBLE(node->value()); // load number to the stack
  } else {
    _pf.SDOUBLE(node->value());    // double is on the DATA segment
  }
}
void udf::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}
void udf::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}
void udf::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

void udf::postfix_writer::do_block_node(udf::block_node *const node, int lvl) {
  _symtab.push(); 
  if (node->declarations()) node->declarations()->accept(this, lvl + 2);
  if (node->instructions()) node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (_inFunctionBody) {
    _pf.INT(node->value()); // integer literal is on the stack: push an integer
  } else {
    _pf.SINT(node->value()); // integer literal is on the DATA segment
  }
}

void udf::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  int lbl1;
  /* generate the string literal */
  _pf.RODATA(); // strings are readonly DATA
  _pf.ALIGN(); // make sure the address is aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  _pf.SSTRING(node->value()); // output string characters
  if (_function) {
    // local variable initializer
    _pf.TEXT();
    _pf.ADDR(mklbl(lbl1));
  } else {
    // global variable initializer
    _pf.DATA();
    _pf.SADDR(mklbl(lbl1));
  }
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_unary_minus_node(cdk::unary_minus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

void udf::postfix_writer::do_unary_plus_node(cdk::unary_plus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  // Check if one operand is tensor and the other is scalar (int or double)
  bool left_tensor = node->left()->type()->name() == cdk::TYPE_TENSOR;
  bool right_tensor = node->right()->type()->name() == cdk::TYPE_TENSOR;

  if (left_tensor && !right_tensor) {
    // tensor * scalar
    node->right()->accept(this, lvl + 2);
    if (node->right()->type()->name() == cdk::TYPE_INT)
      _pf.I2D(); // convert int to double if needed
    node->left()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_add_scalar");
    _pf.CALL("tensor_add_scalar");
    _pf.TRASH(8); // scalar (double) + tensor pointer
    _pf.LDFVAL32();
    return;
  } else if (!left_tensor && right_tensor) {
    // scalar * tensor
    node->left()->accept(this, lvl + 2);
    if (node->left()->type()->name() == cdk::TYPE_INT)
      _pf.I2D(); // convert int to double if needed
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_add_scalar");
    _pf.CALL("tensor_add_scalar");
    _pf.TRASH(8); // scalar (double) + tensor pointer
    _pf.LDFVAL32();
    return;
  } else if (left_tensor && right_tensor) {
    // tensor * tensor
    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_add");
    _pf.CALL("tensor_add");
    _pf.TRASH(8); // two tensor pointers
    _pf.LDFVAL32();
    return;
  } 

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->left()->type()->name() == cdk::TYPE_INT) {
    auto ref = cdk::reference_type::cast(node->type());
    _pf.INT(ref->referenced()->size());
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    auto ref = cdk::reference_type::cast(node->type());
    _pf.INT(ref->referenced()->size());
    _pf.MUL();
  }

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DADD();
  else
    _pf.ADD();
}

void udf::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  // Check if one operand is tensor and the other is scalar (int or double)
  bool left_tensor = node->left()->type()->name() == cdk::TYPE_TENSOR;
  bool right_tensor = node->right()->type()->name() == cdk::TYPE_TENSOR;

  if (left_tensor && !right_tensor) {
    // tensor - scalar
    node->right()->accept(this, lvl + 2);
    if (node->right()->type()->name() == cdk::TYPE_INT)
      _pf.I2D(); // convert int to double if needed
    node->left()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_sub_scalar");
    _pf.CALL("tensor_sub_scalar");
    _pf.TRASH(8); // scalar (double) + tensor pointer
    _pf.LDFVAL32();
    return;
  } else if (left_tensor && right_tensor) {
    // tensor - tensor
    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_sub");
    _pf.CALL("tensor_sub");
    _pf.TRASH(8); // two tensor pointers
    _pf.LDFVAL32();
    return;
  } 

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    auto ref = cdk::reference_type::cast(node->type());
    _pf.INT(ref->referenced()->size());
    _pf.MUL();
  }

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DSUB();
  else
    _pf.SUB();
}

void udf::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  // Check if one operand is tensor and the other is scalar (int or double)
  bool left_tensor = node->left()->type()->name() == cdk::TYPE_TENSOR;
  bool right_tensor = node->right()->type()->name() == cdk::TYPE_TENSOR;

  if (left_tensor && !right_tensor) {
    // tensor * scalar
    node->right()->accept(this, lvl + 2);
    if (node->right()->type()->name() == cdk::TYPE_INT)
      _pf.I2D(); // convert int to double if needed
    node->left()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_mul_scalar");
    _pf.CALL("tensor_mul_scalar");
    _pf.TRASH(8); // scalar (double) + tensor pointer
    _pf.LDFVAL32();
    return;
  } else if (!left_tensor && right_tensor) {
    // scalar * tensor
    node->left()->accept(this, lvl + 2);
    if (node->left()->type()->name() == cdk::TYPE_INT)
      _pf.I2D(); // convert int to double if needed
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_mul_scalar");
    _pf.CALL("tensor_mul_scalar");
    _pf.TRASH(8); // scalar (double) + tensor pointer
    _pf.LDFVAL32();
    return;
  } else if (left_tensor && right_tensor) {
    // tensor * tensor
    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_mul");
    _pf.CALL("tensor_mul");
    _pf.TRASH(8); // two tensor pointers
    _pf.LDFVAL32();
    return;
  } 

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DMUL();
  else
    _pf.MUL();
}
void udf::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  // Check if one operand is tensor and the other is scalar (int or double)
  bool left_tensor = node->left()->type()->name() == cdk::TYPE_TENSOR;
  bool right_tensor = node->right()->type()->name() == cdk::TYPE_TENSOR;

  if (left_tensor && !right_tensor) {
    // tensor / scalar
    node->right()->accept(this, lvl + 2);
    if (node->right()->type()->name() == cdk::TYPE_INT)
      _pf.I2D(); // convert int to double if needed
    node->left()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_div_scalar");
    _pf.CALL("tensor_div_scalar");
    _pf.TRASH(8); // scalar (double) + tensor pointer
    _pf.LDFVAL32();
    return;
  } else if (left_tensor && right_tensor) {
    // tensor / tensor
    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_div");
    _pf.CALL("tensor_div");
    _pf.TRASH(8); // two tensor pointers
    _pf.LDFVAL32();
    return;
  }

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DDIV();
  else
    _pf.DIV();
}
void udf::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}
void udf::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.LT();
}

void udf::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  _pf.LE();
}
void udf::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  _pf.GE();
}
void udf::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) _pf.I2D(); 
  
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.GT();
}
void udf::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  bool left_tensor = node->left()->type()->name() == cdk::TYPE_TENSOR;
  bool right_tensor = node->right()->type()->name() == cdk::TYPE_TENSOR;

  if (left_tensor && right_tensor) {
    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_equals");
    _pf.CALL("tensor_equals");
    _pf.TRASH(8); // two tensor pointers
    _pf.LDFVAL32();
    // invert result: if tensor_equals returns 0, push 1; if returns 1, push 0
    _pf.INT(0);
    _pf.EQ();
    return;
  }
  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  _pf.NE();
}
void udf::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  // Support tensor == tensor using tensor_equals
  bool left_tensor = node->left()->type()->name() == cdk::TYPE_TENSOR;
  bool right_tensor = node->right()->type()->name() == cdk::TYPE_TENSOR;

  if (left_tensor && right_tensor) {
    node->left()->accept(this, lvl + 2);
    node->right()->accept(this, lvl + 2);
    _functions_to_declare.insert("tensor_equals");
    _pf.CALL("tensor_equals");
    _pf.TRASH(8); // two tensor pointers
    _pf.LDFVAL32();
    // tensor_equals returns 1 if equal, 0 otherwise
    return;
  }

  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  _pf.EQ();
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  const std::string &id = node->name();
  auto symbol = _symtab.find(id);
  //external???
  if (!symbol) {
    std::cerr << "Unknown variable: " << id << std::endl;
    return;
  }
  if (symbol->global()) {
    _pf.ADDR(symbol->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void udf::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if(dynamic_cast<udf::tensor_index_node*>(node->lvalue())) {
    node->lvalue()->accept(this, lvl);
    return;
  }
  node->lvalue()->accept(this, lvl);
  //external???
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.LDDOUBLE();
  } else {
    _pf.LDINT();
  }
}

void udf::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    if (node->rvalue()->type()->name() == cdk::TYPE_INT) _pf.I2D();
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }
  _in_lvalue = true;
  node->lvalue()->accept(this, lvl);
  _in_lvalue = false;
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_evaluation_node(udf::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  //nao percebo bem
  _pf.TRASH(node->argument()->type()->size());
}

void udf::postfix_writer::do_print_node(udf::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ix));

    std::shared_ptr<cdk::basic_type> etype = child->type();
    child->accept(this, lvl); // expression to print
    if (etype->name() == cdk::TYPE_INT) {
      _functions_to_declare.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4); // trash int
    } else if (etype->name() == cdk::TYPE_DOUBLE) {
      _functions_to_declare.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8); // trash double
    } else if (etype->name() == cdk::TYPE_STRING) {
      _functions_to_declare.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4); // trash char pointer
    } else if (etype->name() == cdk::TYPE_TENSOR) {
      _functions_to_declare.insert("tensor_print");
      _pf.CALL("tensor_print");
      _pf.TRASH(4); // trash pointer to tensor
    } else {
      std::cerr << "cannot print expression of unknown type" << std::endl;
      return;
    }

  }

  if (node->newline()) {
    _functions_to_declare.insert("println");
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_input_node(udf::input_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_lvalueType == cdk::TYPE_DOUBLE) {
    _functions_to_declare.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
  } else if (_lvalueType == cdk::TYPE_INT) {
    _functions_to_declare.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
  } else {
    std::cerr << "FATAL: " << node->lineno() << ": cannot read type" << std::endl;
    return;
  }
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_for_node(udf::for_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _symtab.push();
  int lbl1, lbl2, lbl3;
  node->init()->accept(this, lvl);
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl2 = ++_lbl));
  node->instruction()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl3 = ++_lbl));
  node->increment()->accept(this, lvl);
  _pf.JMP(mklbl(lbl1));
  _pf.LABEL(mklbl(lbl2));
  _symtab.pop();
}

void udf::postfix_writer::do_break_node(udf::break_node *const node, int lvl) {
  if (_forIni.size() != 0) {
    _pf.JMP(mklbl(_forEnd.top())); // jump to for end
  } else
    error(node->lineno(), "'break' outside 'for'");
}

void udf::postfix_writer::do_continue_node(udf::continue_node *const node, int lvl) {
  if (_forIni.size() != 0) {
    _pf.JMP(mklbl(_forStep.top())); // jump to next cycle
  } else
    error(node->lineno(), "'restart' outside 'for'");
}

void udf::postfix_writer::do_return_node(udf::return_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_function->type()->name() != cdk::TYPE_VOID) {
    node->retval()->accept(this, lvl + 2);

    if (_function->type()->name() == cdk::TYPE_INT || _function->type()->name() == cdk::TYPE_STRING
        || _function->type()->name() == cdk::TYPE_POINTER) {
      _pf.STFVAL32();
    } else if (_function->type()->name() == cdk::TYPE_DOUBLE) {
      if (node->retval()->type()->name() == cdk::TYPE_INT) _pf.I2D();
      _pf.STFVAL64();
    } else {
      std::cerr << node->lineno() << ": should not happen: unknown return type" << std::endl;
    }
  }

  _pf.JMP(_currentBodyRetLabel);
  _returnSeen = true;

}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_if_node(udf::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_if_else_node(udf::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl_else, lbl_end;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl_else = lbl_end = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  if (node->elseblock()) {
    _pf.JMP(mklbl(lbl_end = ++_lbl));
    _pf.LABEL(mklbl(lbl_else));
    node->elseblock()->accept(this, lvl + 2);
  }
  _pf.LABEL(mklbl(lbl_end));
}

//---------------------------------------------------------------------------

void udf::postfix_writer::do_sizeof_node(udf::sizeof_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.INT(node->expression()->type()->size());
}

void udf::postfix_writer::do_nullptr_node(udf::nullptr_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS; // a pointer is a 32-bit integer
  if (_inFunctionBody) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}

void udf::postfix_writer::do_address_of_node(udf::address_of_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // since the argument is an lvalue, it is already an address
  node->lvalue()->accept(this, lvl + 2);
}

void udf::postfix_writer::do_index_node(udf::index_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->ptr()->accept(this, lvl);
  node->index()->accept(this, lvl);
  _pf.INT(node->type()->size()); // type size
  _pf.MUL();                     // type size * index
  _pf.ADD();                     // ptr + (type size * index)
}

void udf::postfix_writer::do_objects_alloc_node(udf::objects_alloc_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  auto pointed = cdk::reference_type::cast(node->type())->referenced();
  size_t size = pointed->size();
  _pf.INT(size);
  _pf.MUL();
  _pf.ALLOC(); // allocate
  _pf.SP(); // put base pointer in stack
}

void udf::postfix_writer::do_function_call_node(udf::function_call_node * const node, int lvl) {
  auto symbol = _symtab.find(node->identifier());

  size_t argsSize = 0;
  if (node->arguments()->size() > 0) {
    for (int ax = node->arguments()->size() - 1; ax >= 0; ax--) {
      cdk::expression_node *arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ax));
      arg->accept(this, lvl + 2);
      if (symbol->argument_is_typed(ax, cdk::TYPE_DOUBLE) && arg->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
      }
      argsSize += symbol->argument_size(ax);
    }
  }
  _pf.CALL(node->identifier());
  if (argsSize != 0) {
    _pf.TRASH(argsSize);
  }

  if (symbol->is_typed(cdk::TYPE_INT) || symbol->is_typed(cdk::TYPE_POINTER) || symbol->is_typed(cdk::TYPE_STRING)) {
    _pf.LDFVAL32();
  } else if (symbol->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDFVAL64();
  } else {
    // cannot happer!
  }
}

void udf::postfix_writer::do_function_definition_node(udf::function_definition_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody || _inFunctionArgs) {
    error(node->lineno(), "cannot define function in body or in arguments");
    return;
  }


  // remember symbol so that args and body know
  _function = new_symbol();
  _functions_to_declare.erase(_function->name());  // just in case
  reset_new_symbol();

  _currentBodyRetLabel = mklbl(++_lbl);

  _offset = 8; // prepare for arguments (4: remember to account for return address)
  _symtab.push(); // scope of args

  if (node->arguments()->size() > 0) {
    _inFunctionArgs = true; //FIXME really needed?
    for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
      cdk::basic_node *arg = node->arguments()->node(ix);
      if (arg == nullptr) break; // this means an empty sequence of arguments
      arg->accept(this, 0); // the function symbol is at the top of the stack
    }
    _inFunctionArgs = false; //FIXME really needed?
  }

  _pf.TEXT();
  _pf.ALIGN();
  if (node->qualifier() == tPUBLIC) _pf.GLOBAL(_function->name(), _pf.FUNC());
  _pf.LABEL(_function->name());

  if (node->identifier() == "udf") { // ou "_main"
    _functions_to_declare.insert("mem_init");
    _pf.CALL("mem_init");
  }

  // compute stack size to be reserved for local variables
  frame_size_calculator lsc(_compiler, _symtab, _function);
  node->accept(&lsc, lvl);
  _pf.ENTER(lsc.localsize()); // total stack size reserved for local variables

  _offset = 0; // prepare for local variable

  // the following flag is a slight hack: it won't work with nested functions
  _inFunctionBody = true;
  os() << "        ;; before body " << std::endl;
  node->block()->accept(this, lvl + 4); // block has its own scope
  os() << "        ;; after body " << std::endl;
  _inFunctionBody = false;
  _returnSeen = false;

  _pf.LABEL(_currentBodyRetLabel);
  _pf.LEAVE();
  _pf.RET();

  _symtab.pop(); // scope of arguments

  if (node->identifier() == "udf") {
    // declare external functions
    for (std::string s : _functions_to_declare)
      _pf.EXTERN(s);
  }
}

void udf::postfix_writer::do_function_declaration_node(udf::function_declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody || _inFunctionArgs) {
    error(node->lineno(), "cannot declare function in body or in args");
    return;
  }

  if (!new_symbol()) return;

  auto function = new_symbol();
  _functions_to_declare.insert(function->name());
  reset_new_symbol();
}

void udf::postfix_writer::do_variable_declaration_node(udf::variable_declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto id = node->identifier();

  std::cout << "INITIAL OFFSET: " << _offset << std::endl;

  // type size?
  int offset = 0, typesize = node->type()->size(); // in bytes
  std::cout << "ARG: " << id << ", " << typesize << std::endl;
  if (_inFunctionBody) {
    std::cout << "IN BODY" << std::endl;
    _offset -= typesize;
    offset = _offset;
  } else if (_inFunctionArgs) {
    std::cout << "IN ARGS" << std::endl;
    offset = _offset;
    _offset += typesize;
  } else {
    std::cout << "GLOBAL!" << std::endl;
    offset = 0; // global variable
  }
  std::cout << "OFFSET: " << id << ", " << offset << std::endl;

  auto symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  if (_inFunctionBody) {
    // if we are dealing with local variables, then no action is needed
    // unless an initializer exists
    if (node->initializer()) {
      node->initializer()->accept(this, lvl);
      if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_STRING) || node->is_typed(cdk::TYPE_POINTER) || node->is_typed(cdk::TYPE_TENSOR)) {
        _pf.LOCAL(symbol->offset());
        _pf.STINT();
      } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
        if (node->initializer()->is_typed(cdk::TYPE_INT))
          _pf.I2D();
        _pf.LOCAL(symbol->offset());
        _pf.STDOUBLE();
      } else {
        std::cerr << "cannot initialize" << std::endl;
      }
    } else {
      if (node->is_typed(cdk::TYPE_TENSOR)) {
        auto tensor_type = cdk::tensor_type::cast(node->type());
        for (int i = tensor_type->n_dims() - 1; i >= 0; i--) {
          size_t dim = tensor_type->dim(i);
          _pf.INT(dim);
        }
        _pf.INT(tensor_type->n_dims());
        _functions_to_declare.insert("tensor_zeros");
        _pf.CALL("tensor_zeros");
        _pf.TRASH((tensor_type->n_dims() + 1) * 4);
        _pf.LDFVAL32();
        _pf.LOCAL(symbol->offset());
        _pf.STINT();
      }
    }
  } else {
    if (!_function) {
      if (node->initializer() == nullptr) {
        _pf.BSS();
        _pf.ALIGN();
        _pf.LABEL(id);
        _pf.SALLOC(typesize);
      } else {

        if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_DOUBLE) || node->is_typed(cdk::TYPE_POINTER) || node->is_typed(cdk::TYPE_TENSOR)) {
          /*if (node->constant()) {
            _pf.RODATA();
          } else {
            _pf.DATA();
          }*/ //Pensar nisto
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);

          if (node->is_typed(cdk::TYPE_INT)) {
            node->initializer()->accept(this, lvl);
          } else if (node->is_typed(cdk::TYPE_POINTER)) {
            node->initializer()->accept(this, lvl);
          }  else if (node->is_typed(cdk::TYPE_DOUBLE)) {
            if (node->initializer()->is_typed(cdk::TYPE_DOUBLE)) {
              node->initializer()->accept(this, lvl);
            } else if (node->initializer()->is_typed(cdk::TYPE_INT)) {
              cdk::integer_node *dclini = dynamic_cast<cdk::integer_node*>(node->initializer());
              cdk::double_node ddi(dclini->lineno(), dclini->value());
              ddi.accept(this, lvl);
            } else {
              std::cerr << node->lineno() << ": '" << id << "' has bad initializer for real value\n";
              _errors = true;
            }
          } else if (node->is_typed(cdk::TYPE_TENSOR)) {
            node->initializer()->accept(this, lvl);
          }
        } else if (node->is_typed(cdk::TYPE_STRING)) {
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);
          node->initializer()->accept(this, lvl);
        } else {
          std::cerr << node->lineno() << ": '" << id << "' has unexpected initializer\n";
          _errors = true;
        }
      }
    }
  }
}

void udf::postfix_writer::do_tensor_reshape_node(udf::tensor_reshape_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  // Push each dimension value onto the stack
  for (int i = node->dimensions()->size() - 1; i >= 0; --i) {
    auto expr = dynamic_cast<cdk::expression_node*>(node->dimensions()->node(i));
    expr->accept(this, lvl + 2);
  }
  _pf.INT(node->dimensions()->size()); // number of dimensions
  node->tensor()->accept(this, lvl + 2);

  _functions_to_declare.insert("tensor_reshape");
  _pf.CALL("tensor_reshape");
  _pf.TRASH(4 * (node->dimensions()->size() + 1)); // remove arguments (dimensions + tensor pointer)
  _pf.LDFVAL32();
}

void udf::postfix_writer::do_tensor_index_node(udf::tensor_index_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto tensor_type = cdk::tensor_type::cast(node->tensor()->type());
  for(size_t i = 0; i < node->indexes()->size(); i++) {
    node->indexes()->node(i)->accept(this, lvl + 2);
  }
  node->tensor()->accept(this, lvl + 2);
  if (_in_lvalue) {
    _functions_to_declare.insert("tensor_getptr");
    _pf.CALL("tensor_getptr");
    _pf.TRASH((tensor_type->n_dims() + 1) * 4);
    _pf.LDFVAL32();
  } else {
    _functions_to_declare.insert("tensor_get");
    _pf.CALL("tensor_get");
    _pf.TRASH((tensor_type->n_dims() + 1) * 4);
    _pf.LDFVAL64();
  }
}

void udf::postfix_writer::do_tensor_rank_node(udf::tensor_rank_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->tensor()->accept(this, lvl);
  _pf.LDINT();
}

void udf::postfix_writer::do_tensor_node(udf::tensor_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto tensor_type = cdk::tensor_type::cast(node->type());
  const auto &dims = tensor_type->dims();
  for (auto it = dims.rbegin(); it != dims.rend(); ++it) {
    _pf.INT(*it);
  }
  _pf.INT(dims.size());
  
  _functions_to_declare.insert("tensor_create");
  _pf.CALL("tensor_create");
  _pf.TRASH(4 * (dims.size() + 1)); // remove argumentos
  _pf.LDFVAL32();

  std::function<void(cdk::sequence_node*, size_t&, int)> emit_tensor_values;
  emit_tensor_values = [&](cdk::sequence_node *seq, size_t &idx, int lvl) {
    for (size_t i = 0; i < seq->size(); ++i) {
      auto nodeptr = seq->node(i);
      if (auto tnode = dynamic_cast<udf::tensor_node*>(nodeptr)) {
        emit_tensor_values(tnode->values(), idx, lvl);
      } else {
        _pf.DUP32(); // tensor pointer
        _pf.INT(idx); // linear index
        
        auto expr = dynamic_cast<cdk::expression_node*>(nodeptr);
        expr->accept(this, lvl); // tensor value
        if (expr->is_typed(cdk::TYPE_INT)) {
          _pf.I2D(); 
        } 
        _functions_to_declare.insert("tensor_put");
        _pf.CALL("tensor_put");
        _pf.TRASH(12); // 3 argments * 4 bytes
        ++idx;
      }
    }
  };

  size_t idx = 0;
  emit_tensor_values(node->values(), idx, lvl);
}

void udf::postfix_writer::do_tensor_contraction_node(udf::tensor_contraction_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->tensor2()->accept(this, lvl + 2);
  node->tensor1()->accept(this, lvl + 2);
  _functions_to_declare.insert("tensor_matmul");
  _pf.CALL("tensor_matmul");
  _pf.TRASH(8); // remove arguments (2 pointers to tensors)
  _pf.LDFVAL32();
}

void udf::postfix_writer::do_tensor_capacity_node(udf::tensor_capacity_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->tensor()->accept(this, lvl);
  _functions_to_declare.insert("tensor_size");
  _pf.CALL("tensor_size");
  _pf.TRASH(4);
  _pf.LDFVAL32();
}

void udf::postfix_writer::do_tensor_dim_node(udf::tensor_dim_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->index()->accept(this, lvl + 2); // get dimension index
  node->tensor()->accept(this, lvl + 2);
  _functions_to_declare.insert("tensor_get_dim_size");
  _pf.CALL("tensor_get_dim_size");
  _pf.TRASH(4); // remove argument (pointer to tensor)
  _pf.LDFVAL32(); // return value is a pointer to an array of dimensions
}

void udf::postfix_writer::do_tensor_dims_node(udf::tensor_dims_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->tensor()->accept(this, lvl + 2);
  _functions_to_declare.insert("tensor_get_dims");
  _pf.CALL("tensor_get_dims");
  _pf.TRASH(4); // remove argument (pointer to tensor)
  _pf.LDFVAL32(); // return value is a pointer to an array of dimensions
}