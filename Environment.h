//==--- tools/clang-check/ClangInterpreter.cpp - Clang Interpreter tool --------------===//
//===----------------------------------------------------------------------===//
#include <stdio.h>

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

using namespace clang;

/* to implement pointer */
class Pointer {
private:
  void *ptr;
  int type;                    /*  point to what type, 1 means int, 2 means pointer */
  Pointer *pointer;             /*  remained to future needs */

public:
  Pointer():ptr(NULL), type(-1), pointer(NULL) {}
  void *getPtr() {
    return ptr;
  }
  void setPtr(void *base) {
    ptr = base;
  }
  void setType(int type) {
    this->type = type;
  }
  int getType() {
    return type;
  }
  Pointer *getChild() {
    return pointer;
  }
  void setChild(Pointer *child) {
    pointer = child;
  }
};

/* to implement array */
class Array : public Pointer {
private:
  int length;

public:
  Array():length(0) {}
  void setLength(int len) {
    length = len;
  }
  int getLength() {
    return length;
  }

  int &operator[](int idx) {
    return ((int *)this->getPtr())[idx];
  }
};

union ValPtr {
  int val;
  void *ptr;
};

class StackFrame {
   /// StackFrame maps Variable Declaration to Value
   /// Which are either integer or addresses (also represented using an Integer value)
   std::map<Decl*, ValPtr> mVars;
   std::map<Stmt*, ValPtr> mExprs;
   //store array
   std::map<std::string, Array> mArray;
   //store pointers
   std::map<std::string, Pointer> mPointer;
   /// The current stmt
   Stmt * mPC;
public:
   StackFrame() : mVars(), mExprs(), mArray(),mPointer(), mPC() {}
   //work for values
   void bindDeclVal(Decl* decl, int val) {
      mVars[decl].val = val;
   }    
   int getDeclVal(Decl * decl) {
      assert (mVars.find(decl) != mVars.end());
      return mVars[decl].val;
   }
   void bindStmtVal(Stmt * stmt, int val) {
	   mExprs[stmt].val = val;
   }
   int getStmtVal(Stmt * stmt) {
	   assert (mExprs.find(stmt) != mExprs.end());
	   return mExprs[stmt].val;
   }
   //work for pointers
   void bindStmtPtr(Stmt *stmt, void *ptr) {
     mExprs[stmt].ptr = ptr;
   }
   void *getStmtPtr(Stmt *stmt) {
     assert (mExprs.find(stmt) != mExprs.end() );
     return mExprs[stmt].ptr;
   }
   //bind an array to its name
   void bindArray(std::string name, Array array) {
      mArray[name] = array; 
   }
   //get an array by its name
   Array getArray(std::string name) {
     assert (mArray.find(name) != mArray.end());
     return mArray.find(name)->second;
   }
   //bind a pointer to its name
   void bindPointer(std::string name, Pointer pointer) {
     mPointer[name] = pointer;
   }
   //get an pointer by its name
   Pointer *getPointer(std::string name) {
     return &mPointer[name];
   }

   void setPC(Stmt * stmt) {
	   mPC = stmt;
   }
   Stmt * getPC() {
	   return mPC;
   }
};

/// Heap maps address to a value
//class Heap {
//public:
//   int Malloc(int size) ;
//   void Free (int addr) ;
//   void Update(int addr, int val) ;
//   int get(int addr);
//};

class Environment {
private:
  std::vector<StackFrame> mStack;

  FunctionDecl * mFree;				/// Declartions to the built-in functions
  FunctionDecl * mMalloc;
  FunctionDecl * mInput;
  FunctionDecl * mOutput;
  FunctionDecl * mEntry;
public:
  Environment() : mStack(), mFree(NULL), mMalloc(NULL), mInput(NULL), mOutput(NULL), mEntry(NULL) {}
  /*  get the main function entry */
  FunctionDecl * getEntry() { return mEntry; }
  //check condition
   int check(Stmt *cond) {
     return mStack.back().getStmtVal(cond);
   }
  /* initialize the Environment */
  void init(TranslationUnitDecl * unit);

  /*  actions when visit DeclStmt */
  void visitDeclStmt(DeclStmt * decl_stmt);
  /*  actions when visit DeclRefExpr */
  void visitDeclRefExpr(DeclRefExpr * decl_ref);
  /*  actions when visit CastExpr */
  void visitCastExpr(CastExpr *cast_expr);
  /*  actions when visit CallExpr */
  void visitCallExpr(CallExpr * call_expr);
  /*   actions when visit ReturnStmt */
  void visitReturnStmt(ReturnStmt *ret_stmt);
  /*  actions when visit BinaryOperator */
  void visitBinaryOperator(BinaryOperator *bop);
  /*  actions when visit UnaryOperator */
  void visitUnaryOperator(UnaryOperator *uop);
  /*  actions when visit sizeof */
  void visitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *expr);
  /*  actions when visit CStyleCastExpr */
  void visitCStyleCastExpr(CStyleCastExpr *c_style_cast_expr);
  /*  actions when visit ArraySubscriptExpr */
  void visitArraySubscriptExpr(ArraySubscriptExpr *array);
  /*  actions when visit ParenExpr */
  void visitParenExpr(ParenExpr *paren_expr);
  /*  actions when visit IntegerLiteral */
  void visitIntegerLiteral(IntegerLiteral *my_int);

};

/* initialize the Environment */
void Environment::init(TranslationUnitDecl * unit) {
  mStack.push_back(StackFrame());
  for (TranslationUnitDecl::decl_iterator i =unit->decls_begin(), e = unit->decls_end(); i != e; ++ i) {
    if (FunctionDecl * fdecl = dyn_cast<FunctionDecl>(*i) ) {    /* deal with function declaration */
 	    if (fdecl->getName().equals("FREE")) {
         mFree = fdecl;
       }
 	    else if (fdecl->getName().equals("MALLOC")) {
         mMalloc = fdecl;
       }
 	    else if (fdecl->getName().equals("GET")) {
         mInput = fdecl;
       }
 	    else if (fdecl->getName().equals("PRINT")){
         mOutput = fdecl;
       }
 	    else if (fdecl->getName().equals("main")) mEntry = fdecl;
    }
    else if (VarDecl *global_decl = dyn_cast<VarDecl>(*i)) {   /* deal with global variable declaration */
      int value = 0;                                         /* default initialized as 0 */
      if (global_decl->hasInit()) {                          /* if has init value */
        Expr *init = global_decl->getInit();
        llvm::APInt g_var = dyn_cast<IntegerLiteral>(init)->getValue();
        value = g_var.getSExtValue();
      }
      mStack.back().bindDeclVal(global_decl, value);
    }
  }
}

/*  actions when visit DeclStmt */
void Environment::visitDeclStmt(DeclStmt * decl_stmt) {
  for (DeclStmt::decl_iterator it = decl_stmt->decl_begin(), ie = decl_stmt->decl_end();
      it != ie; ++ it) {
    Decl * decl = *it;
    if (VarDecl * var_decl = dyn_cast<VarDecl>(decl)) {
      if (var_decl->getType()->isArrayType()) {           /* array declaration */
        std::string name = var_decl->getNameAsString();   /* get the name of the array */
        //get the length of array
        int len = dyn_cast<ConstantArrayType>(var_decl->getType())->getSize().getSExtValue();
        if(var_decl->hasInit()) {
          //@TODO init
        }
        else {
          void *base = malloc(len * sizeof(int));
          memset(base, len, 0);
          Array array;
          array.setPtr(base);
          array.setType(1);        /* type 1 means element is int */
          array.setLength(len);
          mStack.back().bindArray(name, array);
        }
      }
      else if (var_decl->getType()->isPointerType() ) {     /* pointer declaration */
        std::string name = var_decl->getNameAsString();
        std::string type = var_decl->getType().getAsString(); 
        Pointer pointer;
        pointer.setPtr(NULL);                        /* default init as NULL */
        if(type == "int *") {                        /* 1 demension pointer */
          pointer.setType(1);                        /* type 1 menas point to int */
          if (var_decl->hasInit() ) {
            Expr *init = var_decl->getInit();        /* if has init */
            void *ptr = mStack.back().getStmtPtr(init);
            pointer.setPtr(ptr);
          }
          mStack.back().bindPointer(name, pointer);
        }
        else if(type == "int **") {            /* 2 demension pointer */
          pointer.setType(2);                  /* type 2 means point to a pointer */
          if (var_decl->hasInit() ) {          /* if has init */
            Expr *init = var_decl->getInit();
            void *ptr = mStack.back().getStmtPtr(init);
            pointer.setPtr(ptr);
          }
          mStack.back().bindPointer(name, pointer);
        }
      }
      else {                                              /* variable declaration */
        int value = 0;
        if(var_decl->hasInit()) {                        /* if has init */
          Expr *init = var_decl->getInit();
          value = mStack.back().getStmtVal(init);
        }
        mStack.back().bindDeclVal(var_decl, value);
      }
    }
  }
}
   
/*  actions when visit DeclRefExpr */
void Environment::visitDeclRefExpr(DeclRefExpr * decl_ref) {
  mStack.back().setPC(decl_ref);
  if (decl_ref->getType()->isIntegerType()) {
    Decl* decl = decl_ref->getFoundDecl();
    int val = mStack.back().getDeclVal(decl);
    mStack.back().bindStmtVal(decl_ref, val);
  }
  //visit point, no need to distinguish pointer and 2d pointer
  //as we return a Pointer type ptr only
  else if(decl_ref->getType()->isPointerType() ) {
    std::string name = dyn_cast<VarDecl>(decl_ref->getReferencedDeclOfCallee())->getNameAsString();
    Pointer *ptr = mStack.back().getPointer(name);
    mStack.back().bindStmtPtr(decl_ref, ptr);
  }
}

/*  actions when visit CastExpr */
void Environment::visitCastExpr(CastExpr *cast_expr) {
  mStack.back().setPC(cast_expr);
  Expr *expr = cast_expr->getSubExpr();
  //deal with *ptr
  if(isa<UnaryOperator>(expr) ) {
    //*a need implicit cast, indicates that we need to get the value of *a
    UnaryOperator::Opcode op = dyn_cast<UnaryOperator>(expr)->getOpcode();
    if(UnaryOperator::getOpcodeStr(op) == "*") {
      //after deref, the type is int, so the original pointer is 1d
      //the value of a 1d pointer is a value
      if(expr->getType().getAsString() =="int") {
        void *ptr = mStack.back().getStmtPtr(expr);
        int val = *(int *)ptr;
        mStack.back().bindStmtVal(cast_expr, val);
      }
      //after deref, the type is int *, so the original pointer is 2d
      //the value of a 2d pointer is a 1 pointer
      else if (expr->getType().getAsString() == "int *") {
        void **ptr = (void **)mStack.back().getStmtPtr(expr);
        mStack.back().bindStmtPtr(cast_expr, *ptr);
      }
      else if(expr->getType().getAsString() == "int **") {
        //@TODO 3d pointer
      }
    }
  }
  else if (expr->getType()->isIntegerType() ) {
    int val = mStack.back().getStmtVal(expr);
    mStack.back().bindStmtVal(cast_expr, val );
  }
  //deal with pointer
  //no need to distinguish pointer and 2d pointer, as we return ptr->getPtr() only
  else if(expr->getType()->isPointerType()) {
    Pointer *ptr =  (Pointer *)mStack.back().getStmtPtr(expr);
    mStack.back().bindStmtPtr(cast_expr, ptr->getPtr());
  }
}

/*  deal with function call  */ 
void Environment::visitCallExpr(CallExpr * call_expr) {
  mStack.back().setPC(call_expr);
  int val = 0;
  FunctionDecl * callee = call_expr->getDirectCallee();
  if (callee == mInput) {                                  /*  INPUT */
   llvm::errs() << "Please Input an Integer Value : ";
   scanf("%d", &val);
   mStack.back().bindStmtVal(call_expr, val);
  } 
  else if (callee == mOutput) {                            /*  PRINT */
    Expr * decl = call_expr->getArg(0);
    val = mStack.back().getStmtVal(decl);
    llvm::errs() << val << "\n";
  } 
  else if (callee == mMalloc){                             /*  MALLOC */
    Expr *agr = call_expr->getArg(0);
    int size = mStack.back().getStmtVal(agr);
    void *ptr = malloc(size);
    mStack.back().bindStmtPtr(call_expr, ptr);
  } 
  else if (callee == mFree) {                              /*  FREE */
    Expr *agr = call_expr->getArg(0);
    if(!isa<CStyleCastExpr>(agr) ) {
      std::string name = dyn_cast<VarDecl>(agr->getReferencedDeclOfCallee())->getNameAsString();
      Pointer* pointer = mStack.back().getPointer(name);
      free(pointer->getPtr());
    }
    //FREE(int *)c);
    else {
      void *ptr = mStack.back().getStmtPtr(agr);
      free(ptr);
    }
  }
  else {                                                 /*  call my onw defined function */
    StackFrame stack;
    FunctionDecl::param_iterator pit = callee->param_begin();
    for (CallExpr::arg_iterator it = call_expr->arg_begin(), ie = call_expr->arg_end(); it != ie; ++it, ++pit) {
      int val = mStack.back().getStmtVal(*it);
      stack.bindDeclVal(*pit, val);
    }
    //current stack frame has to push back, since we will enter another function
    mStack.push_back(stack);
  }
}

/*   actions when visit ReturnStmt */
void Environment::visitReturnStmt(ReturnStmt *ret_stmt) {
  Expr *expr = ret_stmt->getRetValue();
  if(expr->getType()->isPointerType()) {                    /* return pointer */
    Pointer *pointer;
    std::string name = dyn_cast<VarDecl>(expr->getReferencedDeclOfCallee())->getNameAsString();
    pointer = mStack.back().getPointer(name);
    void *ptr = pointer->getPtr();
    mStack.back().bindStmtPtr(ret_stmt, ptr);
    mStack.pop_back();
    //main return , stack is empty, and no where and no need to store the val of stmt
    if (mStack.empty() ) {
      return;
    }
    Stmt *stmt = mStack.back().getPC();
    mStack.back().bindStmtPtr(stmt, ptr);
    return;
  }
  else {                                                    /*  return value */
    int val = -1;
    val = mStack.back().getStmtVal(expr);
    mStack.back().bindStmtVal(ret_stmt, val);
    mStack.pop_back();
    //main return , stack is empty, and no where and no need to store the val of stmt
    if (mStack.empty() ) {
      return;
    }
    Stmt *stmt = mStack.back().getPC();
    mStack.back().bindStmtVal(stmt, val);
  }
}

/*  actions when visit BinaryOperator */
void Environment::visitBinaryOperator(BinaryOperator *bop) {
  Expr * left = bop->getLHS();
  Expr * right = bop->getRHS();
  int op1 = -1, op2 = 0;
  bool is_array = false;
  bool is_pointer = false;
  bool is_deref_pointer = false;   //*a , a is a pointer
  bool is_2d_pointer_deref = false; //*c, c is a 2 demesion pointer;
  //deal with the left expr
  //can be ArraySubscriptExpr, pointer, 2d pointer, pointer deref, 2d pointer deref, var
  //ArraySubscriptExpr
  if (isa<ArraySubscriptExpr>(left) ) {
    is_array = true;
  }
  else if (dyn_cast<UnaryOperator>(left) ) {
    UnaryOperator::Opcode op = dyn_cast<UnaryOperator>(left)->getOpcode();
    if (UnaryOperator::getOpcodeStr(op) == "*") {
      Expr *sub_expr = dyn_cast<UnaryOperator>(left)->getSubExpr();
      if (sub_expr->getType().getAsString() == "int *") {
        is_deref_pointer = true;
      }
      else if (sub_expr->getType().getAsString() == "int **") {
        is_2d_pointer_deref = true;
      }
    }
  }
  //!!!!!!must judge behind UnaryOperator, because a 2d pointer deref is PointerType
  else if (left->getType()->isPointerType() ) {
    is_pointer = true;
  }
  else {
    op1 = mStack.back().getStmtVal(left);
  }
  //the right expr value
  //the tight can only be a pointeer
  //or a IntegerLiteral
  //or a ImplicitCastExpr
  ////the last two can be treated as stmt
  //assignment
  if (bop->getOpcodeStr() == "=") {
    if (is_2d_pointer_deref) {
      void **ptr_l = (void **)mStack.back().getStmtPtr(left);
      void *ptr_r = mStack.back().getStmtPtr(right);
      *ptr_l = ptr_r; 
    }
    else if (is_deref_pointer) {
      op2 = mStack.back().getStmtVal(right);
      void *ptr = mStack.back().getStmtPtr(left);
      *(int *)ptr = op2;
    }
    else if (is_pointer) {
      void *ptr;
      std::string name = dyn_cast<VarDecl>(left->getReferencedDeclOfCallee())->getNameAsString();
      Pointer *pointer = mStack.back().getPointer(name);
      ptr = mStack.back().getStmtPtr(right);
      pointer->setPtr(ptr);  
    }
    else if (is_array) {
      ArraySubscriptExpr *array = dyn_cast<ArraySubscriptExpr>(left); 
      Expr *base = array->getBase();
      Expr *idx = array->getIdx();
      std::string name = dyn_cast<VarDecl>(base->getReferencedDeclOfCallee())->getNameAsString();
      int index = mStack.back().getStmtVal(idx);
      Array temp_array = mStack.back().getArray(name);
      op1 = temp_array[index];
      op2 = mStack.back().getStmtVal(right);
      temp_array[index] = op2; 
    }
    else {
      op2 = mStack.back().getStmtVal(right);
      mStack.back().bindStmtVal(left, op2);
      //if the left is a var ref
      if (DeclRefExpr * declexpr = dyn_cast<DeclRefExpr>(left)) {
        Decl * decl = declexpr->getFoundDecl();
        mStack.back().bindDeclVal(decl, op2);
      }
    }
    return;
  }
  //add
  if (bop->getOpcodeStr() == "+") {
    //deal with ptr + 1
    bool left_is_pointer = left->getType()->isPointerType();
    bool right_is_pointer = right->getType()->isPointerType();
    if (left_is_pointer) {
      std::string name = dyn_cast<VarDecl>(left->getReferencedDeclOfCallee())->getNameAsString();
      Pointer *pointer = mStack.back().getPointer(name);
      op2 = mStack.back().getStmtVal(right);
      void *temp = (int *)pointer->getPtr() + op2;
      mStack.back().bindStmtPtr(bop, temp);
    }
    else if (right_is_pointer) {
      std::string name = dyn_cast<VarDecl>(right->getReferencedDeclOfCallee())->getNameAsString();
      Pointer *pointer = mStack.back().getPointer(name);
      op1 = mStack.back().getStmtVal(left);
      void *temp = (int *)pointer->getPtr() + op1;
      mStack.back().bindStmtPtr(bop, temp);
    }
    else {
      op1 = mStack.back().getStmtVal(left);
      op2 = mStack.back().getStmtVal(right);
      mStack.back().bindStmtVal(bop, op1 + op2);
    }
    return;
  }
  //@TODO deal wiht pointer
  op2 = mStack.back().getStmtVal(right);
  //sub
  if (bop->getOpcodeStr() == "-") {
    mStack.back().bindStmtVal(bop, op1 -op2);
  }
  //mutiple
  else if (bop->getOpcodeStr() == "*") {
    mStack.back().bindStmtVal(bop, op1 * op2);
  }
  //equal
  else if (bop->isComparisonOp()) {
    //"=" or "<" ">" ....
    if (bop->getOpcodeStr() == "==") {
      if (op1 == op2) 
        mStack.back().bindStmtVal(bop, 1);
      else 
        mStack.back().bindStmtVal(bop, 0);
    }
    if (bop->getOpcodeStr() == ">") {
      if (op1 > op2) 
        mStack.back().bindStmtVal(bop, 1);
      else
        mStack.back().bindStmtVal(bop, 0);
    }
    if (bop->getOpcodeStr() == "<") {
      if (op1 < op2)
        mStack.back().bindStmtVal(bop, 1);
      else
        mStack.back().bindStmtVal(bop, 0);
    }
  }
}

/*  actions when visit UnaryOperator */
void Environment::visitUnaryOperator(UnaryOperator *uop) {
  UnaryOperator::Opcode op = uop->getOpcode();
  Expr *expr = uop->getSubExpr();
  if (UnaryOperator::getOpcodeStr(op) == "-") {
    int value = mStack.back().getStmtVal(expr);
    mStack.back().bindStmtVal(uop, -value);
  }
  else if (UnaryOperator::getOpcodeStr(op) == "*") {
    void *ptr = mStack.back().getStmtPtr(expr);  
    mStack.back().bindStmtPtr(uop, ptr);
  }
  //@TODO other unary operators: ++, --, & , ....., but it is the same as above
  //and it is just a problem of time
}

/*  actions when visit sizeof */
void Environment::visitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *expr) {
  std::string type = expr->getArgumentType().getAsString();
  if (type == "int") {
    mStack.back().bindStmtVal(expr, sizeof(int));
  }
  else if(type == "int *") {
    mStack.back().bindStmtVal(expr, sizeof(int *));
  }
  //@TODO other type to support
}

/*  actions when visit CStyleCastExpr */
void Environment::visitCStyleCastExpr(CStyleCastExpr *c_style_cast_expr) {
  Expr *sub_expr = c_style_cast_expr->getSubExpr();
  if (sub_expr->getType().getAsString() == "int *" ) {
    void *ptr = mStack.back().getStmtPtr(sub_expr);
    mStack.back().bindStmtPtr(c_style_cast_expr, ptr);
  }
  else if(sub_expr->getType().getAsString() == "int **") {
    std::string name = dyn_cast<VarDecl>(sub_expr->getReferencedDeclOfCallee())->getNameAsString();
    Pointer *ptr = mStack.back().getPointer(name);
    mStack.back().bindStmtPtr(c_style_cast_expr, ptr->getPtr()); 
  }
}

/*  actions when visit ArraySubscriptExpr */
void Environment::visitArraySubscriptExpr(ArraySubscriptExpr *array) {
  Expr *base = array->getBase();
  Expr *idx = array->getIdx();
  std::string name = dyn_cast<VarDecl>(base->getReferencedDeclOfCallee())->getNameAsString();
  int index = mStack.back().getStmtVal(idx);
  Array parray = mStack.back().getArray(name);
  mStack.back().bindStmtVal(array, parray[index]);
}

/*  actions when visit ParenExpr */
void Environment::visitParenExpr(ParenExpr *paren_expr) {
  if (paren_expr->getType()->isPointerType() ) {   /* the paren expr is pointer type */
    Expr *expr = paren_expr->getSubExpr();
    void *ptr = mStack.back().getStmtPtr(expr);
    mStack.back().bindStmtPtr(paren_expr, ptr);
  }
  else if(paren_expr->getType()->isIntegerType() ) { 
    Expr *expr = paren_expr->getSubExpr();
    int val = mStack.back().getStmtVal(expr);
    mStack.back().bindStmtVal(paren_expr, val);
  }
}

/*  actions when visit IntegerLiteral */
void Environment::visitIntegerLiteral(IntegerLiteral *my_int) {
  int val = my_int->getValue().getSExtValue();
  mStack.back().bindStmtVal(my_int, val);
}
