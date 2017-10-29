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

class Pointer {
private:
  void *ptr;
  int type;

public:
  Pointer():ptr(NULL), type(-1) {}
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
};

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
  int operator[](int idx) {
    return *(int *)(this->getPtr() + 4 * idx);
  }
};

class StackFrame {
   /// StackFrame maps Variable Declaration to Value
   /// Which are either integer or addresses (also represented using an Integer value)
   std::map<Decl*, int> mVars;
   std::map<Stmt*, int> mExprs;
   //store array
   std::map<std::string, Array> mArray;
   /// The current stmt
   Stmt * mPC;
public:
   StackFrame() : mVars(), mExprs(), mPC() {}

   void bindDecl(Decl* decl, int val) {
      mVars[decl] = val;
   }    
   int getDeclVal(Decl * decl) {
      assert (mVars.find(decl) != mVars.end());
      return mVars.find(decl)->second;
   }
   void bindStmt(Stmt * stmt, int val) {
	   mExprs[stmt] = val;
   }
   int getStmtVal(Stmt * stmt) {
	   assert (mExprs.find(stmt) != mExprs.end());
	   return mExprs[stmt];
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
   std::vector<StackFrame> mStack;

   FunctionDecl * mFree;				/// Declartions to the built-in functions
   FunctionDecl * mMalloc;
   FunctionDecl * mInput;
   FunctionDecl * mOutput;
   FunctionDecl * mEntry;
public:
   /// Get the declartions to the built-in functions
   Environment() : mStack(), mFree(NULL), mMalloc(NULL), mInput(NULL), mOutput(NULL), mEntry(NULL) {}

   /// Initialize the Environment
   void init(TranslationUnitDecl * unit) {
	   mStack.push_back(StackFrame());
	   for (TranslationUnitDecl::decl_iterator i =unit->decls_begin(), e = unit->decls_end(); i != e; ++ i) {
       //deal with funtion declaration
		   if (FunctionDecl * fdecl = dyn_cast<FunctionDecl>(*i) ) {
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
       //deal with global variables
       else if (VarDecl *global_decl = dyn_cast<VarDecl>(*i)) {
           //default initialized as 0
           int value = 0;
           if (global_decl->hasInit()) {
             Expr *init = global_decl->getInit();
             llvm::APInt g_var = dyn_cast<IntegerLiteral>(init)->getValue();
             value = g_var.getSExtValue();
           }
           mStack.back().bindDecl(global_decl, value);
       }
	   }
   }

   FunctionDecl * getEntry() {
	   return mEntry;
   }

   //unary operators
   void unaryOp(UnaryOperator *uop) {
     UnaryOperator::Opcode op = uop->getOpcode();
     Expr *expr = uop->getSubExpr();
     int value = 0;
     //if (IntegerLiteral *num = dyn_cast<IntegerLiteral>(expr)) {
     //  value = num->getValue().getSExtValue();
     //}
     //else {
       value = mStack.back().getStmtVal(expr);
     //}
     if (UnaryOperator::getOpcodeStr(op) == "-") {
       mStack.back().bindStmt(uop, -value);
     }
   }

   /// !TODO Support comparison operation
   //binary operators
   void binaryOp(BinaryOperator *bop) {
	   Expr * left = bop->getLHS();
	   Expr * right = bop->getRHS();
     int op1 = -1, op2 = 0;
     int *temp = NULL;
     //the left expr value
     if (ArraySubscriptExpr *array = dyn_cast<ArraySubscriptExpr>(left) ) {
       Expr *base = array->getBase();
       Expr *idx = array->getIdx();
       std::string name = dyn_cast<VarDecl>(base->getReferencedDeclOfCallee())->getNameAsString();
       int index = - 1;
       //if (IntegerLiteral *index_literal = dyn_cast<IntegerLiteral>(idx) ) {
       //  llvm::outs() << "literal\n";
       //  index = index_literal->getValue().getSExtValue();
       //}
       //else {
         index = mStack.back().getStmtVal(idx);
       //}
       Array ptr = mStack.back().getArray(name);
       //op1 = ptr[index];
       temp = (int *)((int *)ptr.getPtr() + index);
       op1 = *temp;
       llvm::outs() << op1 << "\n";
       llvm::outs() << "success!!!!!\n";
     }
     else {
       op1 = mStack.back().getStmtVal(left);
     }
     //the right expr value
     //the right can only be two types:IntegerLiteral or ImplicitCastExpr
     //both are treated as stmt
     op2 = mStack.back().getStmtVal(right);
     //assignment
     if (bop->getOpcodeStr() == "=") {
       if (ArraySubscriptExpr *array = dyn_cast<ArraySubscriptExpr>(left) ) {
         *temp = op2; 
       }
       else {
		     mStack.back().bindStmt(left, op2);
         //if the left is a var ref
		     if (DeclRefExpr * declexpr = dyn_cast<DeclRefExpr>(left)) {
		       Decl * decl = declexpr->getFoundDecl();
		       mStack.back().bindDecl(decl, op2);
		     }
       }
	   }
     //add
     else if (bop->getOpcodeStr() == "+") {
       mStack.back().bindStmt(bop, op1 + op2);
     }
     //sub
     else if (bop->getOpcodeStr() == "-") {
       mStack.back().bindStmt(bop, op1 -op2);
     }
     //mutiple
     else if (bop->getOpcodeStr() == "*") {
       mStack.back().bindStmt(bop, op1 * op2);
     }
     //equal
     else if (bop->isComparisonOp()) {
       //"=" or "<" ">" ....
       if (bop->getOpcodeStr() == "==") {
         if (op1 == op2) 
           mStack.back().bindStmt(bop, 1);
         else 
           mStack.back().bindStmt(bop, 0);
       }
       if (bop->getOpcodeStr() == ">") {
         if (op1 > op2)
           mStack.back().bindStmt(bop, 1);
         else
           mStack.back().bindStmt(bop, 0);
       }
       if (bop->getOpcodeStr() == "<") {
         if (op1 < op2)
           mStack.back().bindStmt(bop, 1);
         else
           mStack.back().bindStmt(bop, 0);
       }
     }
   }

   void declStmt(DeclStmt * declstmt) {
	   for (DeclStmt::decl_iterator it = declstmt->decl_begin(), ie = declstmt->decl_end();
		     it != ie; ++ it) {
		   Decl * decl = *it;
		   if (VarDecl * var_decl = dyn_cast<VarDecl>(decl)) {
        if (var_decl->getType()->isArrayType()) {
          //get the name of array
          std::string name = var_decl->getNameAsString();
          //get the length of array
          int len = dyn_cast<ConstantArrayType>(var_decl->getType())->getSize().getSExtValue();
          //check if has init
          if(var_decl->hasInit()) {
            //@TODO init
          }
          else {
            void *base = malloc(len * sizeof(int));
            Array array;
            array.setPtr(base);
            //1 means int
            array.setType(1);
            array.setLength(len);
            mStack.back().bindArray(name, array);
          }
        }
        else {
          int value = 0;
          if(var_decl->hasInit()) {
            Expr *init = var_decl->getInit();
            //llvm::APInt var = dyn_cast<IntegerLiteral>(init)->getValue();
            //value = var.getSExtValue();
            value = mStack.back().getStmtVal(init);
          }
          mStack.back().bindDecl(var_decl, value);
        }
		   }
	   }
   }
   
   //
   void varDecl(VarDecl *var_decl) {
     Expr *init = var_decl->getInit();
     int val = mStack.back().getStmtVal(init);
     mStack.back().bindDecl(var_decl, val);
   }

   //
   void integer(IntegerLiteral *my_int) {
     int val = my_int->getValue().getSExtValue();
     mStack.back().bindStmt(my_int, val);
   }
   //check condition
   int check(BinaryOperator *cond) {
     return mStack.back().getStmtVal(cond);
   }

   void declRef(DeclRefExpr * decl_ref) {
	   mStack.back().setPC(decl_ref);
	   if (decl_ref->getType()->isIntegerType()) {
		   Decl* decl = decl_ref->getFoundDecl();

		   int val = mStack.back().getDeclVal(decl);
		   mStack.back().bindStmt(decl_ref, val);
	   }
   }

   void visitArray(ArraySubscriptExpr *array) {
     Expr *base = array->getBase();
     Expr *idx = array->getIdx();
     std::string name = dyn_cast<VarDecl>(base->getReferencedDeclOfCallee())->getNameAsString();
     //int index = dyn_cast<IntegerLiteral>(idx)->getValue().getSExtValue();
     int index = mStack.back().getStmtVal(idx);
     Array ptr = mStack.back().getArray(name);
     int val = *(int *)((int *)ptr.getPtr() + index);
     mStack.back().bindStmt(array, val);
   }

   void cast(CastExpr * castexpr) {
	   mStack.back().setPC(castexpr);
	   if (castexpr->getType()->isIntegerType()) {
		   Expr * expr = castexpr->getSubExpr();
		   int val = mStack.back().getStmtVal(expr);
		   mStack.back().bindStmt(castexpr, val );
	   }
   }

   /// !TODO Support Function Call
   void call(CallExpr * callexpr) {
	   mStack.back().setPC(callexpr);
	   int val = 0;
	   FunctionDecl * callee = callexpr->getDirectCallee();
	   if (callee == mInput) {
		  llvm::errs() << "Please Input an Integer Value : ";
		  scanf("%d", &val);
		  mStack.back().bindStmt(callexpr, val);
	   } 
     else if (callee == mOutput) {
		   Expr * decl = callexpr->getArg(0);
		   val = mStack.back().getStmtVal(decl);
		   llvm::errs() << val << "\n";
	   } 
     else if (callee == mMalloc){
       //Expr *agr = callexpr->getArg(0);
	   } 
     else { //call my own defined function
       StackFrame stack;
       FunctionDecl::param_iterator pit = callee->param_begin();
       for (CallExpr::arg_iterator it = callexpr->arg_begin(), ie = callexpr->arg_end(); it != ie; ++it, ++pit) {
         int val = mStack.back().getStmtVal(*it);
         stack.bindDecl(*pit, val);
       }
       mStack.push_back(stack);
     }
   }

   //return
   void ret(ReturnStmt *ret_stmt) {
     Expr *expr = ret_stmt->getRetValue();
     expr->dump();
     int val = -1;
     val = mStack.back().getStmtVal(expr);
     mStack.back().bindStmt(ret_stmt, val);
     llvm::outs() << "ret : " << val << "\n";
     mStack.pop_back();
     //main return , stack is empty, and no where and no need to store the val of stmt
     if (mStack.empty() ) {
       return;
     }
     Stmt *stmt = mStack.back().getPC();
     mStack.back().bindStmt(stmt, val);
   }
};


