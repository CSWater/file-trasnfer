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

class StackFrame {
   /// StackFrame maps Variable Declaration to Value
   /// Which are either integer or addresses (also represented using an Integer value)
   std::map<Decl*, int> mVars;
   std::map<Stmt*, int> mExprs;
   /// The current stmt
   Stmt * mPC;
public:
   StackFrame() : mVars(), mExprs(), mPC() {
   }

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
   void setPC(Stmt * stmt) {
	   mPC = stmt;
   }
   Stmt * getPC() {
	   return mPC;
   }
};

/// Heap maps address to a value
/*
class Heap {
public:
   int Malloc(int size) ;
   void Free (int addr) ;
   void Update(int addr, int val) ;
   int get(int addr);
};
*/

class Environment {
   std::vector<StackFrame> mStack;

   FunctionDecl * mFree;				/// Declartions to the built-in functions
   FunctionDecl * mMalloc;
   FunctionDecl * mInput;
   FunctionDecl * mOutput;

   FunctionDecl * mEntry;
   ASTContext *EnvmContext;
public:
   /// Get the declartions to the built-in functions
   Environment() : mStack(), mFree(NULL), mMalloc(NULL), mInput(NULL), mOutput(NULL), mEntry(NULL), EnvmContext(NULL) {
   }


   /// Initialize the Environment
   //void init(TranslationUnitDecl * unit) {
   void init(clang::ASTContext &Context) {
     EnvmContext = &Context;
     TranslationUnitDecl *unit = Context.getTranslationUnitDecl();
	   mStack.push_back(StackFrame());
	   for (TranslationUnitDecl::decl_iterator i =unit->decls_begin(), e = unit->decls_end(); i != e; ++ i) {
       //deal with funtion declaration
		   if (FunctionDecl * fdecl = dyn_cast<FunctionDecl>(*i) ) {
			   if (fdecl->getName().equals("FREE")) {
           llvm::outs() << fdecl->getName() << "\n";
           mFree = fdecl;
         }
			   else if (fdecl->getName().equals("MALLOC")) {
           llvm::outs() << fdecl->getName() << "\n";
           mMalloc = fdecl;
         }
			   else if (fdecl->getName().equals("GET")) {
           llvm::outs() << fdecl->getName() << "\n";
           mInput = fdecl;
         }
			   else if (fdecl->getName().equals("PRINT")){
           llvm::outs() << fdecl->getName() << "\n";
           mOutput = fdecl;
         }
			   else if (fdecl->getName().equals("main")) mEntry = fdecl;
		   }
       //deal with global variables
       else if (VarDecl *gdecl = dyn_cast<VarDecl>(*i)) {
           Expr *init = gdecl->getInit();
           llvm::APInt gvar = dyn_cast<IntegerLiteral>(init)->getValue();
           int value = gvar.getSExtValue();
           mStack.back().bindDecl(gdecl, value);
       }
	   }
   }

   FunctionDecl * getEntry() {
	   return mEntry;
   }

   /// !TODO Support comparison operation
   //binary operators
   void binop(BinaryOperator *bop) {
	   Expr * left = bop->getLHS();
	   Expr * right = bop->getRHS();
     //assignment
	   //if (bop->isAssignmentOp()) {
     if (bop->getOpcodeStr() == "=") {
       if(right->isRValue()) {
         llvm::APInt var = dyn_cast<IntegerLiteral>(right)->getValue();
         int value = var.getSExtValue(); 
		     mStack.back().bindStmt(left, value);
         //if the var is global, update its value
		     if (DeclRefExpr * declexpr = dyn_cast<DeclRefExpr>(left)) {
			     Decl * decl = declexpr->getFoundDecl();
			     mStack.back().bindDecl(decl, value);
		     }
       }
       else {
         int value = mStack.back().getStmtVal(right);
         mStack.back().bindStmt(left, value);
       }
	   }
     //add
     else if (bop->getOpcodeStr() == "+") {
       Expr *left = bop->getLHS();
       Expr *right = bop->getRHS();
       int op1 = mStack.back().getStmtVal(left);
       int op2 = mStack.back().getStmtVal(right);
       int result = op1 + op2;
       mStack.back().bindStmt(bop, result);

     }
     //sub
     else if (bop->getOpcodeStr() == "-") {
       Expr *left = bop->getLHS();
       Expr *right = bop->getRHS();
       int op1 = 0, op2 = 0;
       llvm::outs() << "begin\n";
       if(left->isRValue())
         op1 = dyn_cast<IntegerLiteral>(left)->getValue().getSExtValue();
       else
         op1 = mStack.back().getStmtVal(left);
       if(right->isRValue()) {
         llvm::outs() << "left\n";
         op2 = dyn_cast<IntegerLiteral>(right)->getValue().getSExtValue();
       }
       else
         op2 = mStack.back().getStmtVal(right);
       int result = op1 - op2;
       llvm::outs() << result << "\n";
       mStack.back().bindStmt(bop, result);
     }
     //mutiple
     else if (bop->getOpcodeStr() == "*") {
        Expr *left = bop->getLHS();
        Expr *right = bop->getRHS();
        int op1 = mStack.back().getStmtVal(left);
        int op2 = mStack.back().getStmtVal(right);
        int result = op1 * op2;
        mStack.back().bindStmt(bop, result);
     }
   }

   void decl(DeclStmt * declstmt) {
	   for (DeclStmt::decl_iterator it = declstmt->decl_begin(), ie = declstmt->decl_end();
			   it != ie; ++ it) {
		   Decl * decl = *it;
		   if (VarDecl * vardecl = dyn_cast<VarDecl>(decl)) {
			  // mStack.back().bindDecl(vardecl, 0);
        int value = 0;
        if(vardecl->hasInit()) {
          Expr *init = vardecl->getInit();
          llvm::APInt var = dyn_cast<IntegerLiteral>(init)->getValue();
          value = var.getSExtValue();
        }
        mStack.back().bindDecl(vardecl, value);
		   }
	   }
   }

   void declref(DeclRefExpr * declref) {
	   mStack.back().setPC(declref);
	   if (declref->getType()->isIntegerType()) {
		   Decl* decl = declref->getFoundDecl();

		   int val = mStack.back().getDeclVal(decl);
		   mStack.back().bindStmt(declref, val);
	   }
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
	   } else if (callee == mOutput) {
		   Expr * decl = callexpr->getArg(0);
		   val = mStack.back().getStmtVal(decl);
		   llvm::errs() << val << "\n";
	   } else {
		   /// You could add your code here for Function call Return
	   }
   }
};


