//==--- tools/clang-check/ClangInterpreter.cpp - Clang Interpreter tool --------------===//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

using namespace clang;

#include "Environment.h"

class InterpreterVisitor : 
   public EvaluatedExprVisitor<InterpreterVisitor> {
public:
   explicit InterpreterVisitor(const ASTContext &context, Environment * env)
   : EvaluatedExprVisitor(context), mEnv(env) {}
   virtual ~InterpreterVisitor() {}

   virtual void VisitStmt(Stmt *stmt) {
     if (isa<BinaryOperator>(stmt) ) {
       BinaryOperator *bop = dyn_cast<BinaryOperator>(stmt);
       EvaluatedExprVisitor<InterpreterVisitor>::VisitStmt(bop);
       mEnv->binaryOp(bop);
     }
     else if (isa<DeclStmt>(stmt) ) {
       DeclStmt *decl = dyn_cast<DeclStmt>(stmt);
       EvaluatedExprVisitor<InterpreterVisitor>::VisitStmt(decl);
       mEnv->declStmt(decl);
     }
     else {
       EvaluatedExprVisitor<InterpreterVisitor>::VisitStmt(stmt);
     }
   }
   //actions when visit binary operators
   virtual void VisitBinaryOperator (BinaryOperator * bop) {
	   VisitStmt(bop);
	   //mEnv->binaryOp(bop);
   }
 
   //actions when visit unary operators
   virtual void VisitUnaryOperator (UnaryOperator *uop) {
     VisitStmt(uop);
     mEnv->unaryOp(uop);
   }

   //actions when visit array expr
   virtual void VisitArraySubscriptExpr(ArraySubscriptExpr *array) {
     VisitStmt(array);
     mEnv->visitArray(array);
   }

   //funtions when visit a declared variable or function
   virtual void VisitDeclRefExpr(DeclRefExpr * expr) {
	   VisitStmt(expr);
	   mEnv->declRef(expr);
   }

   //actions when visit cast expr
   virtual void VisitCastExpr(CastExpr * expr) {
	   VisitStmt(expr);
	   mEnv->cast(expr);
   }


   //actions when visit declaration stmt
   virtual void VisitDeclStmt(DeclStmt * declstmt) {
     llvm::outs() << "decl\n";
     VisitStmt(declstmt);
	   //mEnv->declStmt(declstmt);
   }

   //actions when visit if stmt
   virtual void VisitIfStmt(IfStmt *if_stmt) {
     BinaryOperator *cond = dyn_cast<BinaryOperator>(if_stmt->getCond());
     Stmt *then_clause = if_stmt->getThen();
     Stmt *else_clause = if_stmt->getElse();
     VisitStmt(cond);
     mEnv->binaryOp(cond);
     //has else stmt
     if (else_clause != NULL) {
       if (mEnv->check(cond)) {
         VisitStmt(then_clause);
       }
       else {
         VisitStmt(else_clause);
       }
     }
     else { //has no else clause
       if (mEnv->check(cond)) 
         VisitStmt(then_clause);
     }
   }

   //
   virtual void VisitIntegerLiteral(IntegerLiteral *my_int) {
     mEnv->integer(my_int);
   }

   virtual void VisitCallExpr(CallExpr *call) {
     VisitStmt(call);
     mEnv->call(call);
     //get the function body
     FunctionDecl *callee = call->getDirectCallee();
     Stmt *body = callee->getBody();
     if(body && isa<CompoundStmt>(body) ) {
       llvm::outs() << "body\n";
       VisitStmt(body);
     }
   }

   virtual void VisitReturnStmt(ReturnStmt *ret_stmt) {
     VisitStmt(ret_stmt);
     mEnv->ret(ret_stmt);
   }
   
   //actions when visit while stmt
   virtual void VisitWhileStmt(WhileStmt *while_stmt) {
     //get while block
     Stmt *while_body = while_stmt->getBody();
     BinaryOperator *cond = NULL;
     cond = dyn_cast<BinaryOperator>(while_stmt->getCond());
     VisitStmt(cond);
     mEnv->binaryOp(cond);
     while (mEnv->check(cond)) {
        VisitStmt(while_body);
        VisitStmt(cond);
        mEnv->binaryOp(cond);
     }
   }

   //actions when visit for stmt
   virtual void VisitForStmt(ForStmt *for_stmt) {
     //get for block
     Stmt* for_body = for_stmt->getBody();
     BinaryOperator *cond = NULL;
     cond = dyn_cast<BinaryOperator>(for_stmt->getCond());
     Stmt *init = for_stmt->getInit();
     Expr *inc = for_stmt->getInc();
     //@TODO for( ; cond; inc)
     //for(; ; inc)
     //for(; ; ;)
     //.....
     //if init is not empty, VisitStmt(init)
     if (init && cond && inc) {
       for (VisitStmt(init) ; VisitStmt(cond), mEnv->check(cond); VisitStmt(inc)) {
         VisitStmt(for_body);
       }
     }
     else if (cond && inc) {
       for ( ; VisitStmt(cond), mEnv->check(cond); VisitStmt(inc) ) {
         VisitStmt(for_body);
       }
     }
   }

private:
   Environment * mEnv;
};

class InterpreterConsumer : public ASTConsumer {
public:
   explicit InterpreterConsumer(const ASTContext& context) : mEnv(),
   	   mVisitor(context, &mEnv) {
   }
   virtual ~InterpreterConsumer() {}

   virtual void HandleTranslationUnit(clang::ASTContext &Context) {
	   TranslationUnitDecl * decl = Context.getTranslationUnitDecl();
	   mEnv.init(decl);

	   FunctionDecl * entry = mEnv.getEntry();
	   mVisitor.VisitStmt(entry->getBody());
  }
private:
   Environment mEnv;
   InterpreterVisitor mVisitor;
};

class InterpreterClassAction : public ASTFrontendAction {
public: 
  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
    clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    return std::unique_ptr<clang::ASTConsumer>(
        new InterpreterConsumer(Compiler.getASTContext()));
  }
};

int main (int argc, char ** argv) {
   if (argc > 1) {
       clang::tooling::runToolOnCode(new InterpreterClassAction, argv[1]);
   }
}
