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

   //actions when visit binary operators
   virtual void VisitBinaryOperator (BinaryOperator * bop) {
     llvm::outs() << "hello VisitBinaryOperator!\n";
	   VisitStmt(bop);
	   mEnv->binaryOp(bop);
   }
 
   //actions when visit unary operators
   virtual void VisitUnaryOperator (UnaryOperator *uop) {
     llvm::outs() << "hello VisitUnaryOperator!\n";
     VisitStmt(uop);
     mEnv->unaryOp(uop);

   }

   //funtions when visit a declared variable or function
   virtual void VisitDeclRefExpr(DeclRefExpr * expr) {
     llvm::outs() << "hello VisitDeclRefExpr!\n";
	   VisitStmt(expr);
	   mEnv->declref(expr);
   }

   //actions when visit cast expr
   virtual void VisitCastExpr(CastExpr * expr) {
     llvm::outs() << "hello VisitCastExpr!\n";
	   VisitStmt(expr);
	   mEnv->cast(expr);
   }

   //actions when visit function call expr
   virtual void VisitCallExpr(CallExpr * call) {
     llvm::outs() << "hello VisitCallExpr!\n";
	   VisitStmt(call);
	   mEnv->call(call);
   }

   //actions when visit declaration stmt
   virtual void VisitDeclStmt(DeclStmt * declstmt) {
     llvm::outs() << "hello VisitDeclStmt!\n";
	   mEnv->declStmt(declstmt);
   }

   //actions when visit if stmt
   virtual void VisitIfStmt(IfStmt *if_stmt) {
     llvm::outs() << "hello VisitIfStmt\n";
     BinaryOperator *cond = dyn_cast<BinaryOperator>(if_stmt->getCond());
     //cond->dump();
     VisitStmt(cond);
     mEnv->binaryOp(cond);
     if (mEnv->check(cond)) {
       Stmt *then_clause = if_stmt->getThen();
       VisitStmt(then_clause);
     }
     else {
       Stmt *else_clause = if_stmt->getElse();
       VisitStmt(else_clause);
     }
   }

   //actions when visit while stmt
   virtual void VisitWhileStmt(WhileStmt *while_stmt) {
     llvm::outs() << "hello VisitWhileStmt\n";
     BinaryOperator *cond = NULL;
     cond = dyn_cast<BinaryOperator>(while_stmt->getCond());
     VisitStmt(cond);
     mEnv->binaryOp(cond);
     while (mEnv->check(cond)) {
        Stmt *while_body = while_stmt->getBody();
        VisitStmt(while_body);
        VisitStmt(cond);
        mEnv->binaryOp(cond);
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
