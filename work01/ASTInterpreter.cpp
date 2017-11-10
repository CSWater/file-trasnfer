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
    if (BinaryOperator *bop = dyn_cast<BinaryOperator>(stmt) ) {     /* BinaryOperator */
      EvaluatedExprVisitor<InterpreterVisitor>::VisitStmt(bop);
      mEnv->visitBinaryOperator(bop);
    }
    else if (CastExpr *cast_expr = dyn_cast<CastExpr>(stmt) ) {
      EvaluatedExprVisitor<InterpreterVisitor>::VisitStmt(cast_expr);
      mEnv->visitCastExpr(cast_expr);
    }
    else {
      EvaluatedExprVisitor<InterpreterVisitor>::VisitStmt(stmt);
    }
  }
  //actions when visit binary operators
  virtual void VisitBinaryOperator (BinaryOperator * bop) {
	  VisitStmt(bop);
  }
 
  //actions when visit unary operators
  virtual void VisitUnaryOperator (UnaryOperator *uop) {
    VisitStmt(uop);
    mEnv->visitUnaryOperator(uop);
  }

  //actions when visit array expr
  virtual void VisitArraySubscriptExpr(ArraySubscriptExpr *array) {
    VisitStmt(array);
    mEnv->visitArraySubscriptExpr(array);
  }

  /*  funtions when visit a declared variable or function */ 
  virtual void VisitDeclRefExpr(DeclRefExpr * decl_ref) {
	  VisitStmt(decl_ref);
    mEnv->visitDeclRefExpr(decl_ref);
  }

  /*  actions when visit cast expr */ 
  virtual void VisitCastExpr(CastExpr * cast_expr) {
	  VisitStmt(cast_expr);
    //mEnv->visitCastExpr(cast_expr);
  }

  /*  actions when visit paren expr */ 
  virtual void VisitParenExpr(ParenExpr *paren_expr) {
    VisitStmt(paren_expr);
    mEnv->visitParenExpr(paren_expr);
  }

  /*  actions when visit c_style_cast_expr */
  virtual void VisitCStyleCastExpr(CStyleCastExpr *c_style_cast_expr) {
    VisitStmt(c_style_cast_expr);
    mEnv->visitCStyleCastExpr(c_style_cast_expr);
  }

  /* actions when visit IntegerLiteral */
  virtual void VisitIntegerLiteral(IntegerLiteral *my_int) {
    mEnv->visitIntegerLiteral(my_int);
  }

  /*  actions when visit DeclStmt */ 
  virtual void VisitDeclStmt(DeclStmt * decl_stmt) {
    VisitStmt(decl_stmt);
    mEnv->visitDeclStmt(decl_stmt);
  }

  /* actions when visit UnaryOperator */
  virtual void VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *expr) {
    mEnv->visitUnaryExprOrTypeTraitExpr(expr);
  }

  /*  actions when visit ReturnStmt */ 
  virtual void VisitReturnStmt(ReturnStmt *ret_stmt) {
    VisitStmt(ret_stmt);
    mEnv->visitReturnStmt(ret_stmt);
  }

  /*  actions when visit IfStmt */ 
  virtual void VisitIfStmt(IfStmt *if_stmt) {
    Stmt *cond = dyn_cast<BinaryOperator>(if_stmt->getCond());
    Stmt *then_clause = if_stmt->getThen();
    Stmt *else_clause = if_stmt->getElse();
    VisitStmt(cond);
    if (else_clause != NULL) {                /* has else clause */
      if (mEnv->check(cond)) {
        VisitStmt(then_clause);
      }
      else {
        VisitStmt(else_clause);
      }
    }
    else {                                   /*  has no else clause */ 
      if (mEnv->check(cond)) {
        VisitStmt(then_clause);
      }
    }
  }

  /*  actions when visit CallExpr */ 
  virtual void VisitCallExpr(CallExpr *call) {
    VisitStmt(call);
    mEnv->visitCallExpr(call);
    //get the function body
    FunctionDecl *callee = call->getDirectCallee();
    Stmt *body = callee->getBody();
    if(body && isa<CompoundStmt>(body) ) {
      VisitStmt(body);
    }
  }
  
  /*  actions when visit WhileStmt */ 
  virtual void VisitWhileStmt(WhileStmt *while_stmt) {
    //get while block
    Stmt *while_body = while_stmt->getBody();
    BinaryOperator *cond = NULL;
    cond = dyn_cast<BinaryOperator>(while_stmt->getCond());
    VisitStmt(cond);
    mEnv->visitBinaryOperator(cond);
    while (mEnv->check(cond)) {
       VisitStmt(while_body);
       VisitStmt(cond);
       mEnv->visitBinaryOperator(cond);
    }
  }

  /*  actions when visit ForStmt */ 
  virtual void VisitForStmt(ForStmt *for_stmt) {
    Stmt *for_body = for_stmt->getBody();
    Stmt *cond = for_stmt->getCond();
    Stmt *init = for_stmt->getInit();
    Expr *inc = for_stmt->getInc();
    //@TODO for( ; cond; inc)
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
