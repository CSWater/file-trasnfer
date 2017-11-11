//===- Hello.cpp - Example code from "Writing an LLVM Pass" ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements two versions of the LLVM "Hello World" pass described
// in docs/WritingAnLLVMPass.html
//
//===----------------------------------------------------------------------===//

#include <llvm/Support/CommandLine.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/ToolOutputFile.h>

#include <llvm/Transforms/Scalar.h>

#include <llvm/IR/Function.h>
#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/User.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Metadata.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/InstIterator.h"

#if LLVM_VERSION_MAJOR >= 4
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>

#else
#include <llvm/Bitcode/ReaderWriter.h>
#endif
using namespace llvm;
#if LLVM_VERSION_MAJOR >= 4
static ManagedStatic<LLVMContext> GlobalContext;
static LLVMContext &getGlobalContext() { return *GlobalContext; }
#endif
/* In LLVM 5.0, when  -O0 passed to clang , the functions generated with clang will
 * have optnone attribute which would lead to some transform passes disabled, like mem2reg.
 */
#if LLVM_VERSION_MAJOR == 5
struct EnableFunctionOptPass: public FunctionPass {
    static char ID;
    EnableFunctionOptPass():FunctionPass(ID){}
    bool runOnFunction(Function & F) override{
        if(F.hasFnAttribute(Attribute::OptimizeNone))
        {
            F.removeFnAttr(Attribute::OptimizeNone);
        }
        return true;
    }
};

char EnableFunctionOptPass::ID=0;
#endif

	
///!TODO TO BE COMPLETED BY YOU FOR ASSIGNMENT 2
///Updated 11/10/2017 by fargo: make all functions
///processed by mem2reg before this pass.
struct FuncPtrPass : public ModulePass {
  static char ID; // Pass identification, replacement for typeid
  FuncPtrPass() : ModulePass(ID) {}


void dealWithIndirectFptr(Value* val){
    if(Argument* argument = dyn_cast<Argument>(val))
      dealWithArgument(argument);
    if(CallInst* call_inst = dyn_cast<CallInst>(val))
      dealWithCallInst(call_inst);
    if(PHINode* phi_node = dyn_cast<PHINode>(val))
      dealWithPhinode(phi_node);
  }


  void dealWithPhinode(PHINode* phi_node){
  	errs() <<"hello phinode\n";
  	unsigned num = phi_node->getNumIncomingValues();
	for (int i = 0; i < num; ++i) {
		Value *temp = phi_node->getIncomingValue(i);
		if(dyn_cast<PHINode>(temp))
			dealWithPhinode(dyn_cast<PHINode>(temp));
		errs() << temp->getName()<<'\n';
	}
  }

  void dealWithArgument(Argument* argument){
  	errs() <<"hello argument\n";
    unsigned index = argument->getArgNo();
    Function* parent_func = argument->getParent();
    /*Value::user_iterator user_i = parent_func->user_begin();
    for(;user_i != parent_func->user_end();++user_i){
    	//if(Function* func = dyn_cast)
    	(dyn_cast<Value>(user_i)).dump();
    }*/
    for (User * U:parent_func->users ()){
    	U->dump();
    }
  } 

  void dealWithCallInst(CallInst* call_inst){
  	errs() <<"hello callinst\n";
  }
  
  bool runOnModule(Module &M) override {
    errs() << "Hello: ";
    errs().write_escaped(M.getName()) << '\n';
    Module::iterator func_i = M.begin();
    for(;func_i != M.end();++func_i){
    	Function* func = dyn_cast<Function>(func_i);
    	Function::iterator block_i = func->begin();
	    for(;block_i != func->end();++block_i){
	    	BasicBlock::iterator inst_i = block_i->begin();
	    	for(;inst_i != block_i->end();++inst_i){
	    		Instruction* inst = dyn_cast<Instruction> (inst_i);
	    		if(CallInst* call_inst = dyn_cast<CallInst> (inst)){
	    			//deal with direct function call
	    			if(Function* called_func = call_inst->getCalledFunction()){
	    				if(!called_func->isIntrinsic())
	    			    	errs()<<call_inst->getDebugLoc().getLine()<<":"<<called_func->getName()<<'\n';
	    			}
	    			//deal with indirect function call
	    		    else{
	    		    	errs()<<call_inst->getDebugLoc().getLine()<<"---------->"<<'\n';
	    		    	Value* called_value = call_inst->getCalledValue();
	    		    	called_value->dump();
	                    dealWithIndirectFptr(called_value);
	    		    }
	    		} 
	    	} 
	    }
    }
    errs()<<"------------------------------\n";
    return false;
	}
};


char FuncPtrPass::ID = 0;
static RegisterPass<FuncPtrPass> X("funcptrpass", "Print function call instruction");

static cl::opt<std::string>
InputFilename(cl::Positional,
              cl::desc("<filename>.bc"),
              cl::init(""));


int main(int argc, char **argv) {
   LLVMContext &Context = getGlobalContext();
   SMDiagnostic Err;
   // Parse the command line to read the Inputfilename
   cl::ParseCommandLineOptions(argc, argv,
                              "FuncPtrPass \n My first LLVM too which does not do much.\n");


   // Load the input module
   std::unique_ptr<Module> M = parseIRFile(InputFilename, Err, Context);
   if (!M) {
      Err.print(argv[0], errs());
      return 1;
   }

   llvm::legacy::PassManager Passes;
   	
   ///Remove functions' optnone attribute in LLVM5.0
   #if LLVM_VERSION_MAJOR == 5
   Passes.add(new EnableFunctionOptPass());
   #endif
   ///Transform it to SSA
   Passes.add(llvm::createPromoteMemoryToRegisterPass());

   /// Your pass to print Function and Call Instructions
   Passes.add(new FuncPtrPass());
   Passes.run(*M.get());
}

