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
#include <llvm/Support/FileSystem.h>
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
#include "llvm/IR/InstrTypes.h"
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
#include<vector>
#include<set>
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
  struct line_func {
    unsigned line;
    std::set<StringRef> name;
  };
  std::vector<line_func>func_name;

  bool updated;

  void display(std::vector<line_func> &funcname) {
    for(auto iter = funcname.begin(); iter != funcname.end(); ++iter) {
      std::string out;
      errs() << iter->line << ":";
      for(auto &it : iter->name) {
        out.append(it).append(", ");
      }
      out.resize(out.length() - 2);
      errs() << out << "\n";
    }
  }
  void dealWithIndirectFptr(Value* val){
    if(Argument* argument = dyn_cast<Argument>(val)) {
      dealWithArgument(argument);
    }
    if(CallInst* call_inst = dyn_cast<CallInst>(val)) {
      dealWithCallInst(call_inst);
    }
    if(PHINode* phi_node = dyn_cast<PHINode>(val)) {
      dealWithPhinode(phi_node);
    }
  }


  void dealWithPhinode(PHINode* phi_node){
    //phi_node->getParent()->dump();
  	unsigned num = phi_node->getNumIncomingValues();

    std::set<BasicBlock *> pre_block;
    unsigned wanted = -1;
    for(int i = 0; i < num; ++i) {
      BasicBlock *basic = phi_node->getIncomingBlock(i);
      BasicBlock *pre = basic->getSinglePredecessor();
      pre_block.insert(pre);
    }
    if(pre_block.size() == 1) {
      BasicBlock *pre = *pre_block.begin(); 
      TerminatorInst *ter = pre->getTerminator();
      if(isa<BranchInst>(ter) ) {
        BranchInst *br = dyn_cast<BranchInst>(ter);
        if(br->isConditional() ) {
          CmpInst *cmp = dyn_cast<CmpInst>(br->getCondition());
          Value *op1 = cmp->getOperand(0);
          Value *op2 = cmp->getOperand(1);
          if(isa<ConstantInt>(op1) && isa<ConstantInt>(op2) ) {
            //TODO ???
            //why int opt_value = dyn_cast<ConstantInt>(op1).getSExtValue() is wrong
            int op1_value = (*dyn_cast<ConstantInt>(op1)).getSExtValue();
            int op2_value = (*dyn_cast<ConstantInt>(op2)).getSExtValue();
            if(op1_value > op2_value) {
              //phi_node->removeIncomingValue(1);
              wanted = 0;
	            for (int i = 0; i < phi_node->getNumIncomingValues(); ++i) {
                if(i == wanted) {
	            	  Value *temp = phi_node->getIncomingValue(i);
                  if(isa<Function>(temp) ) {
                    if(!temp->getName().empty() ) {
                     phi_node->replaceAllUsesWith(temp);      
                     updated = true;
                     func_name.back().name.insert(temp->getName() );
                    }
                  }
                }
              }
              //have updated, return;
              return;
            }
          }
        }
      }
    }
	  for (int i = 0; i < phi_node->getNumIncomingValues(); ++i) {
	  	Value *temp = phi_node->getIncomingValue(i);
      if(isa<Function>(temp) ) {
        if(!temp->getName().empty() ) {
         func_name.back().name.insert(temp->getName() );
        }
      }
      else {
        dealWithIndirectFptr(temp);
      }
	  }
  }

  void dealWithArgument(Argument* argument){
    //argument number
    unsigned index = argument->getArgNo();
    //belong to which function
    Function* func = argument->getParent();
    std::string temp_name1 = func->getName();
    auto user_i = func->user_begin();
    for(; user_i != func->user_end(); ++user_i){
      //call function name is the last operand of the user_i
      unsigned num = (*user_i)->getNumOperands();
      std::string temp_name2 = (*user_i)->getOperand(num - 1)->getName();
      if(CallInst *call_inst = dyn_cast<CallInst>(*user_i) ) {
        //used as a function call, i.e func(param_0, param_1, ...)
        if(temp_name2.compare(temp_name1) == 0) {
          Value *arg = call_inst->getArgOperand(index);
          if(Function *func_ptr = dyn_cast<Function>(arg)) {
            func_name.back().name.insert(func_ptr->getName() );
          }
          else {
            dealWithIndirectFptr(arg);
          }
        }
        //used as a param of a function call, i.e funcXXX(..., func, ...)
        else {
          unsigned num = call_inst->getNumArgOperands();
          int i = 0;
          //as a param, find the param number in the param list
          for(i = 0; i < num; ++i) {
            Value *temp_arg = call_inst->getArgOperand(i);
            if(temp_name1.compare(temp_arg->getName()) == 0)
              break;
          }
          //visit the called function to get what the param "func" do in the called function
          Function *call_func = call_inst->getCalledFunction();
          for(auto inst_i = inst_begin(call_func); inst_i != inst_end(call_func); ++inst_i) {
            if(CallInst *param_call = dyn_cast<CallInst>(&(*inst_i) ) ) {
	    			  if(Function* called_func = param_call->getCalledFunction()){/*intrinsic, Do nothing*/}
              else {
	    		  	  Value* called_value = param_call->getCalledValue();
                //make sure deal with the right CallInst
                if(isa<Argument>(called_value) && (dyn_cast<Argument>(called_value)->getArgNo() == i) ) {
                  func_name.back().name.insert(param_call->getArgOperand(index)->getName() );
                  //Argument *temp_arg = dyn_cast<Argument>(param_call->getArgOperand(index));
                  //dealWithArgument(temp_arg);
                }
              }
            }
          }
        }
      }
      else if(PHINode *phi_node = dyn_cast<PHINode>(*user_i) ) {
        for(User *user_p :phi_node->users() )  {
          if(CallInst *call_inst = dyn_cast<CallInst>(user_p)) {
            Value *arg = call_inst->getArgOperand(index);
            if(Function * arg_func = dyn_cast<Function>(arg) ) {
              func_name.back().name.insert(arg_func->getName() );
            }
            else {
              if(PHINode *temp = dyn_cast<PHINode>(arg)) {
                temp->replaceAllUsesWith(temp->getIncomingValue(0));      
                updated = true;
              }
              //func_name.back().name.insert(temp->getName() );
            }
          }
        }
      }
    }
  } 

  void dealWithCallInst(CallInst* call_inst){
    if(Function *called_func = call_inst->getCalledFunction() ) {
      for(auto inst_i = inst_begin(called_func); inst_i != inst_end(called_func); ++inst_i) {
        if(ReturnInst *ret = dyn_cast<ReturnInst>(&(*inst_i) ) ) {
          Value *return_val = ret->getReturnValue();
          dealWithIndirectFptr(return_val);
        }
      }
    }
    else {
      Value *called_value = call_inst->getCalledValue();
      if(PHINode *phi_node = dyn_cast<PHINode>(called_value)) {
        unsigned num = phi_node->getNumIncomingValues();
        for(int i = 0; i < num; ++i) {
          Value *temp = phi_node->getIncomingValue(i);
          if(Function *called_func = dyn_cast<Function>(temp)) {
            for(auto inst_i = inst_begin(called_func); inst_i != inst_end(called_func); ++inst_i) {
              if(ReturnInst *ret = dyn_cast<ReturnInst>(&(*inst_i) ) ) {
                Value *return_val = ret->getReturnValue();
                dealWithIndirectFptr(return_val);
              }
            }
          }
        }
      }
    }
  }
  
  bool runOnModule(Module &M) override {
    updated = false;
    Module::iterator func_i = M.begin();
    //iterate on all the function
    for(;func_i != M.end();++func_i){
      //deal with each function
    	Function* func = dyn_cast<Function>(func_i);
      if(!func) 
        continue;
    	Function::iterator block_i = func->begin();
	    for(;block_i != func->end();++block_i){
	    	BasicBlock::iterator inst_i = block_i->begin();
	    	for(;inst_i != block_i->end();++inst_i){
	    		Instruction* inst = dyn_cast<Instruction> (inst_i);
	    		if(CallInst* call_inst = dyn_cast<CallInst> (inst)){
	    			//deal with direct function call
	    			if(Function* called_func = call_inst->getCalledFunction()){
	    				if(!called_func->isIntrinsic()) {
                unsigned line = call_inst->getDebugLoc().getLine();
                if(func_name.empty() || func_name.back().line != line) {
                  line_func temp;
                  temp.line = line;
                  func_name.push_back(temp);
                }
                func_name.back().name.insert(called_func->getName());
              }
	    			}
	    			//deal with indirect function call
	    		  else{
	    		  	Value* called_value = call_inst->getCalledValue();
              unsigned line = call_inst->getDebugLoc().getLine();
              if(func_name.empty() || func_name.back().line != line) {
                line_func temp;
                temp.line = line;
                func_name.push_back(temp);
              }
	            dealWithIndirectFptr(called_value);
	    		  }
	    		} 
	    	} 
	    }
    }
    display(func_name);
    //if(updated )
    //  errs() << "\n updated!!!!!\n";
    return updated;
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
   bool updated = Passes.run(*M.get());

   if(updated) {
     std::error_code EC;
     std::unique_ptr<tool_output_file> Out(new tool_output_file(InputFilename, EC, sys::fs::F_RW));
     if(EC) {
       errs() << EC.message() << "\n";
     }
     WriteBitcodeToFile(M.get(), Out->os());
     Out->keep();
   }
}

