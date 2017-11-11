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
#include <llvm/IR/DebugInfoMetadata.h>

#include <llvm/Transforms/Scalar.h>

#include <llvm/IR/Function.h>
#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/User.h>
#include <llvm/IR/Use.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DebugLoc.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/InstIterator.h>

#include <vector>

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
#define OUTS() errs() << call_inst->getDebugLoc().getLine() << ":";\
               for(auto iter = func_name.begin(); iter != func_name.end() - 1; ++iter) { \
                 errs() << *iter << ", ";\
               }\
               errs() << func_name.back() << "\n"

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
struct FuncPtrPass : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid
  FuncPtrPass() : FunctionPass(ID) {}
  std::vector<StringRef> func_name; 

  void dealWithPHINode(PHINode *pn) {
    //PHINode::block_iterator is of type BasicBlock **
    PHINode::block_iterator phi_i = pn->block_begin();
    for(; phi_i != pn->block_end(); ++phi_i) {
      Value *temp = pn->getIncomingValueForBlock(*phi_i);
      //recursive deal with PHINode
      if(dyn_cast<PHINode>(temp) ) {
        dealWithPHINode(dyn_cast<PHINode>(temp));
       // errs() << temp->getName() << "\n";
      }
      //delete NULL
      else if(!temp->getName().empty() ) {
        func_name.push_back(temp->getName() );
      }
    }
  }

  void dealWithRawFptr() {

  }

  bool runOnFunction(Function &F) override {
    bool update = false;
    Function::iterator block_i = F.begin();
    for(; block_i != F.end(); ++block_i) {
      BasicBlock::iterator inst_i = block_i->begin();
      for(; inst_i != block_i->end(); ++inst_i) {
        Instruction *inst = dyn_cast<Instruction>(inst_i);
        func_name.clear();
        if(CallInst *call_inst = dyn_cast<CallInst> (inst) ) {
          Value *called_value = call_inst->getCalledValue();
          //dierect function call
          if(Function *called_func = call_inst->getCalledFunction()) {
            if(!called_func->isIntrinsic() ) {
              errs() << call_inst->getDebugLoc().getLine() << ":" << called_func->getName() << "\n";
            }
          }
          //indirect function call
          else {
            //phinode function type
            Value *called_value = call_inst->getCalledValue();
            if(PHINode *phi_node = dyn_cast<PHINode>(called_value) ) {
              dealWithPHINode(phi_node); 
              OUTS();
            }
            //raw function pointer
            else {

            }
          }
        }
      }
    }
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

