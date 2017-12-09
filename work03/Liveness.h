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

#include <llvm/IR/Function.h>
#include <llvm/Pass.h>
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/DebugInfoMetadata.h"

#include "Dataflow.h"
#include <vector>
#include <set>
#include <utility>
#include <string>
using namespace llvm;

#define VALUE_TYPE 1
#define POINTER_TYPE 2

//struct to denote a point-to-set
struct PTS{
  int type;    //a func ptr or a struct ptr
  std::set<std::string> pts;
};

//struct to denote a pointer variable and its point-to-set
struct PtrNode {
  std::string id;         //ptr id
  int type;   
  std::set<PtrNode *> *pts;                //point to sets
};

//struct to denote a pts of an instruction
typedef std::pair<Instruction *, std::set<PtrNode *> *> IPTS;
//struct to denote a pts of a basic block
typedef std::pair<BasicBlock *, std::vector<IPTS *> *> BPTS;      
//struct to denote a pts of a function's parameters
typedef std::pair<Function *, std::vector<BPTS *> *> FPTS;

struct PTSInfo {
   std::vector<FPTS *> gpts; 
   PTSInfo() : gpts() {}
   PTSInfo(const PTSInfo & info) : gpts(info.gpts) {}
  
   bool operator == (const PTSInfo & info) const {
       return gpts == info.gpts;
   }
};

class PTSVisitor : public DataflowVisitor<struct PTSInfo> {
public:
   PTSVisitor() {}
   void merge(PTSInfo * dest, const PTSInfo & src) override {
     //@TODO
   }

   void computePTS(Instruction *inst, PTSInfo * dfval) override{
     //@TODO
   }
};

class Liveness : public ModulePass {
public:
   static char ID;
   //store all the call inst to print out function call
   std::vector<Instruction *> call_inst_vec;
   //quick to find ptr node according to varialble name
   std::map<std::string, PtrNode *> ptr_node_table;     
   PTSInfo initval;
   PTSVisitor visitor;
   std::vector<FPTS *> gpts; 
   Liveness() : ModulePass(ID) {} 

   bool runOnModule(Module &M) override {
     for(auto func_i = M.begin(); func_i != M.end(); ++func_i) {
        Function *F = cast<Function>(func_i);
        runOnFunction(F);
     }
     printGPTS();
     showResult(call_inst_vec);
     return false;
   }

   //compute each function's intradataflow and add it to g_pts
   void runOnFunction(Function *F) {
       if(F->isIntrinsic() ) 
         return;
       FPTS *fpts = new FPTS;
       std::vector<BPTS *> *bpts_vec = new std::vector<BPTS *>;
       for(auto block_i = F->begin(); block_i != F->end(); ++block_i) {
         BasicBlock *block = cast<BasicBlock>(block_i);
         BPTS *bpts = new BPTS;
         std::vector<IPTS *> *ipts_vec = new std::vector<IPTS *>;
         *bpts = std::make_pair(block, ipts_vec);
         for(auto inst_i = block->begin(); inst_i != block->end(); ++inst_i) {
           Instruction *inst = cast<Instruction>(inst_i);
           //deal with function call inst
           if(CallInst *call_inst = dyn_cast<CallInst>(inst) ) {
             if(Function *func = call_inst->getCalledFunction() ) {
               if(func->isIntrinsic() ) {         //intrinsic call, which may 
                 //@TODO
                 computeIPTS(inst, ipts_vec); 
               }
               else {                //direct normal function call
                 call_inst_vec.push_back(call_inst);
               }
             }
             else {                  //indirect normal function call
               call_inst_vec.push_back(call_inst);
             }
           }
           //deal with other instructions that may change the
           //program's point-to-set
           else {
             //@TODO
             computeIPTS(inst, ipts_vec);
           }
         }
         bpts_vec->push_back(bpts);
       }
       //compBackwardDataflow(&F, &visitor, &result, initval);
       //printDataflowResult<PTSInfo>(errs(), result);
       fpts->first = F;
       fpts->second = bpts_vec;
       gpts.push_back(fpts);
   }

   void computeIPTS(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
#define IS(inst_name) !std::string(curr->getOpcodeName() ).compare(inst_name)
     if(CallInst *call_intrinsic = dyn_cast<CallInst>(curr) ) {
       Function *intrinsic = call_intrinsic->getCalledFunction(); 
       if(intrinsic->getName() == "llvm.dbg.value" ) {
         //get the declare pointer and init this inst pts, then add to this block pts
         //The first argument is the new value (wrapped as metadata)
         Metadata *temp = cast<Metadata>(cast<MetadataAsValue>(call_intrinsic->getOperand(0) )->getMetadata() );
         Value *var_value = cast<ValueAsMetadata>(temp)->getValue();
         std::string var_value_name = var_value->getName();
         DILocalVariable *var_meta = cast<DILocalVariable>((cast<MetadataAsValue>(call_intrinsic->getOperand(2) ) )->getMetadata() );
         std::string var_name = var_meta->getName();
         //pointer type
         if( (cast<DIType>(var_meta->getRawType() ) )->getTag() == 0x000f) {
            IPTS *temp_ipts = new IPTS;      //create IPTS for curr inst
            std::set<PtrNode *> *set_of_inst= new std::set<PtrNode *>;
            PtrNode *temp_node = new PtrNode;
            temp_node->id = var_name; 
            //push the ptr node into ptr_node_table, node with same name need to further deal with
            ptr_node_table[temp_node->id] = temp_node;
            //if it is a parameter, create a PtrNode and initialize it as "NULL"
            if(var_meta->getArg() ) {
              temp_node->type = POINTER_TYPE;   //it must be a function pointer
              temp_node->pts = NULL;    //when a call occurs, we use real_arg to initialize 
            }
            else {          //it is a local variable
              temp_node->pts = new std::set<PtrNode *>;
              //@TODO how to decide its type???
              temp_node->type = POINTER_TYPE;
              temp_node->pts = new std::set<PtrNode *>;
              //the var_value PtrNode exist, put it into var's pts directly
              if(ptr_node_table.find(var_value_name) != ptr_node_table.end() ) {
                temp_node->pts->insert(ptr_node_table.find(var_value_name)->second);
              }
              else {          //it must be a funtion addr, create a PtrNode for it
                PtrNode *func_ptr = new PtrNode;
                func_ptr->id = var_value_name;
                ptr_node_table[func_ptr->id] = func_ptr;
                func_ptr->type = VALUE_TYPE;
                func_ptr->pts = NULL;    //VALUE_TYPE has no pts
                temp_node->pts->insert(func_ptr);
              }
            }
            set_of_inst->insert(temp_node);
            *temp_ipts = std::make_pair(curr, set_of_inst);
            ipts_vec->push_back(temp_ipts);
         }
       }
       else if(intrinsic->getName() == "llvm.dbg.declare") {

       }
       //@TODO
     }
     //@TODO deal with other instructions, like phi
     else if(IS("phi") ) {
        
     }
     //else if() {

     //}
   }

   //merge the IPTS of the curr instruction and its pre instruction
   void mergePTS(std::set<PtrNode *> *dest, std::set<PtrNode *> *src) {
      
   }
     
   void changePTS( ) {

   }

   void showResult(std::vector<Instruction *> &result) {
     outs() << "call inst:\n";
     for(auto iter = result.begin(); iter != result.end(); ++iter) {
        outs() << (*iter)->getDebugLoc()->getLine() << " : ";
        (*iter)->dump();
     }
   }

   void printGPTS() {
     //iterate over FPTS
     for(auto iterf = gpts.begin(); iterf != gpts.end(); ++iterf) {
       outs() << (*iterf)->first->getName() << " FPTS:\n";
       std::vector<BPTS *> *bpts_vec = (*iterf)->second;
       //iterate over BPTS
       for(auto iterb = bpts_vec->begin(); iterb != bpts_vec->end(); ++iterb) {
         std::vector<IPTS *> *ipts_vec = (*iterb)->second;
         //iterate over IPTS
         for(auto iteri = ipts_vec->begin(); iteri != ipts_vec->end(); ++iteri) {
           Instruction *curr = (*iteri)->first;
           curr->dump();
           std::set<PtrNode *> *ipts = (*iteri)->second;
           //iterate over PtrNode
           for(auto iterp = ipts->begin(); iterp != ipts->end(); ++iterp) {
             if((*iterp)->type == VALUE_TYPE) continue;
             else if((*iterp)->type == POINTER_TYPE) {
               outs() << (*iterp)->id << " : ";
               std::set<PtrNode *> *pts = (*iterp)->pts;
               //if((std::set<PtrNode *> *pts = (*iterp)->pts) != NULL)
               if(pts != NULL) {
                 for(auto iter = pts->begin(); iter != pts->end(); ++iter) {
                   outs() << (*iter)->id << "\t";
                 }
                 outs() << "\n";
               }
               else {
                 outs() << "NULL\n";
               }
             }
           }
         }
       }
       outs() << "\n";
     }
   }
   
};

