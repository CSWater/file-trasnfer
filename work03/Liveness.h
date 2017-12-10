//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
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
#define ARG_TYPE 128

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

void dump(IPTS *ipts) {
  ipts->first->dump();
  std::set<PtrNode *> *pts = ipts->second;
  //iterate over ptr variable
  for(auto iter = pts->begin(); iter != pts->end(); ++iter) {
    PtrNode *ptr = *iter;
    outs() << ptr->id << ":";
    std::set<PtrNode *> *ptr_pts = ptr->pts;  //get ptr varialbe's pts
    if (!ptr_pts && ptr_pts->empty() ) {
      outs() << "NULL\n";
      continue;
    }
    //iterate over ptr variable's pts
    for(auto it = ptr_pts->begin(); it != ptr_pts->end(); ++it) {
      outs() << (*it)->id << "\t";
    }
    outs() << "\n";
  }
}
void dump(BPTS *bpts) {
  std::vector<IPTS *> *ipts_vec = bpts->second;
  for(auto iter = ipts_vec->begin(); iter != ipts_vec->end(); ++iter) {
    dump(*iter); 
  }
}
void dump(FPTS *fpts) {
  outs() << fpts->first->getName() << " FPTS\n";
  std::vector<BPTS *> *bpts_vec = fpts->second;
  for(auto iter = bpts_vec->begin(); iter != bpts_vec->end(); ++iter) {
    dump(*iter);
  }
  outs() << "\n";
}
struct PTSInfo {
  std::vector<FPTS *> gpts; 
  PTSInfo() : gpts() {}
  PTSInfo(const PTSInfo & info) : gpts(info.gpts) {}
  
  bool operator == (const PTSInfo & info) const {
      return gpts == info.gpts;
  }

public:
 void printGPTS() {
   for(auto iter = gpts.begin(); iter != gpts.end(); ++iter) {
     dump(*iter);
   }
 }
};

class PTSVisitor : public DataflowVisitor<struct PTSInfo> {
public:
 PTSVisitor() {}
 //function to compute PTS of a given function
 void computePTS(Function *F, PTSInfo *pts_info) override {
   if(F->isIntrinsic() ) 
     return;
   FPTS *fpts = new FPTS;
   std::vector<BPTS *> *bpts_vec = new std::vector<BPTS *>;
   for(auto block_i = F->begin(); block_i != F->end(); ++block_i) {
     BasicBlock *block = cast<BasicBlock>(block_i);
     computeBPTS(block, bpts_vec);
   }
   fpts->first = F;
   fpts->second = bpts_vec;
   pts_info->gpts.push_back(fpts);
 }
//funtion to calculate global PTS, assume that each funtion's PTS 
//has been calculated
 void computeGPTS(PTSInfo *gpts_info) override {
   std::vector<FPTS *> *gpts = &(gpts_info->gpts);
   for(auto iterf = gpts->begin(); iterf != gpts->end(); ++iterf) {
     FPTS *fpts = *iterf;
     for(auto iterb = fpts->second->begin(); iterb != fpts->second->end(); ++iterb) {
       BPTS *bpts = *iterb;
       for(auto iteri = bpts->second->begin(); iteri != bpts->second->end(); ++iteri) {
         IPTS *cur_ipts = *iteri;
         //the first inst of the func, don't have pre inst
         if(iteri == bpts->second->begin() && iterb == fpts->second->begin()) {
           continue;
         }
         //the first inst of a block, its pre inst is the last inst of the pre block
         else if(iteri == bpts->second->begin() ) {
           BasicBlock *block = bpts->first;
           for(auto pre = pred_begin(block); pre != pred_end(block); ++pre) {
             BasicBlock *pre_block = (*pre);
             //get the BPTS of pre_block
             auto iter = fpts->second->begin();
             while( (*iter)->first != pre_block) {
               iter++;
             }
             BPTS *pre_bpts = *iter;
             IPTS *pre_ipts = pre_bpts->second->back();
             merge(cur_ipts, pre_ipts);
           }
         }
         else {           //inst in the block, the first inst is *(iteri - 1)
           IPTS *pre_ipts = *(iteri - 1);
           merge(cur_ipts, pre_ipts);
         }
       }
     }
   }
 }

 //@TODO calculate PTS inter procudure
 void interProcedurePTSTransfer(PTSInfo *gpts) override {
   computeGPTS(gpts);
   int flag = 1;
   while(flag) {
     flag = 0;
     for(auto iter = call_inst_vec.rbegin(); iter != call_inst_vec.rend(); ++iter) {
       IPTS *curr_ipts = getIPTS(*iter, gpts);
       CallInst *call_inst = cast<CallInst>(*iter);
       getParamsFromCalled(call_inst, curr_ipts, gpts);
     }
   }
   computeGPTS(gpts);
 }
 
 //function to compute PTS of a given BasicBlock
 void computeBPTS(BasicBlock *block, std::vector<BPTS *> *bpts_vec) {
   BPTS *bpts = new BPTS;
   std::vector<IPTS *> *ipts_vec = new std::vector<IPTS *>;
   *bpts = std::make_pair(block, ipts_vec);
   for(auto inst_i = block->begin(); inst_i != block->end(); ++inst_i) {
      Instruction *inst = cast<Instruction>(inst_i);
      computeIPTS(inst, ipts_vec);
   }
   bpts_vec->push_back(bpts);
 }

 //function to compute PTS of a givem instrction
 void computeIPTS(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
#define IS(inst_name) !std::string(curr->getOpcodeName() ).compare(inst_name)
   if(CallInst *call_inst = dyn_cast<CallInst>(curr) ) {        
     //deal with intrinsic
     if(call_inst->getCalledFunction() && call_inst->getCalledFunction()->isIntrinsic()){
       switch(call_inst->getCalledFunction()->getIntrinsicID() ) {
         case Intrinsic::dbg_value:   dealWithIntrinsicValue(curr, ipts_vec);
                                      break; 
         case Intrinsic::dbg_declare: dealWithIntrisicDeclare(curr, ipts_vec);
                                      break;
         default:                     outs() << "unrecognized intrinsic error\n";
       }
     }
     else {                     //normal function call
       //push back call inst for output result
       call_inst_vec.push_back(curr); 
       dealWithCall(curr, ipts_vec);
     }
   }
   //@TODO deal with instructions that may change pts, like phi
   else if(IS("phi") ) {
     dealWithPhi(curr, ipts_vec);
   }
   //else {       //deal with instructions that will not change pts
   //  dealWithOtherInst(curr, ipts_vec);
   //}
 }

private:
 //store all the call inst to print out function call
 std::vector<Instruction *> call_inst_vec;
 //quick to find ptr node according to varialble name
 std::map<std::string, PtrNode *> ptr_node_table;     
//get real arg from called, if changed gpts, return 1
 int getParamsFromCalled(CallInst *call_inst, IPTS *ipts, PTSInfo *gpts) {
   if(Function *func = call_inst->getCalledFunction() ) {       //direct function call
     unsigned num = call_inst->getNumArgOperands();
     for(int i = 0; i < num; ++i) {
       Value *arg = call_inst->getArgOperand(i);
       if(arg->getType()->isPointerTy() ) {
         //get the called func's FPTS
         FPTS *fpts = NULL;
         for(auto iter = gpts->gpts.begin(); iter != gpts->gpts.end(); ++iter) {
           if(func == (*iter)->first) {
             fpts = *iter;
             break;
           }
         }
         //get arg PtrNode
         std::string arg_name = arg->getName();
         PtrNode *arg_ptr_node = NULL;
         for(auto it = ipts->second->begin(); it != ipts->second->end(); ++it) {
           if(arg_name.compare((*it)->id) ) {
             arg_ptr_node = *it;
           }
         }
         transferParamToFunc(arg_ptr_node, i, fpts);
       }
     }
   }
   else {                                                       //indirect function call
     //outs() << "indirect call:";
     //Value *call = call_inst->getCalledValue();
     //outs() << call->getName() << "\n";
     //Function *func = getFuntionByName(call->getName() );
   }
   return 0;
 }

 //transfer the arg PtrNode to the called func
 void transferParamToFunc(PtrNode *arg, int index, FPTS *fpts) {
   //the arg must in the entry block
   BPTS *entry_bpts = fpts->second->front();
   for(auto iteri = entry_bpts->second->begin(); iteri != entry_bpts->second->end(); ++iteri) {
     IPTS *ipts = *iteri;
     for(auto iter = ipts->second->begin(); iter != ipts->second->end(); ++iter) {
        PtrNode *ptr = *iter;
        if((ptr->type & 0x80 ) &&(ptr->type & 0xf) == (index + 1) ) {  //index start at 0, but argNO start at 1
          ptr->pts->insert(arg);
          outs() << "\n" << arg->id << " transfer complete!\n";
          return;
        }
     }
   }
 }

 void merge(IPTS *dest, IPTS *src) {
   std::set<PtrNode *> *dest_pts = dest->second;
   std::set<PtrNode *> *src_pts = src->second;
   for(auto iters = src_pts->begin(); iters != src_pts->end(); ++iters) {
     PtrNode *temp = *iters;
     bool new_var = true;
     for(auto iterd = dest_pts->begin(); iterd != dest_pts->end(); ++iterd) {
       if(!temp->id.compare((*iterd)->id) ) {     //merge the pts of the same ptr variable
         new_var = false;
         for(auto iter = temp->pts->begin(); iter != temp->pts->end(); ++iter) {
           (*iterd)->pts->insert(*iter);
         }
       }
     }
     if(new_var) {
        dest_pts->insert(temp);
     }
   }
 }

 //deal with llvm.dbg.value
 void dealWithIntrinsicValue(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
   CallInst *call_intrinsic = cast<CallInst>(curr);
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
     temp_ipts->first = curr;
     temp_ipts->second = new std::set<PtrNode *>;
     PtrNode *temp_node = new PtrNode;
     temp_node->id = var_name; 
     //push the ptr node into ptr_node_table, node with same name need to further deal with
     ptr_node_table[temp_node->id] = temp_node;
     //if it is a parameter, create a PtrNode and initialize it as "NULL"
     if(var_meta->getArg() ) {
       //argNO start at 1
       temp_node->type = ARG_TYPE | (var_meta->getArg() );   //it must be a function pointer
       temp_node->pts = new std::set<PtrNode *>;    //when a call occurs, we use real_arg to initialize 
     }
     else {          //it is a local variable
       //@TODO how to decide its type???
       temp_node->type = POINTER_TYPE;
       temp_node->pts = new std::set<PtrNode *>;      //create pts for this node
       //the var_value PtrNode exist, put it into var's pts directly
       if(ptr_node_table.find(var_value_name) != ptr_node_table.end() ) {
         temp_node->pts->insert(ptr_node_table.find(var_value_name)->second);
       }
       else {        //it must be a func addr or NULL,if it is a func addr, create a PtrNode for it
         if(!var_value_name.empty() ) {   
           PtrNode *func_ptr = new PtrNode;
           func_ptr->id = var_value_name;           
           ptr_node_table[func_ptr->id] = func_ptr;  
           func_ptr->type = VALUE_TYPE;
           func_ptr->pts = NULL;    //VALUE_TYPE has no pts
           temp_node->pts->insert(func_ptr);
         }
       }
     }
     temp_ipts->second->insert(temp_node);
     ipts_vec->push_back(temp_ipts);
     //dump(temp_ipts);
   }
 }
 //deal with llvm.dbg.declare
 void dealWithIntrisicDeclare(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
   CallInst *call_intrinsic = cast<CallInst>(curr);
  //@TODO 
 }
 //deal with phi node
 void dealWithPhi(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
   PHINode *phi = cast<PHINode>(curr);
   IPTS *temp_ipts = new IPTS;      //create IPTS for curr inst
   temp_ipts->first = curr;
   temp_ipts->second = new std::set<PtrNode *>;
   std::string var_name = curr->getName();
   PtrNode *temp_node = new PtrNode;               
   temp_node->id = var_name;
   temp_node->type = POINTER_TYPE;                 //Phinode must be PINTER_TYPE
   temp_node->pts = new std::set<PtrNode *>;       //create pts for this node
   //push the ptr node into ptr_node_table, node with same name need to further deal with
   ptr_node_table[temp_node->id] = temp_node;       
   for(int i = 0; i < phi->getNumIncomingValues(); ++i) {
      Value *phi_value = phi->getIncomingValue(i);
      std::string var_value_name = phi_value->getName();
      if(!var_value_name.empty() ) {      //deal with NON_NULL value
        //all phi node values must have exist
        temp_node->pts->insert(ptr_node_table.find(var_value_name)->second); 
      }
   }
   temp_ipts->second->insert(temp_node);
   ipts_vec->push_back(temp_ipts);
 }

 //deal with call inst, which won't change pts
 void dealWithCall(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
   // just create a empty PtrNode for call inst, 
   // and call inst will get its pts after called computeGPTS
   IPTS *temp_ipts = new IPTS;
   temp_ipts->first = curr;
   temp_ipts->second = new std::set<PtrNode *>;
   ipts_vec->push_back(temp_ipts);
 }

 //get the inst's IPTS
 IPTS *getIPTS(Instruction *inst, PTSInfo *gpts) {
   Function *func = inst->getFunction();   
   BasicBlock *block = inst->getParent();
   //get the FPTS this inst belongs to
   FPTS *fpts = NULL;
   for(auto iterf = gpts->gpts.begin(); iterf != gpts->gpts.end(); ++iterf) {
     if(func == (*iterf)->first) {
       fpts = *iterf; 
       break;
     }
   }
   //get the BPTS this inst belongs to
   BPTS *bpts = NULL;
   for(auto iterb = fpts->second->begin(); iterb != fpts->second->end(); ++iterb) {
     if(block == (*iterb)->first) {
       bpts = *iterb;
     }
   }
   //get the IPTS of this inst
   IPTS *ipts = NULL;
   for(auto iteri = bpts->second->begin(); iteri != bpts->second->end(); ++iteri) {
     if(inst == (*iteri)->first) {
       ipts = *iteri;
     }
   }
   return ipts;
 }
};

class Liveness : public ModulePass {
public:
 static char ID;

 PTSInfo pts_info;
 PTSVisitor visitor;

 Liveness() : ModulePass(ID) {} 

 bool runOnModule(Module &M) override {
   //compute each function's intradataflow and add it to g_pts
   for(auto func_i = M.begin(); func_i != M.end(); ++func_i) {
      Function *F = cast<Function>(func_i);
      visitor.computePTS(F, &pts_info);
   }
   visitor.computeGPTS(&pts_info);
   visitor.interProcedurePTSTransfer(&pts_info);
   pts_info.printGPTS();
   return false;
 }

 void showResult(std::vector<Instruction *> &result) {
   outs() << "call inst:\n";
   for(auto iter = result.begin(); iter != result.end(); ++iter) {
      outs() << (*iter)->getDebugLoc()->getLine() << " : ";
      (*iter)->dump();
   }
 }

};

