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
#include "llvm/Support/Dwarf.h"

#include "Dataflow.h"
#include <vector>
#include <set>
#include <utility>
#include <string>
using namespace llvm;

#define VALUE_TYPE 1
#define POINTER_TYPE 2            
#define PHI_TYPE  4               
#define ARG_TYPE 128

//struct to store output result
struct LineFunc {
  Instruction *inst;
  std::set<std::string> name;
  LineFunc():inst(NULL), name() { }
};
//struct to denote a pointer variable and its point-to-set
struct PtrNode {
  std::string id;         //ptr id
  int type;   
  std::set<std::string> pts_name;
  std::set<PtrNode *> *pts;
  PtrNode() : id(), type(0), pts_name(), pts(NULL){}
};
//struct to denote a pts of an instruction
typedef std::pair<Instruction *, std::set<PtrNode *> *> IPTS;
//struct to denote a pts of a basic block
typedef std::pair<BasicBlock *, std::vector<IPTS *> *> BPTS;      
//struct to denote a pts of a function's parameters
typedef std::pair<Function *, std::vector<BPTS *> *> FPTS;


//Base address
struct BaseAddr {
  std::string id;                                 //id to represnt a addr
  unsigned len;                                     //the number of elements
  std::vector<std::vector<StoreInst *> > elements;   //use store op to record the newest value of the addr
  BaseAddr() : id(), len(-1), elements() {}
};

//alia of a addr
struct AddrAlia {
  std::string name;                                 //addr alia
  std::string base;                                 //alia of which base
  unsigned index;                                   //along with base to lacate the real addr
  AddrAlia() : name(), base(), index(-1) {}
};

void dump(IPTS *ipts) {
  ipts->first->dump();
  std::set<PtrNode *> *pts = ipts->second;
  //iterate over ptr variable
  for(auto iter1 = pts->begin(); iter1 != pts->end(); ++iter1) {
    PtrNode *ptr = *iter1;
    outs() << ptr->id << ":";
    for(auto iter = ptr->pts_name.begin(); iter != ptr->pts_name.end(); ++iter) {
      outs() << *iter << "\t";
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
private:
 //global function addr
 std::map<std::string, PtrNode *> gfuncs;
 //global base addr. get by AllocaInst and malloc function
 std::map<Function *, std::map<std::string, BaseAddr *> > gaddr;
 //store all the call inst to help inter procedure PTS analysis
 std::vector<Instruction *> call_inst_vec;
 //store result
 std::vector<LineFunc> result;
public :
 PTSVisitor() {}
 //function to compute PTS of a given function
 void computePTS(Function *F, PTSInfo *pts_info) override;
//funtion to calculate global PTS, assume that each funtion's PTS 
//has been calculated
 void computeGPTS(PTSInfo *gpts_info) override ;
 //calculate PTS inter procudure
 void interProcedurePTSTransfer(PTSInfo *gpts) override;
 //calculate function call information
 void computeResult(PTSInfo *gpts);
 //function to compute PTS of a given BasicBlock
 void computeBPTS(BasicBlock *block, std::vector<BPTS *> *bpts_vec, std::set<AddrAlia *> *addr_alias);
 //function to compute PTS of a givem instrction
 void computeIPTS(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias);
 //print result
 void printResult();
 
private:
 //get real arg from called, if changed gpts, return 1
 int getParamsFromCalled(CallInst *call_inst, IPTS *ipts, PTSInfo *gpts);
 //transfer the arg PtrNode to the called func
 void transferParamToFunc(PtrNode *arg, int index, FPTS *fpts);
 //merge
 void merge(IPTS *dest, IPTS *src);
 //deal with llvm.dbg.value
 void dealWithIntrinsicValue(Instruction *curr, std::vector<IPTS *> *ipts_vec);
 //deal with llvm.dbg.declare
 void dealWithIntrisicDeclare(Instruction *curr, std::vector<IPTS *> *ipts_vec);
 //deal with phi node
 void dealWithPhi(Instruction *curr, std::vector<IPTS *> *ipts_vec);
 //deal with call inst, which won't change pts
 void dealWithCall(Instruction *curr, std::vector<IPTS *> *ipts_vec);
 //deal with alloca inst
 void dealWithAlloca(Instruction *curr);
 //deal with getElementPtr inst
 void dealWithGetElementPtr(Instruction *curr, std::set<AddrAlia *> *addr_alias);
 //deal with store inst
 void dealWithStore(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias);
 //deal with load inst
 void dealWithLoad(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias);
 //get the inst's IPTS
 IPTS *getIPTS(Instruction *inst, PTSInfo *gpts);
};

//function to compute PTS of a given function
void PTSVisitor::computePTS(Function *F, PTSInfo *pts_info) {
  if(F->isIntrinsic() ) 
    return;
  //add global information to gfuncs and gaddr
  PtrNode *func_ptr = new PtrNode;
  func_ptr->id = F->getName();
  func_ptr->type = VALUE_TYPE;
  gfuncs[F->getName()] = func_ptr;
  std::map<std::string, BaseAddr *> base_addrs;
  gaddr[F] = base_addrs;
  //create FPTS andaddr alias  for the function
  FPTS *fpts = new FPTS;
  std::set<AddrAlia *> *addr_alias = new std::set<AddrAlia *>;
  std::vector<BPTS *> *bpts_vec = new std::vector<BPTS *>;

  for(auto block_i = F->begin(); block_i != F->end(); ++block_i) {
    BasicBlock *block = cast<BasicBlock>(block_i);
    computeBPTS(block, bpts_vec, addr_alias);
  }
  fpts->first = F;
  fpts->second = bpts_vec;
  pts_info->gpts.push_back(fpts);
}

//funtion to calculate global PTS, assume that each funtion's PTS 
//has been calculated
void PTSVisitor::computeGPTS(PTSInfo *gpts_info) {
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
            if(!pre_bpts->second->empty() ) {
              IPTS *pre_ipts = pre_bpts->second->back();
              merge(cur_ipts, pre_ipts);
            }
            else {
              outs() << "merge error! pre BPTS is empty\n";
            }
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
//calculate PTS inter procudure
void PTSVisitor::interProcedurePTSTransfer(PTSInfo *gpts) {
  computeGPTS(gpts);
  int flag = 1;
  flag = 0;
  for(auto iter = call_inst_vec.rbegin(); iter != call_inst_vec.rend(); ++iter) {
    IPTS *curr_ipts = getIPTS(*iter, gpts);
    CallInst *call_inst = cast<CallInst>(*iter);
    getParamsFromCalled(call_inst, curr_ipts, gpts);
    //computeResult(gpts);
  }
}
//calculate function call information
void PTSVisitor::computeResult(PTSInfo *gpts) {
  for(auto iter1 = result.begin(); iter1 != result.end(); ++iter1) {
    LineFunc &temp = *iter1;
    CallInst *call_inst = cast<CallInst>(temp.inst);
    //direct call
    if(Function *func = call_inst->getCalledFunction() ) {
      temp.name.insert(func->getName() );
    }
    else {                     //indirect call
      Value *call = call_inst->getCalledValue();
      std::string ptr_name = call->getName();
      IPTS *ipts = getIPTS(temp.inst, gpts);
      PtrNode *me = NULL;
      for(auto iter2 = ipts->second->begin(); iter2 != ipts->second->end(); ++iter2) {
        if(!ptr_name.compare((*iter2)->id) ) {
          me = *iter2;
          break;
        }
      }
      bool flag = true;
      for(auto iter3 = me->pts->begin(); iter3 != me->pts->end(); ++iter3) {
        if((*iter3)->type == VALUE_TYPE) {
          temp.name.insert((*iter3)->id);        
        }
      }
    }
  }
}
//function to compute PTS of a given BasicBlock
void PTSVisitor::computeBPTS(BasicBlock *block, std::vector<BPTS *> *bpts_vec, std::set<AddrAlia *> *addr_alias) {
  BPTS *bpts = new BPTS;
  std::vector<IPTS *> *ipts_vec = new std::vector<IPTS *>;
  *bpts = std::make_pair(block, ipts_vec);
  for(auto inst_i = block->begin(); inst_i != block->end(); ++inst_i) {
     Instruction *inst = cast<Instruction>(inst_i);
     computeIPTS(inst, ipts_vec, addr_alias);
  }
  bpts_vec->push_back(bpts);
}
//function to compute PTS of a givem instrction
void PTSVisitor::computeIPTS(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias) {
#define IS(inst_name) !std::string(curr->getOpcodeName() ).compare(inst_name)
  if(CallInst *call_inst = dyn_cast<CallInst>(curr) ) {        
    //deal with intrinsic
    if(call_inst->getCalledFunction() && call_inst->getCalledFunction()->isIntrinsic()){
      switch(call_inst->getCalledFunction()->getIntrinsicID() ) {
        case Intrinsic::dbg_value:   dealWithIntrinsicValue(curr, ipts_vec);
                                     break; 
        case Intrinsic::dbg_declare: dealWithIntrisicDeclare(curr, ipts_vec);
                                     break;
        case Intrinsic::memset: break;                   
        default:                     outs() << "unrecognized intrinsic error\n";
                                     call_inst->getCalledFunction()->dump();
      }
    }
    else {                     //normal function call
      //push back call inst for interProcedure PTS analysis
      call_inst_vec.push_back(curr); 
      LineFunc temp;
      temp.inst = curr;
      result.push_back(temp);
      dealWithCall(curr, ipts_vec);
    }
  }
  //@TODO deal with instructions that may change pts, like phi
  else if(IS("phi") ) {
    dealWithPhi(curr, ipts_vec);
  }
  else if(IS("alloca") ) {             //alloca mem, produce base addr
    dealWithAlloca(curr);
  }
  else if(IS("getelementptr") ) {
    dealWithGetElementPtr(curr, addr_alias); 
  }
  else if(IS("store") ) {
    dealWithStore(curr, ipts_vec, addr_alias);
  }
  else if(IS("load") ) {
    dealWithLoad(curr, ipts_vec, addr_alias);
  }
  //else {       //deal with instructions that will not change pts
  //  dealWithOtherInst(curr, ipts_vec);
  //}
}
//print result
void PTSVisitor::printResult() {
  if(result.empty() )
    return;
  for(auto iter = result.begin(); iter != result.end(); ++iter) {
    std::string out;
    errs() << iter->inst->getDebugLoc()->getLine() << ":";
    for(auto &it : iter->name) {
      out.append(it).append(", ");
    }
    out.resize(out.length() - 2);
    errs() << out << "\n";
  }
}
//get the inst's IPTS
IPTS* PTSVisitor::getIPTS(Instruction *inst, PTSInfo *gpts) {
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
//get real arg from called, if changed gpts, return 1
int PTSVisitor::getParamsFromCalled(CallInst *call_inst, IPTS *ipts, PTSInfo *gpts) {
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
        std::string arg_name = arg->getName();
        PtrNode *arg_ptr_node = NULL;
        if(gfuncs.find(arg_name) != gfuncs.end() ) {         //func addr as param
          arg_ptr_node = gfuncs.find(arg_name)->second;
        }
        else {                                               //func ptr as param
          for(auto it = ipts->second->begin(); it != ipts->second->end(); ++it) {
            if(!arg_name.compare((*it)->id) ) {
              arg_ptr_node = *it;
            }
          }
        }
        transferParamToFunc(arg_ptr_node, i, fpts);
      }
    }
  }
  else {                                                       //indirect function call
    //@TODO
  }
  return 0;
}
//transfer the arg PtrNode to the called func
void PTSVisitor::transferParamToFunc(PtrNode *arg, int index, FPTS *fpts) {
  //the arg must in the entry block
  BPTS *entry_bpts = fpts->second->front();
  for(auto iteri = entry_bpts->second->begin(); iteri != entry_bpts->second->end(); ++iteri) {
    IPTS *ipts = *iteri;
    for(auto iter = ipts->second->begin(); iter != ipts->second->end(); ++iter) {
       PtrNode *param = *iter;
       if((param->type & 0x80 ) &&(param->type & 0xf) == (index + 1) ) {  //index start at 0, but argNO start at 1
         //delete the old call inst arg
         param->pts_name.clear();
         param->pts->clear();
         //pass the new arg
         param->pts_name.insert(arg->id);
         param->pts->insert(arg);
         outs() << "\n" << arg->id << " transfer complete!\n";
         return;
       }
    }
  }
}
//merge the IPTS of two successive inst
//two stages
//stage 1, logically merge and update
//stage 2, physically update
//for example (dest:af_ptr-->plus, bf_ptr-->minus) (src:cf_ptr's ptr_name is af_ptr)
//stage 1, dest:af_ptr-->plus, bf_ptr-->minus, cf_ptr's ptr_name is af_ptr
//stage 2, dest:af_ptr-->plus, bf_ptr-->minus. cf_ptr-->af_ptr
//@TODO there is some problem to be solved, in a single block, a ptr get two value
//but it should has the last value as its value at last
void PTSVisitor::merge(IPTS *dest, IPTS *src) {
  std::set<PtrNode *> *dest_pts = dest->second;
  std::set<PtrNode *> *src_pts = src->second;
  //stage 1 merge
  for(auto iters = src_pts->begin(); iters != src_pts->end(); ++iters) {
    PtrNode *temp = *iters;
    bool new_var = true;
    for(auto iterd = dest_pts->begin(); iterd != dest_pts->end(); ++iterd) {
      if(!temp->id.compare((*iterd)->id) ) {    
        new_var = false;
      }
    }
    if(new_var) {
       dest_pts->insert(temp);
    }
  }
  //stage 2 merge
  for(auto iter1 = dest_pts->begin(); iter1 != dest_pts->end(); ++iter1) {
    PtrNode *temp = *iter1;
    if(temp->pts->empty() ) {
      for(auto iter1 = temp->pts_name.begin(); iter1 != temp->pts_name.end(); ++iter1) {
        std::string ptr_name = *iter1;
        for(auto iter2 = dest_pts->begin(); iter2 != dest_pts->end(); ++iter2) {
          if(ptr_name.compare((*iter2)->id) ) {
            temp->pts->insert(*iter2);
            break;
          }
        }
      }
    }
  }
}
//deal with llvm.dbg.value
void PTSVisitor::dealWithIntrinsicValue(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
  CallInst *call_intrinsic = cast<CallInst>(curr);
  //get the declare pointer and init this inst pts, then add to this block pts
  //The first argument is the new value (wrapped as metadata)
  Metadata *temp = cast<Metadata>(cast<MetadataAsValue>(call_intrinsic->getOperand(0) )->getMetadata() );
  Value *var_value = cast<ValueAsMetadata>(temp)->getValue();
  std::string var_value_name = var_value->getName();
  DILocalVariable *var_meta = cast<DILocalVariable>((cast<MetadataAsValue>(call_intrinsic->getOperand(2) ) )->getMetadata() );
  std::string var_name = var_meta->getName();
  //pointer type
  if( (cast<DIType>(var_meta->getRawType() ) )->getTag() == dwarf::DW_TAG_pointer_type) {        
    PtrNode *temp_node = new PtrNode;
    temp_node->id = var_name; 
    temp_node->pts = new std::set<PtrNode *>;                  //init as null pts
    //if it is a parameter, create a PtrNode and initialize it as "NULL"
    //when a call occurs, we use real_arg to initialize 
    if(var_meta->getArg() ) {
      temp_node->type = ARG_TYPE | (var_meta->getArg() );  
    }
    else {          //it is a local variable
      temp_node->type = POINTER_TYPE;
      if(!var_value_name.empty() ) {                           //point to NON_NULL value
        //if it is a function addr, just initialize the temp_node
        //else we just do a logically "point to"
        //we delay the physically "point to" to the computeGPTS stage. Because we need 
        //PtrNode infomation of this value, which we can get at computeGPTS stage
        if(gfuncs.find(var_value_name) != gfuncs.end() ) {     //it is a func addr
          temp_node->pts_name.insert(var_value_name);
          temp_node->pts->insert(gfuncs.find(var_value_name)->second);
        }
        else {
          temp_node->pts_name.insert(var_value_name);
        }
      }
    }
    IPTS *temp_ipts = new IPTS;      //create IPTS for curr inst
    temp_ipts->first = curr;
    temp_ipts->second = new std::set<PtrNode *>;
    temp_ipts->second->insert(temp_node);
    ipts_vec->push_back(temp_ipts);
  }
}
//deal with llvm.dbg.declare
void PTSVisitor::dealWithIntrisicDeclare(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
  CallInst *call_intrinsic = cast<CallInst>(curr);
 //@TODO 
}
//deal with phi node
void PTSVisitor::dealWithPhi(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
  PHINode *phi = cast<PHINode>(curr);
  std::string var_name = curr->getName();
  PtrNode *temp_node = new PtrNode;               
  temp_node->id = var_name;
  temp_node->type = PHI_TYPE;                 
  temp_node->pts = new std::set<PtrNode *>;
  for(int i = 0; i < phi->getNumIncomingValues(); ++i) {
     Value *phi_value = phi->getIncomingValue(i);
     std::string var_value_name = phi_value->getName();
     if(!var_value_name.empty() ) {      //deal with NON_NULL value
       //@TODO
       temp_node->pts_name.insert(var_value_name);
       temp_node->pts->insert(gfuncs[var_value_name]); 
     }
  }
  IPTS *temp_ipts = new IPTS;      //create IPTS for curr inst
  temp_ipts->first = curr;
  temp_ipts->second = new std::set<PtrNode *>;
  temp_ipts->second->insert(temp_node);
  ipts_vec->push_back(temp_ipts);
}
//deal with call inst, which won't change pts
void PTSVisitor::dealWithCall(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
  // just create a empty PtrNode for call inst, 
  // and call inst will get its pts after called computeGPTS
  IPTS *temp_ipts = new IPTS;
  temp_ipts->first = curr;
  temp_ipts->second = new std::set<PtrNode *>;
  ipts_vec->push_back(temp_ipts);
}
//deal with alloca inst
void PTSVisitor::dealWithAlloca(Instruction *curr) {
  BaseAddr *base = new BaseAddr;
  Function *func = curr->getFunction();
  std::string name = curr->getName();          //addr name
  AllocaInst *alloca = cast<AllocaInst>(curr);
  unsigned len = cast<ConstantInt>(alloca->getArraySize() )->getZExtValue();
  base->id = name;
  base->len = len;
  std::vector<std::vector<StoreInst *> > elements;
  base->elements = elements;
  for(int i = 0; i < len; ++i) {
    std::vector<StoreInst *> store_op;
    base->elements.push_back(store_op);
  }
  gaddr[func][name] = base;
  outs() << name << " length: " << len << "\n";
}
//deal with getElementPtr inst
void PTSVisitor::dealWithGetElementPtr(Instruction *curr, std::set<AddrAlia *> *addr_alias) {
  AddrAlia *alia = new AddrAlia;
  alia->name = curr->getName();
  GetElementPtrInst *get_ptr = cast<GetElementPtrInst>(curr);
  std::string base = get_ptr->getPointerOperand()->getName();
  alia->base = base;
  unsigned index_num = get_ptr->getNumIndices();
  //the first operand is PointerOperand, and the first index is always 0
  for(int i = 2; i < index_num + 1; ++i) {
     alia->index = cast<ConstantInt>(get_ptr->getOperand(i) )->getZExtValue(); 
  }
  addr_alias->insert(alia);
  outs() << "base : " << alia->base << "num of index : " << index_num << "\n";

}
//deal with store inst
void PTSVisitor::dealWithStore(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias) {
  StoreInst *store_inst = cast<StoreInst>(curr);
  std::string alia_name = store_inst->getPointerOperand()->getName();
  std::string base_name;
  BaseAddr *base = NULL;
  AddrAlia *alia = NULL;
  for(auto iter = addr_alias->begin(); iter != addr_alias->end(); ++iter) {
    if(!alia_name.compare((*iter)->name) ) {
      alia = *iter; 
      break;
    }
  }
  if(alia != NULL) {
    base_name = alia->base;
  }
  else {            //should not occur
    outs() << "unrecongnized addr alia error!\n";
  }
  outs() << "alia : " << alia->name << " base: " << alia->base << "\n";
  Function *func = curr->getFunction();
  base = gaddr[func][base_name];
  if(base == NULL) {
    outs() << "error! can not find the base addr\n";
  }
  outs() << "base : " << base_name << " index : " << alia->index << "\n";
  if(base->elements[alia->index].empty() ) {
    base->elements[alia->index].push_back(store_inst);
  } 
  //if the before store inst has the same pre block, indicate
  //it is if store else store
  //else just cover the before store inst
  else {
    BasicBlock *b1 = store_inst->getParent();
    BasicBlock *b2 = base->elements[alia->index].back()->getParent();
    if(b1 == b2 || b1->getSinglePredecessor() == NULL || b1->getSinglePredecessor() == b2) {
      base->elements[alia->index].clear();
      base->elements[alia->index].push_back(store_inst);
    }
    else {
      base->elements[alia->index].push_back(store_inst);
      //for(auto iter = base->elements[alia->index].begin(); iter != base->elements[alia->index].end(); ++iter) {
      //  outs() << "\nmay store:\n";
      //  (*iter)->dump();
      //}
    }
  }
 // IPTS *temp_ipts = new IPTS;      //create IPTS for curr inst
 // temp_ipts->first = curr;
 // temp_ipts->second = new std::set<PtrNode *>;
 // ipts_vec->push_back(temp_ipts);
}

//deal with load inst
void PTSVisitor::dealWithLoad(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias) {
  LoadInst *load_inst = cast<LoadInst>(curr);
  std::string alia_name = load_inst->getPointerOperand()->getName();
  std::string base_name;
  BaseAddr *base = NULL;
  AddrAlia *alia = NULL;
  for(auto iter = addr_alias->begin(); iter != addr_alias->end(); ++iter) {
    if(!alia_name.compare((*iter)->name) ) {
      alia = *iter; 
      break;
    }
  }
  if(alia != NULL) {
    base_name = alia->base;
  }
  else {            //should not occur
    outs() << "unrecongnized addr alia error!\n";
  }
  Function *func = curr->getFunction();
  base = gaddr[func][base_name];
  if(base == NULL) {
    outs() << "error! can not find the base addr\n";
  }
 for(auto iter = base->elements[alia->index].begin(); iter != base->elements[alia->index].end(); ++iter) {
   outs() << "\nmay load:\n";
   (*iter)->dump();
 }
 PtrNode *load_value = new PtrNode;
 load_value->id = curr->getName();
 load_value->type = POINTER_TYPE;
 load_value->pts = new std::set<PtrNode *>;
 for(auto iter = base->elements[alia->index].begin(); iter != base->elements[alia->index].end(); ++iter) {
   StoreInst *store_inst = *iter;
   std::string ptr_name = store_inst->getValueOperand()->getName();
   load_value->pts_name.insert(ptr_name);
 }
 outs() << "load value:";
 for(auto iter = load_value->pts_name.begin(); iter != load_value->pts_name.end(); ++iter) {
   outs() << *iter << "\t";
 }
 outs() << "\n";
 IPTS *temp_ipts = new IPTS;      //create IPTS for curr inst
 temp_ipts->first = curr;
 temp_ipts->second = new std::set<PtrNode *>;
 temp_ipts->second->insert(load_value);
 ipts_vec->push_back(temp_ipts);
}
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
   //visitor.printResult();
   //pts_info.printGPTS();
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

