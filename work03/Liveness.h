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
#include<cstdlib>
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
  void dump() {
    outs() << "id: " << id << "\n";
    outs() << "type: " << type << "\n";
    outs() << "pts_name size: " << pts_name.size() << "\n";
    if(pts == NULL)
      outs() << "pts is NULL\n";
    else
      outs() << "pts size: " << pts->size() << "\n";
  }
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
  unsigned type;                                  //0 indicates the addr of value, 1 indicates the addr of addr
  bool is_param;                                  //true incates it is a param
  BaseAddr *real_base;                             //when is_param = true, represent its real base addr 
  std::vector<std::vector<StoreInst *> > elements;   //use store op to record the newest value of the addr
  BaseAddr() : id(), len(-1), type(-1), is_param(false), real_base(NULL), elements() {}
  void dump() {
    outs() << "id: " << id << "\n";
    outs() << "len: " << len << "\n";
  }
};

//alia of a addr
struct AddrAlia {
  std::string name;                                 //addr alia
  std::string base;                                 //alia of which base
  unsigned index;                                   //along with base to lacate the real addr
  AddrAlia() : name(), base(), index(-1) {}
  void dump() {
    outs() << "alia name: " << name << "\n";
    outs() << "base: " << base << "\n";
    outs()  << "index: " << index << "\n";
  }
};

void dump(IPTS *ipts) {
  if(ipts->first != NULL)
    ipts->first->dump();
  else 
    outs() << "NULL inst\n";
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
 //global map of function name to function
 std::map<std::string, Function *> func_name_to_func;
 //global base addr. get by AllocaInst and malloc function
 std::map<Function *, std::map<std::string, BaseAddr *> > gaddr;
 //load type variable alia name, for load variable may be unnamed temprary
 std::map<Instruction *, std::string> unnamed_variable_name;
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
 //get point to function from given PtrNode
 void getFuncFromPtr(std::set<std::string> &func, PtrNode *ptr);
 //generate name for unnamed variable, i.e load inst
 void generateName(Instruction *inst);
 //find the relationship of two given blocks, ckeck if the first dominate the second
 //ought to generate dominator tree, but I am tired to this now and llvm has this function
 //but I ca not use in this homework, it is of no use bothering to do this
 bool isDominated(BasicBlock *father, BasicBlock *son);
 bool findWayToEntry(BasicBlock *entry, BasicBlock *me, BasicBlock *father);
 
private:
 //if param is base addr, add it to gaddr
 void initBaseAddr(Function *F);
 //
 void createArgIpts(Function *F, IPTS *arg_ipts);
 //get real arg from called, if changed gpts, return 1
 int getParamsFromCalled(CallInst *call_inst, IPTS *ipts, PTSInfo *gpts);
 //get called function from a call_inst, may not be only one function
 void getFunctionFromCallInst(CallInst *call_inst, IPTS *ipts, std::set<Function *> &called_func_set);
 //transfer the arg PtrNode to the called func
 void transferParamToFunc(std::vector<PtrNode *> &args, std::vector<int> index, FPTS *fpts);
 //merge
 void merge(IPTS *dest, IPTS *src);
 //deal with llvm.dbg.value
 void dealWithIntrinsicValue(Instruction *curr, std::vector<IPTS *> *ipts_vec);
 //deal with llvm.memcpy
 void dealWithMemcpy(Instruction *curr, std::vector<IPTS *> *ipts_vec);
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
 //deal with bitcast, it is actually a addr alia, same as getElementPtr, I don't know why llvm do like this
 void dealWithBitCast(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias);
 //deal with store inst
 void dealWithStore(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias);
 //deal with load inst
 void dealWithLoad(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias);
 //deal with instructions that will not change pts
 void dealWithOthers(Instruction *curr, std::vector<IPTS *> *ipts_vec);
 //get the inst's IPTS
 IPTS *getIPTS(Instruction *inst, PTSInfo *gpts);
};

//function to compute PTS of a given function
void PTSVisitor::computePTS(Function *F, PTSInfo *pts_info) {
  if(F->isIntrinsic() || F->getName() == "malloc") 
    return;
  //add global information to gfuncs, gaddr, func_name_to_func;
  //outs() << F->getName() << "\n";
  PtrNode *func_ptr = new PtrNode;
  func_ptr->id = F->getName();
  func_ptr->type = VALUE_TYPE;
  gfuncs[F->getName()] = func_ptr;
  std::map<std::string, BaseAddr *> base_addrs;
  gaddr[F] = base_addrs;
  func_name_to_func[F->getName()] = F;
  //create FPTS andaddr alias  for the function
  FPTS *fpts = new FPTS;
  std::set<AddrAlia *> *addr_alias = new std::set<AddrAlia *>;
  std::vector<BPTS *> *bpts_vec = new std::vector<BPTS *>;
  initBaseAddr(F);
  for(auto block_i = F->begin(); block_i != F->end(); ++block_i) {
    BasicBlock *block = cast<BasicBlock>(block_i);
    computeBPTS(block, bpts_vec, addr_alias);
  }
  fpts->first = F;
  fpts->second = bpts_vec;
  pts_info->gpts.push_back(fpts);
  //
  BPTS *entry_bpts = bpts_vec->front();
  IPTS *arg_ipts = new IPTS;
  createArgIpts(F, arg_ipts);
  entry_bpts->second->insert(entry_bpts->second->begin(), arg_ipts);
}


//if param is base addr, add it to gaddr
void PTSVisitor::initBaseAddr(Function *F) {
  for(auto iter = F->arg_begin(); iter != F->arg_end(); iter++) {
    if(iter->getType()->isPointerTy() ) {                         
      if(iter->getType()->getPointerElementType()->isPointerTy() ) {
        //param is addr, add it to gaddr
        BaseAddr *base = new BaseAddr;
        base->id = iter->getName();
        base->is_param = true;
        gaddr[F][iter->getName()] = base;
        //outs() << "type: ";
        //iter->getType()->dump();
        //outs() << "point to: ";
        //iter->getType()->getPointerElementType()->dump();
      }
    }
  }
}
//create a PtrNode and initialize it as "NULL" for each pointer
//parameter, when a call occurs, we use real_arg to initialize 
void PTSVisitor::createArgIpts(Function *F, IPTS *arg_ipts) {
  //if param is a pointer, create PtrNode for it
  arg_ipts->first = NULL;
  arg_ipts->second = new std::set<PtrNode *>;
  for(auto iter = F->arg_begin(); iter != F->arg_end(); iter++) {
    if(iter->getType()->isPointerTy() ) {
      PtrNode *arg = new PtrNode;
      arg->id = iter->getName();
      arg->type = ARG_TYPE | iter->getArgNo();
      arg->pts = new std::set<PtrNode *>;
      //outs() << "arg index:" << iter->getArgNo() << "\n";
      arg_ipts->second->insert(arg);
    }
  }
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
          //there is also succ_begin() / succ_end(), remember for later use
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
              //errs() << "merge error! pre BPTS is empty\n";
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
void PTSVisitor::interProcedurePTSTransfer(PTSInfo *gpts_info) {
  computeGPTS(gpts_info);
  for(auto iter = call_inst_vec.rbegin(); iter != call_inst_vec.rend(); ++iter) {
    IPTS *curr_ipts = getIPTS(*iter, gpts_info);
    CallInst *call_inst = cast<CallInst>(*iter);
    getParamsFromCalled(call_inst, curr_ipts, gpts_info);
  }
  computeResult(gpts_info);
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
      if(ptr_name.empty() ) {             //an unnamed ptr, i.e like %1
        if(unnamed_variable_name.find(cast<LoadInst>(call) ) != unnamed_variable_name.end() ) {
          ptr_name = unnamed_variable_name[cast<LoadInst>(call)]; 
        }
        else {
          errs() << __LINE__ << ": Error001! Unrecongized unnamed call ptr\n";
        }
      }
      PtrNode *called_ptr = NULL;
      for(auto iter2 = ipts->second->begin(); iter2 != ipts->second->end(); ++iter2) {
        if(ptr_name.compare((*iter2)->id) == 0 ) {
          called_ptr = *iter2;
          break;
        }
      }
      if(called_ptr == NULL) {
        errs() << __LINE__ << ": Error009!" << ptr_name << " Can not find called function\n";
        exit(-1);
      }
      std::set<std::string> func_name;
      getFuncFromPtr(func_name, called_ptr);
      for(auto iter3 = func_name.begin(); iter3 != func_name.end(); ++iter3) {
        temp.name.insert(*iter3);
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
        case Intrinsic::memcpy:      dealWithMemcpy(curr, ipts_vec); 
                                     break;
        default:                     errs() << "unrecognized intrinsic error\n";
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
  //deal with instructions that may change pts, like phi
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
  else if(IS("bitcast") ) {
    dealWithBitCast(curr, ipts_vec, addr_alias);
  }
  else {                                //deal with instructions that will not change pts
    dealWithOthers(curr, ipts_vec);
  }
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

//get pointed to function from given PtrNode
void PTSVisitor::getFuncFromPtr(std::set<std::string> &func, PtrNode *ptr) {
  if(ptr->type == VALUE_TYPE) {
    func.insert(ptr->id);
    return;
  }
  if(ptr->pts->empty() ) {
    errs() << __LINE__ << ": Error000! Empty PtrNode error: ";
    errs() << ptr->id << " : pts is empty\n";
    //exit(-1);
  }
  for(auto iter = ptr->pts->begin(); iter != ptr->pts->end(); ++iter) {
    getFuncFromPtr(func, *iter);
  }
}

//generate name for unnamed variable
void PTSVisitor::generateName(Instruction *inst) {
  static int ID = 0;
  char id[10];
  std::string name(inst->getOpcodeName() );
  sprintf(id, "%d", ID);
  name.append(id);
  unnamed_variable_name[inst] = name; 
  ID++;
}

//judge if father dominates son
bool PTSVisitor::isDominated(BasicBlock *father, BasicBlock *son) {
  if(father->getParent() != son->getParent() ) {
    errs() << __LINE__ << ": Error007! Can not judge relationship of blocks of different funtion.\n";
  }
  Function *F = father->getParent();
  BasicBlock *entry = &(F->getEntryBlock());
  if(findWayToEntry(entry, son, father) )
    return false;
  return true;     
}
//may be wrong... I am to tired, it's 3 o'clock in morning now
bool PTSVisitor::findWayToEntry(BasicBlock *entry, BasicBlock *me, BasicBlock *father) {
  if(me == entry) {
    return true;
  }
  for(auto pre = pred_begin(me); pre != pred_end(me); ++pre) {
    if(*pre != father)
      return findWayToEntry(entry, *pre, father);
  }
  return false;
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

//get called function from a call_inst, may not be only one function
void PTSVisitor::getFunctionFromCallInst(CallInst *call_inst, IPTS *ipts, std::set<Function *> &called_func_set) {
  if(Function *func = call_inst->getCalledFunction() ) {
     called_func_set.insert(func);                           //direct function call
   }
   else {                                                       //indirect function call
     Value *called_value = call_inst->getCalledValue();
     std::string called_name = called_value->getName();
     if(called_name.empty() ) {                      //unnamed ptr, like %1 = load ..., it is a load value
       if(LoadInst *load_inst = dyn_cast<LoadInst>(called_value) ) {
         called_name = unnamed_variable_name[load_inst];        //get the generated name
       }
       else {                                        //should not happen
         errs() << __LINE__ << " Error011! Unrecongized unnamed variable error.\n";
       }
     }
     //get the called_value PtrNode
     PtrNode *called_ptr = NULL;
     for(auto iter = ipts->second->begin(); iter != ipts->second->end(); ++iter) {
       if((*iter)->id.compare(called_name) == 0) {
         called_ptr = *iter;
         break;
       }
     }
     if(called_ptr == NULL) {
       errs() << __LINE__ << ": Error009!" << called_name << " Can not find called function\n";
       exit(-1);
     }
     std::set<std::string> func_name;
     //get the called function and do param tranfer for each may-called-func
     getFuncFromPtr(func_name, called_ptr);
     for(auto iter1 = func_name.begin(); iter1 != func_name.end(); ++iter1) {
       called_func_set.insert(func_name_to_func[*iter1]);
     }
   }
}

//get real arg from called, if changed gpts, return 1
int PTSVisitor::getParamsFromCalled(CallInst *call_inst, IPTS *ipts, PTSInfo *gpts) {
  //get called Function
  std::set<Function *> called_func_set;
  getFunctionFromCallInst(call_inst, ipts, called_func_set);
 //get real arg from each function call instance
  for(auto func_iter = called_func_set.begin(); func_iter != called_func_set.end(); ++func_iter) {
    Function *func = *func_iter;
    if(func->getName() == "malloc")       //no need to deal with malloc
      continue;
    std::vector<PtrNode *> args;                                 //arg list
    std::vector<int> args_index;                                 //arg index
    //get the called func's FPTS
    FPTS *fpts = NULL;
    for(auto iter = gpts->gpts.begin(); iter != gpts->gpts.end(); ++iter) {
      if(func == (*iter)->first) {
        fpts = *iter;
        break;
      }
    }
    if(fpts == NULL) {          //should not happen
      errs() << __LINE__ << " Error010! Can not find called Function!\n";
      exit(-1);
    }
    //get the args
    unsigned num = call_inst->getNumArgOperands();
    for(int i = 0; i < num; ++i) {
      Value *arg = call_inst->getArgOperand(i);
      //only need to deal with function pointer parameters
      if(arg->getType()->isPointerTy() ) {
       // arg->getType()->dump();
        std::string arg_name = arg->getName();
        if(arg_name.empty() )    {         //param is unnamed value, like %1
          arg_name = unnamed_variable_name[cast<Instruction>(arg)];
        }
        if(gfuncs.find(arg_name) != gfuncs.end() ) {         //func addr as param
          args.push_back(gfuncs.find(arg_name)->second);
          args_index.push_back(i);
        }
        else {                                               //func ptr as param
          for(auto it = ipts->second->begin(); it != ipts->second->end(); ++it) {
            if(arg_name.compare((*it)->id) == 0) {
              //outs() << (*it)->id << "\n";
              args.push_back(*it);
              args_index.push_back(i);
              break;
            }
          }
        }
      }
    }
    if(args.size() != 0) {
      if(args.size() != args_index.size() ) {     //should not happen
        errs() << __LINE__ << ": Error014! Param transfer error!\n";
        exit(-1); 
      }
      transferParamToFunc(args, args_index, fpts);
      computeResult(gpts);
    }
  }
  return 0;
}

//transfer the arg PtrNode to the called func
void PTSVisitor::transferParamToFunc(std::vector<PtrNode *> &args, std::vector<int> args_index, FPTS *fpts) {
  outs() << "transfer start\n";
  //the arg must in the entry block
  BPTS *entry_bpts = fpts->second->front();
  //transfer the args list to the param list
  auto iter1 = args.begin();
  auto iter2 = args_index.begin();
  for( ;iter1 != args.end(); ++iter1, ++iter2) {
    bool completed = false;
    PtrNode *arg = *iter1;
    int index = *iter2;
    IPTS *ipts = entry_bpts->second->front();
    for(auto iter = ipts->second->begin(); iter != ipts->second->end(); ++iter) {
      PtrNode *param = *iter;
      if((param->type & 0x80 ) &&(param->type & 0xf) == index ) {
        //delete the old call inst arg
        param->pts_name.clear();
        param->pts->clear();
        //pass the new arg
        param->pts_name.insert(arg->id);
        param->pts->insert(arg);
        outs() << param->id << " get " <<  arg->id << " transfer complete!\n";
        completed = true;
        break;
      }
    }
    if(!completed) {   //should not happen
      errs() << __LINE__  << ": Error013. Param Transfer error, errro param index: " << index << "\n";
    }
  }
  outs() << "transfer over!\n";
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
      if(temp->id.compare((*iterd)->id) == 0) {    
        new_var = false;
        break;
      }
    }
    if(new_var) {
       dest_pts->insert(temp);
    }
  }
  //stage 2 merge
  for(auto iter1 = dest_pts->begin(); iter1 != dest_pts->end(); ++iter1) {
    PtrNode *temp = *iter1;
    if(temp->pts->size() != temp->pts_name.size() ) {
      for(auto iter2 = temp->pts_name.begin(); iter2 != temp->pts_name.end(); ++iter2) {
        bool flag = false;
        std::string ptr_name = *iter2;
        if(gfuncs.find(ptr_name) != gfuncs.end() )  //any func addr must have been physically linked to the object PtrNode
          continue;
        for(auto iter3 = dest_pts->begin(); iter3 != dest_pts->end(); ++iter3) {
          if(ptr_name.compare((*iter3)->id) == 0) {
            temp->pts->insert(*iter3);
            flag = true;
            break;
          }
        }
        if(!flag) {
          errs() << __LINE__ << ": Error008!" << ptr_name << " Can find object PtrNode in the IPTS when merge stage 2.\n";
        }
      }
    }
  }
}

//deal with llvm.dbg.value
//get the declare pointer and init this inst pts, then add to this block pts
void PTSVisitor::dealWithIntrinsicValue(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
  IPTS *temp_ipts = new IPTS;      //create IPTS for curr inst
  temp_ipts->first = curr;
  temp_ipts->second = new std::set<PtrNode *>;
  ipts_vec->push_back(temp_ipts);
  CallInst *call_intrinsic = cast<CallInst>(curr);
  //The first argument is the new value (wrapped as metadata)
  DILocalVariable *var_meta = cast<DILocalVariable>((cast<MetadataAsValue>(call_intrinsic->getOperand(2) ) )->getMetadata() );
  std::string var_name = var_meta->getName();
  //pointer type
  if( (cast<DIType>(var_meta->getRawType() ) )->getTag() == dwarf::DW_TAG_pointer_type) {        
    if(var_meta->getArg() == 0) {       //it is a local variable, we have dealed with params at createArgIpts
      PtrNode *temp_node = new PtrNode;
      temp_node->id = var_name; 
      temp_node->pts = new std::set<PtrNode *>;                  //init as null pts
      Metadata *temp = cast<Metadata>(cast<MetadataAsValue>(call_intrinsic->getOperand(0) )->getMetadata() );
      Value *var_value = cast<ValueAsMetadata>(temp)->getValue();
      std::string var_value_name = var_value->getName();
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
      temp_ipts->second->insert(temp_node);
    }
  }
}

//deal with llvm.memcpy
void PTSVisitor::dealWithMemcpy(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
  CallInst *call_memcpy = cast<CallInst>(curr);
  Value *dest = call_memcpy->getOperand(0);
  Value *src = call_memcpy->getOperand(1);
  std::string dest_name = dest->getName();
  std::string src_name = src->getName();
  if(src_name.empty() && dest_name.empty() ) {
    dest_name = unnamed_variable_name[cast<Instruction>(dest)];
    src_name = unnamed_variable_name[cast<Instruction>(src)];
  }
  BaseAddr *dest_base = NULL, *src_base = NULL;
  dest_base = gaddr[curr->getFunction()][dest_name];
  src_base = gaddr[curr->getFunction()][src_name];
  if(dest_base == NULL || src_base == NULL) {
    errs() << __LINE__ << ": Error012! Wrong memcpy src or dest.\n";
    exit(-1);
  }
  else {            //@TODO do copy, assume both are structs, and both are one store
    dest_base->elements[0].push_back(src_base->elements[0].back() );
  }
}

//deal with llvm.dbg.declare
void PTSVisitor::dealWithIntrisicDeclare(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
  CallInst *call_intrinsic = cast<CallInst>(curr);
 //@TODO 
}
//deal with phi node
void PTSVisitor::dealWithPhi(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
  //@TODO if it is not the object type PhiNode, we need not to deal with it
  //object PhiNode: array, pointer, struct ... we can not decide now
  //but we can decide which is not the object type and exclude it: find one, add one
  PHINode *phi = cast<PHINode>(curr);
  if(phi->getIncomingValue(0)->getType()->isIntegerTy() ) {     //  need not to deal with int
    IPTS *temp_ipts = new IPTS;      //create empty IPTS for curr inst
    temp_ipts->first = curr;
    temp_ipts->second = new std::set<PtrNode *>;
    ipts_vec->push_back(temp_ipts);
    return;
  }
  std::string var_name = curr->getName();
  PtrNode *temp_node = new PtrNode;               
  temp_node->id = var_name;
  temp_node->type = PHI_TYPE;                 
  temp_node->pts = new std::set<PtrNode *>;
  for(int i = 0; i < phi->getNumIncomingValues(); ++i) {
     Value *phi_value = phi->getIncomingValue(i);
     std::string var_value_name = phi_value->getName();
     if(!var_value_name.empty() ) {      //deal with NON_NULL value
       //logically point to, if it is not a func ptrnode, do physically point to when merge
       temp_node->pts_name.insert(var_value_name);       
       if(gfuncs.find(var_value_name) != gfuncs.end() )            //it is a func addr, physically point to
         temp_node->pts->insert(gfuncs.find(var_value_name)->second); 
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
  if(Function *func = dyn_cast<CallInst>(curr)->getCalledFunction() ) {
    if(func->getName() == "malloc") {
      BaseAddr *base = new BaseAddr;
      //@TODO defalut as 0, more complicate cases need to be dealed with
      //i.e malloc a struct or something else
      base->type = 0;
      Function *func = curr->getFunction();
      std::string name = curr->getName();          //addr name
      base->id = name;
      //@TODO if malloc more than one element, we shou get the number
      //it is not difficult, may be get the arg of the malloc and divide
      //the size of one element will get the number. I just don't bother 
      //to do it now. It is set as default value 1
      base->len = 1;
      std::vector<std::vector<StoreInst *> > elements;
      base->elements = elements;
      for(int i = 0; i < base->len; ++i) {
        std::vector<StoreInst *> store_op;
        base->elements.push_back(store_op);
      }
      gaddr[func][name] = base;
    }
  }
}
//deal with alloca inst
void PTSVisitor::dealWithAlloca(Instruction *curr) {
  BaseAddr *base = new BaseAddr;
  Function *func = curr->getFunction();
  std::string name = curr->getName();          //addr name
  AllocaInst *alloca = cast<AllocaInst>(curr);
  Type *alloca_type = alloca->getAllocatedType();
  base->type = 0;           //default as function pointer type
  if(alloca_type->isStructTy() ) {
    Type *element_type = alloca_type->getStructElementType(0);      //only one element, a pointer
    //@TODO try to know is a function pointer or a struct pointer
    if(cast<PointerType>(element_type)->getElementType()->isStructTy() ) {          //struct pointer
      base->type = 1;
    }
  }
  //@TODO array, struct, struct array
  //len default as 1
  unsigned len = 1;
  if(alloca_type->isArrayTy() ) {
    len = alloca->getAllocatedType()->getArrayNumElements();
  }
  base->id = name;
  base->len = len;
  //outs() << "init len:" << len << "\n";
  std::vector<std::vector<StoreInst *> > elements;
  base->elements = elements;
  for(int i = 0; i < len; ++i) {
    std::vector<StoreInst *> store_op;
    base->elements.push_back(store_op);
  }
  gaddr[func][name] = base;
}
//deal with getElementPtr inst
void PTSVisitor::dealWithGetElementPtr(Instruction *curr, std::set<AddrAlia *> *addr_alias) {
  AddrAlia *alia = new AddrAlia;
  alia->name = curr->getName();
  GetElementPtrInst *get_ptr = cast<GetElementPtrInst>(curr);
  //@TODO
  //base may be a unnamed variable from a load
  Value *base_addr = get_ptr->getPointerOperand();
  std::string base_name = base_addr->getName();
  if(base_name.empty() ) {        
    base_name = unnamed_variable_name[cast<Instruction>(base_addr)]; 
  }
  alia->base = base_name;
  unsigned index_num = get_ptr->getNumIndices();
  //@TODO multi-dimention index
  //the first operand is PointerOperand, and the first index is always 0
  for(int i = 2; i < index_num + 1; ++i) {
     alia->index = cast<ConstantInt>(get_ptr->getOperand(i) )->getZExtValue(); 
  }
  addr_alias->insert(alia);
}

 //deal with bitcast, it is actually a addr alia, same as getElementPtr, I don't know why llvm do like this
void PTSVisitor::dealWithBitCast(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias) {
  BitCastInst *bit_cast = cast<BitCastInst>(curr);
  Type *src_type = bit_cast->getSrcTy();
  Type *dest_type = bit_cast->getDestTy();
  bool flag1 = false, flag2 = false;
  if(src_type->isPointerTy() ) {
    if(src_type->getPointerElementType()->isStructTy() ) 
      flag1 = true;
  }
  if(dest_type->isPointerTy() ) {
    if(dest_type->getPointerElementType()->isStructTy() ) 
      flag2 = true;
  }
  if(flag1 || flag2) {
    std::string base_name = curr->getOperand(0)->getName(); 
    if(base_name.empty() ) {            //cast a unnamed variable
      Value *temp = curr->getOperand(0);
      base_name = unnamed_variable_name[cast<Instruction>(temp)];
    }
    unnamed_variable_name[curr] = base_name;
    //check if it is a struct that has a pointer of struct as element, that means its type should be 1
    if(flag1) {
      Type *type = src_type->getPointerElementType()->getStructElementType(0);
      if(type->getPointerElementType()->isStructTy() ) {
        gaddr[curr->getFunction()][base_name]->type = 1;
      }
    }
    if(flag2) {
      Type *type = dest_type->getPointerElementType()->getStructElementType(0);
      if(type->getPointerElementType()->isStructTy() ) {
        gaddr[curr->getFunction()][base_name]->type = 1;
      }
    }
  }
  else {
    AddrAlia *alia = new AddrAlia;
    generateName(curr);
    alia->name = unnamed_variable_name[curr];
    alia->base = curr->getOperand(0)->getName();
    //@TODO default as 0
    alia->index = 0;
    addr_alias->insert(alia);
  }
}

//deal with store inst
void PTSVisitor::dealWithStore(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias) {
  StoreInst *store_inst = cast<StoreInst>(curr);
  Value *pointer_operand = store_inst->getPointerOperand();
  std::string alia_name = pointer_operand->getName();
  if(alia_name.empty() ) {        //store at unnamed addr, i.e store @plus %0
    alia_name = unnamed_variable_name[cast<Instruction>(pointer_operand)];
  }
  BaseAddr *base = NULL;
  AddrAlia *alia = NULL;
  //get alia
  for(auto iter = addr_alias->begin(); iter != addr_alias->end(); ++iter) {
    if(!alia_name.compare((*iter)->name) ) {
      alia = *iter; 
      break;
    }
  }
  if(alia != NULL) {                 //get base
    base = gaddr[curr->getFunction()][alia->base];
  }
  else {       //should not occur
    errs() << __LINE__ << ": Error005! Unrecongized alia error when store.\n";
    exit(-1);
  }
  if(base == NULL) {    //should not occur
    errs() << __LINE__ << ": Error006!" << alia->base << " can not find the base addr when store.\n";
    curr->dump();
    exit(-1);
  }
  if(base->elements[alia->index].empty() ) {
    base->elements[alia->index].push_back(store_inst);
  } 
  //if the before store inst has the same pre block, indicate
  //it is if store else store
  //else just cover the before store inst
  else {
    BasicBlock *b1 = store_inst->getParent();
    BasicBlock *b2 = base->elements[alia->index].back()->getParent();
    if(b1 == b2 || b1->getSinglePredecessor() == b2) {
      base->elements[alia->index].clear();
      base->elements[alia->index].push_back(store_inst);
    }
    else {
      outs() << "multivalue\n";
      base->elements[alia->index].push_back(store_inst);
    }
  }
  IPTS *temp_ipts = new IPTS;      //create IPTS for curr inst
  temp_ipts->first = curr;
  temp_ipts->second = new std::set<PtrNode *>;
  ipts_vec->push_back(temp_ipts);
}

//deal with load inst
void PTSVisitor::dealWithLoad(Instruction *curr, std::vector<IPTS *> *ipts_vec, std::set<AddrAlia *> *addr_alias) {
  outs() << "load start\n";
  LoadInst *load_inst = cast<LoadInst>(curr);
  Value *pointer_operand = load_inst->getPointerOperand();
  std::string alia_name = pointer_operand->getName();
  if(alia_name.empty() ) {
    alia_name = unnamed_variable_name[cast<Instruction>(pointer_operand)];
  }
  BaseAddr *base = NULL;
  AddrAlia *alia = NULL;
  //get alia, it must have been occur in a store inst or it is an error
  for(auto iter = addr_alias->begin(); iter != addr_alias->end(); ++iter) {
    if(!alia_name.compare((*iter)->name) ) {
      alia = *iter; 
      break;
    }
  }
  if(alia != NULL) {      //get base
    base = gaddr[curr->getFunction()][alia->base];
  }
  else {            //should not occur
    errs() << __LINE__ << ": Error002! Unrecongnized addr alia error when load.\n";
    errs() << "error load: ";
    curr->dump();
    exit(-1);
  }
  if(base == NULL) {   //should not occur
    errs() << __LINE__ << ": Error003! Can not find the base addr when load.\n";
    errs() << "error load: ";
    curr->dump();
    exit(-1);
  }
  IPTS *temp_ipts = new IPTS;      //create IPTS for curr inst
  temp_ipts->first = curr;
  temp_ipts->second = new std::set<PtrNode *>;
  ipts_vec->push_back(temp_ipts);
  if(base->type == 0) {             //it is a function pointer
    PtrNode *load_value = new PtrNode;
    if(curr->getName().empty() ) {     //load value is unnamed
      generateName(curr);              //generate a name for the unname load value
      load_value->id = unnamed_variable_name[curr];
    }
    else {
      load_value->id = curr->getName();    //load value is named
    }
    load_value->type = POINTER_TYPE;
    load_value->pts = new std::set<PtrNode *>;
    //@TODO uncertain if store value can really pass to load
    //there may be cases that the store value can not find because it is not passed along the way
    //and we can not do a right phycally point to
    for(auto iter = base->elements[alia->index].begin(); iter != base->elements[alia->index].end(); ++iter) {
      StoreInst *store_inst = *iter;
      Value *store_value = store_inst->getValueOperand();
      std::string ptr_name = store_value->getName();
      if(ptr_name.empty() ) {    //that means we have store a unnamed variable, i.e a value from loaded
        store_inst->dump();
        if(LoadInst *temp_load = dyn_cast<LoadInst>(store_value) ) {
          ptr_name = unnamed_variable_name[temp_load];
        }
        else {
          errs() << __LINE__ << ": Error004! Unrecongnized load value.\n";
          exit(-1);
        }
      }
      //do a logically point to
      load_value->pts_name.insert(ptr_name);
      //it ia a func PtrNode, do a physically point to
      if(gfuncs.find(ptr_name) != gfuncs.end() ) {
        load_value->pts->insert(gfuncs.find(ptr_name)->second);
      }
    }
    temp_ipts->second->insert(load_value);
  }
  else if(base->type == 1) {       //nothing to say, it is not beautiful now
      outs() << "here\n";
      StoreInst *store_inst = base->elements[alia->index].back();
      Value *store_value = store_inst->getValueOperand();
      unnamed_variable_name[curr] = store_value->getName();
  }
  else if(base->type == -1) {
    outs() << "uninitialized base addr\n";
  }
}

//deal with instructions that will not change pts
void PTSVisitor::dealWithOthers(Instruction *curr, std::vector<IPTS *> *ipts_vec) {
  //do nothing, just create a empty IPTS, and at merge stage, inherite from its pres 
  IPTS *temp_ipts = new IPTS;
  temp_ipts->first = curr;
  temp_ipts->second = new std::set<PtrNode *>;
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
   visitor.printResult();
   //pts_info.printGPTS();
   return false;
 }

};

