#ifndef _DATAFLOW_H_
#define _DATAFLOW_H_

#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Function.h>
#include <set>
#include <map>
using namespace llvm;

///Base PTS visitor class, defines the PTS analysis function
template <class T>
class DataflowVisitor {
public:
    virtual ~DataflowVisitor() { }
    // PTS analysis Function invoked for each function 
    // to compute basis pts of each inst in the function
    virtual void computePTS(Function *F, T *gpts) = 0;
    // PTS analysis Function invoked for calculating
    // global pts of each inst, only intral procedure
    virtual void computeGPTS(T *gpts) = 0;
    // PTS analysis Function invoked for calculating
    // globale pts of each inst, inter procudure
    virtual void interProcedurePTSTransfer(T *gpts) = 0;
};

template<class T>
void compForwardDataflow(Function *fn,
  DataflowVisitor<T> *visitor,
  T &pts_info) {
  return;
}

//@TODO PTS analysis is forward, don't need to complete at present
template<class T>
void compBackwardDataflow(Function *fn,
  DataflowVisitor<T> *visitor,
  T &pts_info) {
  return;
}

#endif /* !_DATAFLOW_H_ */

