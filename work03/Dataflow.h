/************************************************************************
 *
 * @file Dataflow.h
 *
 * General dataflow framework
 *
 ***********************************************************************/

#ifndef _DATAFLOW_H_
#define _DATAFLOW_H_

#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Function.h>
#include <set>
#include <map>
using namespace llvm;

///Base dataflow visitor class, defines the dataflow function
template <class T>
class DataflowVisitor {
public:
    virtual ~DataflowVisitor() { }

    virtual void computePTS(Function *F) {

    }

    /// Dataflow Function invoked for each basic block 
    virtual void computePTS(BasicBlock *block, T *dfval, bool isforward) {
        if (isforward == true) {
           for (auto inst_i = block->begin(); inst_i != block->end(); ++inst_i) {
                Instruction *inst = &*inst_i;
                computePTS(inst, dfval);
           }
        } 
        else {
           for (auto inst_i = block->rbegin(); inst_i != block->rend(); ++inst_i) {
                Instruction *inst = &*inst_i;
                computePTS(inst, dfval);
           }
        }
    }

    /// Dataflow Function invoked for each instruction
    virtual void computePTS(Instruction *inst, T *dfval ) = 0;
    /// Merge two PTS, dest will be ther merged result
    virtual void merge( T *dest, const T &src ) = 0;
};

/// Dummy class to provide a typedef for the detailed result set
/// For each basicblock, we compute its input dataflow val and its output dataflow val
template<class T>
struct DataflowResult {
    typedef typename std::map<BasicBlock *, std::pair<T, T> > Type;
};

template<class T>
void compForwardDataflow(Function *fn,
    DataflowVisitor<T> *visitor,
    typename DataflowResult<T>::Type *result,
    const T & initval) {
    return;
}

template<class T>
void compBackwardDataflow(Function *fn,
    DataflowVisitor<T> *visitor,
    typename DataflowResult<T>::Type *result,
    const T &initval) {
    std::set<BasicBlock *> worklist;
    // Initialize the worklist with all exist blocks
    for (Function::iterator bi = fn->begin(); bi != fn->end(); ++bi) {
        BasicBlock * bb = &*bi;
        result->insert(std::make_pair(bb, std::make_pair(initval, initval)));
        worklist.insert(bb);
    }
    // Iteratively compute the dataflow result : std::map<BasicBlock *, std::pair<T, T> >
    while (!worklist.empty()) {
        BasicBlock *bb = *worklist.begin();
        worklist.erase(worklist.begin());
        // Merge all incoming value
        T bbexitval = (*result)[bb].second;
        for (auto si = succ_begin(bb), se = succ_end(bb); si != se; si++) {
            BasicBlock *succ = *si;
            visitor->merge(&bbexitval, (*result)[succ].first);
        }
        (*result)[bb].second = bbexitval;
        visitor->compDFVal(bb, &bbexitval, false);
        // If outgoing value changed, propagate it along the CFG
        if (bbexitval == (*result)[bb].first) continue;
        (*result)[bb].first = bbexitval;
        for (pred_iterator pi = pred_begin(bb), pe = pred_end(bb); pi != pe; pi++) {
            worklist.insert(*pi);
        }
    }
}

#endif /* !_DATAFLOW_H_ */

