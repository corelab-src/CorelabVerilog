//===-- PADriver.h - Points-To Analysis -------------------*- C++ -*-===//
//
// This code was taken from:
//      https://code.google.com/p/addr-leaks/wiki/HowToUseThePointerAnalysis
//
//===----------------------------------------------------------------------===//
//
// Determines points-to analysis. Using the algorithm from the paper:
//      Ben Hardekopf and Calvin Lin. 2007. The ant and the grasshopper: fast
//      and accurate pointer analysis for millions of lines of code. In
//      Proceedings of the 2007 ACM SIGPLAN conference on Programming language
//      design and implementation (PLDI '07). ACM, New York, NY, USA, 290-299.
//
//===----------------------------------------------------------------------===//

//Editted by Changsu Kim

#ifndef PADRIVER_H
#define PADRIVER_H

#include "llvm/IR/Value.h"
#include "llvm/IR/Use.h"
#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/ADT/DenseMap.h"

#include <set>
#include <vector>
#include <string>


namespace corelab
{
using namespace llvm;

class PointerAnalysis;

// class PADriver : public ModulePass {
class PADriver : public ModulePass {
  public:
    // +++++ FIELDS +++++ //
    // Used to assign a int ID to Values and store names
		Module *module;
    int currInd;
    int nextMemoryBlock;
    DenseMap<Value *, int> value2int;
    DenseMap<int, Value *> int2value;

    DenseMap<Value *, int> valMap;
    DenseMap<Value *, std::vector<int>> valMem;
    DenseMap<int, std::string> nameMap;

    DenseMap<Value *, std::vector<int>> memoryBlock;
    DenseMap<int, Value *> int2mem;
    DenseMap<int, std::vector<int>> memoryBlock2;
    DenseMap<Value *, std::vector<Value *>> phiValues;
    DenseMap<Value *, std::vector<std::vector<int>>> memoryBlocks;
    unsigned int numInst;

		bool unResolved;
		bool hasUnResolved(void) { return unResolved; }

    static char ID;
    PointerAnalysis *pointerAnalysis;

		virtual void getAnalysisUsage(AnalysisUsage &AU) const;
		virtual StringRef getPassName() const { return "PA Builder"; }

    PADriver();

		PADriver *getPA(void) { return this; };

		std::set<Instruction *> getUserInstruction(Value *v) { return memory2UserI[v]; }
		std::set<Function *> getUserFunction(Value *v) { return memory2UserF[v]; }
		std::set<Value *> getUsedMemory(Function *f) { return userF2Memory[f]; }
		std::set<Value *> getUsedMemoryFromCallSite(Instruction *inst) { return call2Memory[inst]; }

		//Memory Table
		std::set<Value *> getPointedMemory(Value *v) { return pointer2Memory[v]; }

		//Memory
		DenseMap<int, std::set<int>> memory2Pointed;
		DenseMap<Value *, std::set<Instruction *>> memory2UserI;
		DenseMap<Value *, std::set<Function *>> memory2UserF;
		DenseMap<Function *, std::set<Value *>> userF2Memory;

		//Memory Table
		DenseMap<Value *, std::set<Value *>> pointer2Memory;

		//Callsite aware memory tracking
		//CallInst or InvokeInst 2 Memory Value
		//call Insts argument check
		DenseMap<Instruction *, std::set<Value *>> call2Memory;

		//Alias Method
		bool isNoAlias(Value *, Value *);

		std::set<int> getPointsToSet(Value *);

    // +++++ METHODS +++++ //

    bool runOnModule(Module &M);
    int Value2Int(Value *v);
    void findAllPointerOperands(User *U, std::set<Value *> &ptrs);
    void handleGetElementPtrConst(Value *op);
    int getNewMem(std::string name);
    int getNewInt();
    int getNewMemoryBlock();
    void handleNestedStructs(const Type *Ty, int parent);
    void handleAlloca(Instruction *I);
    void handleGlobalVariable(GlobalVariable *G);
    void handleGetElementPtr(Instruction *I);
    // Value* Int2Value(int);
    virtual void print(raw_fd_ostream &O, const Module *M) const;
    std::string intToStr(int v);
    void process_mem_usage(double &vm_usage, double &resident_set);
    void addConstraints(Function &F);
    void matchFormalWithActualParameters(Function &F);
    void matchReturnValueWithReturnVariable(Function &F);
		void checkMemoryPrivate();
		void handleOperator(Value *);
};
}
#endif
