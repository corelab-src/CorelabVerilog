#ifndef CORELAB_VERILOG_PASS_H
#define CORELAB_VERILOG_PASS_H

#include "llvm/Pass.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"

#include "BBCDFG.h"
#include "VerilogConfig.h"
#include "NLT.h"
#include "LoopPatternAnal.h"
#include "FSM.h"
#include "PABuilder.h"

#include "MemoryTable.h"
#include "CallTable.h"
#include "OperatorTable.h"
#include "BitWidthTable.h"

#include "PrintVerilog.h"

using namespace llvm;

namespace corelab {

class CorelabVerilogPass : public ModulePass, public InstVisitor<CorelabVerilogPass> {
public:
    CorelabVerilogPass(raw_ostream &o) : ModulePass(ID), Out(o) {}

		virtual StringRef getPassName() const { return "Corelab Verilog Backend Pass"; }

    static char ID;

    /// doInitialization - Allocate RAMs for global variables
//    virtual bool doInitialization(Module &M);

    /// runOnFunction - schedule each function and create HwModule object
    bool runOnModule(Module &M);

    /// doFinalization - print the verilog
//    virtual bool doFinalization(Module &M);

		FSM *fsm;

private:

    virtual void getAnalysisUsage(AnalysisUsage &AU) const;

		void initAnalysis(void);

    raw_ostream &Out;

		Module *module;
		BBCDFG *bbcdfg;
		NLT *nlt;
		PADriver *pa;

		VerilogConfigInfo *verilogConfigInfo;
		LoopPatternAnal *LPA;
		
		DataLayout *DL;

		DenseMap<const Function *, LoopInfo *> loopInfoOf;

};

} // end namespace

#endif
