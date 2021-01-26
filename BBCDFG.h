#ifndef BBCDFG_H
#define BBCDFG_H

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instruction.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Analysis/AliasAnalysis.h"

#include "PABuilder.h"
#include "PABasedAA.h"

#include <set>

namespace corelab
{
using namespace llvm;
using namespace std;

	typedef set<Instruction *> InstructionSet;
	typedef DenseMap<Instruction *, InstructionSet> BBCDFG_Map;

	class BBCDFG : public ModulePass 
	{
		public:
			BBCDFG() : ModulePass(ID) {}

			virtual StringRef getPassName() const { return "BB CDFG"; }

			static char ID;

			virtual void getAnalysisUsage(AnalysisUsage &AU) const;
			bool runOnModule(Module &F);

			BBCDFG_Map getRegDepMap(void) { return regDepMap; };
			BBCDFG_Map getRegUseMap(void) { return regUseMap; };
			BBCDFG_Map getMemDepMap(void) { return memDepMap; };
			BBCDFG_Map getMemUseMap(void) { return memUseMap; };

			BBCDFG *getBBCDFG(void) { return this; };

		private:

			void genBBCDFG_Map(void);

			void genBBCDFG_Reg_Map_for_Inst(Instruction *inst);
			void genBBCDFG_Reg_Map_for_Network(Instruction *inst);
			void genBBCDFG_Reg_Map_for_Thread(Instruction *inst);

			void get_BBCDFG_File(void);

			raw_fd_ostream *regMapFile;
			raw_fd_ostream *memMapFile;

			AAResults *aa;
			Module *module;
			PADriver *pa;
			DenseMap<const Function *, LoopInfo *> loopInfoOf;

			PABasedAA *paaa;

			BBCDFG_Map regDepMap;
			BBCDFG_Map regUseMap;
			BBCDFG_Map memDepMap;
			BBCDFG_Map memUseMap;
	};

} //end namespace

#endif
