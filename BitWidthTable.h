#ifndef CORELAB_BWT_H
#define CORELAB_BWT_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Analysis/LoopInfo.h"

#include "MemoryTable.h"
#include "CallTable.h"
#include "PABuilder.h"
#include "PABasedAA.h"

#include <list>
#include <set>

namespace corelab
{
	using namespace llvm;
	using namespace std;

	class BitWidthTable 
	{
		public:
			BitWidthTable(unsigned pointerBitSize_) : pointerBitSize(pointerBitSize_) {}
			~BitWidthTable() {}

			// --------- Method -------- //
			unsigned getWidth(Instruction *inst) {
				if ( inst2width.count(inst) )
					return inst2width[inst];
				else {
					inst->dump();
					assert(0);
				}
			}

			//This is used in printverilog
			unsigned getRequiredBitWidth(Constant *c) {
				if ( ConstantInt *cInt = dyn_cast<ConstantInt>(c) ) {
					uint64_t value = cInt->getSExtValue();
					if ( cInt->isNegative() )
						value = value * (-1);

					unsigned i = 0;
					while ( value != 0 ) {
						value = value >> 1;
						i++;
					}
					return i==0? 1 : i;
				}
				else if ( isa<PointerType>(c->getType()) ) {
					return pointerBitSize;
				}
				else {
					c->dump();
					assert(0);
				}
			}

			bool existWidth(Instruction *inst) { return inst2width.count(inst); }
			bool getSigned(Instruction *inst) { 
				if ( inst2signed.count(inst) )
					return inst2signed[inst];
				else
					return false;
			}

			// --------- build -------- //
			void addWidth(Instruction *inst, unsigned width) { inst2width[inst] = width; }
			void addSigned(Instruction *inst, bool sign) { inst2signed[inst] = sign; }

		private:
			DenseMap<Instruction *, unsigned> inst2width;
			DenseMap<Instruction *, bool> inst2signed;

			unsigned pointerBitSize;
	};

	class BitWidthTableBuilder
	{
		public:
			BitWidthTableBuilder(Module *module_, MemoryTable *memoryTable_,
				CallTable *callTable_, DenseMap<const Function *, LoopInfo *> loopInfoOf_, 
				PADriver *pa_, bool bitWidthOff_, Function *accelFunction_);
			~BitWidthTableBuilder() {}

			BitWidthTable *getBitWidthTable() { return bitWidthTable; }

			void setBitWidthFromFunction();
			void setFunctionArgBitWidth_Top(Function *);
			void setBitWidthFunction(Function *);

//			void searchAllInst();
			void setInstructionBitWidth(Function *, Instruction *);

			Instruction *getCMPInst(PHINode *phi);
			Instruction *checkInductionVariable(Instruction *, Instruction *, BasicBlock *, BasicBlock *);
			void handlePHINode(PHINode *);

			
			unsigned getWidthType(Type *);
			unsigned getWidthValue(Function *, Value *);

			void targetFuncSetting(Function *target) {
				targetFuncSet.insert(target);
				func2done[target] = false;
				for ( auto callee : callTable->getCalleeFromFunction(target) )
					targetFuncSetting(callee);
			}

			Function *getNextFunction() {
				unsigned i = 1;
				for ( auto func : targetFuncSet )
				{
					if ( i != seed ) {
						i++;
						continue;
					}
					if ( func2done[func] )
						continue;

					addSeed();
					return func;
				}
				for ( auto func : targetFuncSet )
				{
					if ( func2done[func] )
						continue;

					addSeed();
					return func;
				}
				return NULL;
			}

			void addSeed() {
				unsigned size = targetFuncSet.size();
				if ( size == seed )
					seed = 1;
				else
					seed++;
			}

			void setSignedInstruction();
			void searchSignedUser(Instruction *);

		private:
			BitWidthTable *bitWidthTable;

			Module *module;
			MemoryTable *memoryTable;
			CallTable *callTable;

			DenseMap<const Function *, LoopInfo *> loopInfoOf;
			PADriver *pa;

			PABasedAA *paaa;

			bool bitWidthOff;
			Function *accelFunction;

			set<Function *> targetFuncSet;
			DenseMap<Function *, bool> func2done;
			unsigned seed;
	};
}

#endif
