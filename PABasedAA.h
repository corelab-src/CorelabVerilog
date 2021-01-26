//Written by csKim
#ifndef LLVM_CORELAB_PABASEDAA_H
#define LLVM_CORELAB_PABASEDAA_H

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/ADT/DenseMap.h"

#include "PABuilder.h"

#include <set>
#include <list>
#include <vector>
#include <iostream>
#include <fstream>

namespace corelab
{
	using namespace llvm;
	using namespace std;

	class LoopNodeAA
	{
		public:
			LoopNodeAA(const Loop *L_, bool innerMost_) : L(L_), innerMost(innerMost_) {}
			
			//-----------Loop Usage------------//
			const Loop *getLoop() { return L; }

			bool isInnerMost() { return innerMost; }
			bool isSimpleForm() { return simpleForm; }
			bool hasSimpleCanonical() { return simpleCanonical; }
			bool hasSimpleExitCond() { return simpleExitCond; }

			PHINode *getInductionVariable() { return indvar; }
			Instruction *getStride() { return stride; }
			Value *getInitValue() { return initValue; }
			Instruction *getExitCondition() { return exitCondition; }
			BranchInst *getExitBranch() { return exitBranch; }

			//-----------Loop Build------------//
			void setLoopInfo(void);
			bool setCanonicalInductionVariableAux(const Loop *);
			bool setExitCondition(const Loop *);

		private:
			const Loop *L;
			bool innerMost;
			
			//Loop does not contain exit block
			bool simpleForm; //simple form && # of exit block == 1
			bool simpleCanonical;
			bool simpleExitCond;

			PHINode *indvar;
			Instruction *stride;
			Value *initValue;
			Instruction *exitCondition;
			BranchInst *exitBranch;
	};


	class PABasedAA 
	{
		public:
			PABasedAA(Module *module_, PADriver*pa_, 
				DenseMap<const Function *, LoopInfo *> loopInfoOf_) :
				module(module_), pa(pa_), loopInfoOf(loopInfoOf_) {}
			~PABasedAA();

			//-----------Usage------------//
			//memInst A , B : 
			//					ptr of A, access size, ptr of B, access size
			//size as byte
			bool isNoAlias(Value *, int, Value *, int);

			int getAccessSize(Instruction *inst) {
				Type *bitWidthType;
				if ( LoadInst *lInst = dyn_cast<LoadInst>(inst) )
					bitWidthType = lInst->getType();
				else if ( StoreInst *sInst = dyn_cast<StoreInst>(inst) )
					bitWidthType = sInst->getOperand(0)->getType();
				else
					assert(0);

				if ( IntegerType *intTy = dyn_cast<IntegerType>(bitWidthType) )
					return intTy->getBitWidth() /8;
				else if ( isa<PointerType>(bitWidthType) )
					return 4;
				else if ( bitWidthType->isFloatTy() )
					return 4;
				else if ( bitWidthType->isDoubleTy() )
					return 8;
				else
					return 8;
			}

			
			//-----------LoopAA------------//
			//need to call before use
			void initLoopAA(void);

			list<LoopNodeAA *> getLoopNodes() { return loopNodeList; }
			LoopNodeAA *getLoopNode(const Loop *L) {
				if ( loop2node.count(L) )
					return loop2node[L];
				else
					return NULL;
			}

			pair<bool, int> distanceCheck(LoopNodeAA *,
					list<pair<int, list<Value *>>>, int, list<pair<int, list<Value *>>>, int);
			bool hasTargetIndInFirst(Value *,list<pair<int, list<Value *>>>);



			//-----------Build------------//
			void collectPointers(list<Value *>&, Value *);
			Value *getSamePoint(list<Value *>, list<Value *>);
			list<Value *> trimList(list<Value *>, Value *);
			bool collectOperations(list<pair<int, list<Value *>>>&, Value *);
			bool collectOperationsAux(list<pair<int, list<Value *>>>&, int, list<Value *>&, Value *);
			bool getInductionInc(PHINode *, pair<Value *, Value *>&);
			bool offsetAliasCheckLoopInvariant(
					list<pair<int, list<Value *>>>, int, list<pair<int, list<Value *>>>, int);
			void collectOffsetList(list<pair<int, list<Value *>>>, list<int> &);

			void initLoop(const Loop *, bool);
			void initLoops(const Loop *);

		private:
			Module *module;
			PADriver*pa;
			DenseMap<const Function *, LoopInfo *> loopInfoOf;

			list<LoopNodeAA *> loopNodeList;
			DenseMap<const Loop *, LoopNodeAA *> loop2node;
	};
}

#endif
