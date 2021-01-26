#ifndef CORELAB_OPERATOR_TABLE_H
#define CORELAB_OPERATOR_TABLE_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/ADT/DenseMap.h"

#include "VerilogConfig.h"

#include <list>
#include <set>

namespace corelab
{
	using namespace llvm;
	using namespace std;

//	struct OperatorType{
//		unsigned opCode; //LLVM OPCODE
//		unsigned bitWidth; // 8, 16, 32, 64
//	};

	typedef pair<unsigned, unsigned> OperatorType;

	class OperatorTable
	{
		public:
			// --------- Method -------- //
			bool isFlatOperator(void) { return flatOperator; }

			list<OperatorType> getShareableOperator(void) { return shareableOperator; }
			bool isShareableOperator(unsigned opCode, unsigned bitWidth) { 
				for ( auto operatorType : shareableOperator )
				{
					if ( operatorType.first == opCode && 
							operatorType.second == bitWidth )
						return true;
				}
				return false;
			}
			bool isShareableUser(Instruction *inst) {
				Type *instType = inst->getType();
				if ( IntegerType *intType = dyn_cast<IntegerType>(instType) ) 
					return isShareableOperator(inst->getOpcode(), intType->getBitWidth());
				else if ( instType->isFloatTy() )
					return isShareableOperator(inst->getOpcode(), 32);
				else if ( instType->isDoubleTy() )
					return isShareableOperator(inst->getOpcode(), 64);
				else
					return isShareableOperator(inst->getOpcode(), 64);
			}

			//There can be different bitwidth across the same operator type
			//But, when generating code, instantiate operator only once for each operator type
			// with maximum bitwidth.
			//And binding instructions to the one operator of one type.
			set<OperatorType> getUsedOperatorByFunction(Function *func) { return func2operator[func]; }
			set<OperatorType> getUsedOperator(void) { return usedOperator; }

			list<Instruction *> getOperatorUserInst(Function *func, OperatorType opType)
			{ return (func2userInst[func])[opType]; }

			set<Function *> getFunctionFromOperator(OperatorType opType) 
			{ return operator2func[opType]; }

			// --------- build -------- //
			void setFlatOperator(bool fo) { flatOperator = fo; }

			void addShareableOperator(OperatorType opType) { shareableOperator.push_back(opType); }

			void addUsedOperatorByFunction(Function *func, OperatorType opType)
			{ (func2operator[func]).insert(opType); }

			void addUsedOperator(OperatorType opType)
			{ usedOperator.insert(opType); }

			void addUserInst(Function *func, OperatorType opType, Instruction *inst)
			{ (func2userInst[func])[opType].push_back(inst); }

			void addOperator2Function(OperatorType opType, Function *func) 
			{ operator2func[opType].insert(func); }

		private:
			bool flatOperator;

			list<OperatorType> shareableOperator;
			
			//Function to Used Operator
			DenseMap<Function *, set<OperatorType>> func2operator;
			//For Flat Operator Topology
			set<OperatorType> usedOperator;

			DenseMap<OperatorType, set<Function *>> operator2func;

			//Binding
			DenseMap<Function *, DenseMap<OperatorType, list<Instruction *>>> func2userInst;
	};

	class OperatorTableBuilder
	{
		public:
			OperatorTableBuilder(Module *module_, VerilogConfigInfo *verilogConfigInfo_);
			~OperatorTableBuilder() {}

			OperatorTable *getOperatorTable() { return operatorTable; }

			void setShareableOperator(OperatorTable *);

			OperatorType genOperatorType(unsigned op, unsigned bitWidth) {
				OperatorType opType;
				opType.first = op;
				opType.second = bitWidth;

				return opType;
			}

		private:
			OperatorTable *operatorTable;

			Module *module;
			VerilogConfigInfo *verilogConfigInfo;
	};
}

#endif
