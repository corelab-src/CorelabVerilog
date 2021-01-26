#ifndef PRINT_VERILOG_H
#define PRINT_VERILOG_H

#include "llvm/Support/raw_ostream.h"

#include "VerilogConfig.h"
#include "FSM.h"
#include "LoopPatternAnal.h"
#include "MemoryTable.h"
#include "CallTable.h"
#include "OperatorTable.h"
#include "BitWidthTable.h"

#include <string>

namespace corelab
{
	using namespace llvm;
	using namespace std;

	class PrintVerilog
	{
		public:
			PrintVerilog(Module *module_, VerilogConfigInfo *verilogConfigInfo_, FSM *fsm_,
					LoopPatternAnal *lpa_, MemoryTable *memoryTable_,
					CallTable *callTable_, OperatorTable *operatorTable_, BitWidthTable *bitWidthTable_,
					PADriver *pa_) :
				module(module_), verilogConfigInfo(verilogConfigInfo_), fsm(fsm_),
				lpa(lpa_), memoryTable(memoryTable_), callTable(callTable_), 
				operatorTable(operatorTable_), bitWidthTable(bitWidthTable_), pa(pa_) {}
			~PrintVerilog() {}

			void printVerilog(void);

		private:
			void printRegDeclaration(Instruction *, raw_fd_ostream *);
			void printWireDeclaration(Instruction *, raw_fd_ostream *);
			void printNameOfInst(Instruction *, Function *, raw_fd_ostream *);
			void printNameFromInst(Instruction *, Function *, State *, unsigned, raw_fd_ostream *);
			void printNameFromValue(Value *, Function *, State *, unsigned, raw_fd_ostream *);
			void printHex(uint64_t, raw_fd_ostream *);

			void printBinaryOperator(Instruction *, raw_fd_ostream *);
			void printByteAddressShift(Type *, raw_fd_ostream *);
			unsigned getByteSize(Type *);

			void printPipelineRegister(PipelineState *, Function *, raw_fd_ostream *);
			void printStageTransition(PipelineState *, raw_fd_ostream *);
			void printStageParameter(PipelineState *, raw_fd_ostream *);

			void printFunctionDeclaration(Function *, raw_fd_ostream *);
			void printFunctionRegister(Function *, raw_fd_ostream *);
			void printFunctionStateParameter(Function *, raw_fd_ostream *);
			void printFunctionStateTransition(Function *, raw_fd_ostream *);
			void printFunctionNormalOperation(Function *, raw_fd_ostream *);
			void printFunctionShareableOperation(Function *, raw_fd_ostream *);
			void printFunctionCallInstruction(Function *, raw_fd_ostream *);
			void printFunctionMemoryController(Function *, raw_fd_ostream *);
			void printFunctionPrivateAccess(Function *, raw_fd_ostream *);
			void printFunctionAddressAccess(Function *, raw_fd_ostream *);
			void printFunctionUnresolvedAccess(Function *, raw_fd_ostream *);
			void printFunctionValidationRequest(Function *, raw_fd_ostream *);

			void functionModuleDefine(Function *);
			void functionModuleGeneration(void);

			//-----PrintVerilogMemory.cpp-----//
			void memoryModuleGeneration(void);
			void printRegisterRAMBody(raw_fd_ostream *, bool);
			void printLUTRAMBody(raw_fd_ostream *, bool);
			void printBlockRAMBody(raw_fd_ostream *, bool);

			void printRAMInstance(RAM_ *, raw_fd_ostream *);
			//--------------------------------//

			void operatorModuleGeneration(void);
			

			void printDefinedParameter(raw_fd_ostream *);
			void printInstantiation(raw_fd_ostream *);
			void printConnection(raw_fd_ostream *);
			void topModuleGeneration(void);

			void ramDualPortSetting(void);

			void targetSetting(void);
			void targetFuncSetting(Function *);

			bool isUsedInMemFunc(RAM_ *);
			bool isMemFunction(Function *func) {
				for ( auto iter : setFunctions )
					if ( iter == func )
						return true;
				for ( auto iter : cpyFunctions )
					if ( iter == func )
						return true;
				return false;
			}

			bool isSetFunction(Function *func) {
				for ( auto iter : setFunctions )
					if ( iter == func )
						return true;
			}
			bool isCpyFunction(Function *func) {
				for ( auto iter : cpyFunctions )
					if ( iter == func )
						return true;
			}

			unsigned getMemFunctionBitSize(Function *func) {
				assert( isSetFunction(func) || isCpyFunction(func) );

				for (auto bi = func->begin(); bi != func->end(); bi++)
					for (auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++)
					{
						Instruction *inst = &*ii;
						if ( isa<StoreInst>(inst) ) {
							IntegerType *stType = dyn_cast<IntegerType>(inst->getOperand(0)->getType());
							assert(stType);

							return stType->getBitWidth();
						}
					}
				assert(0&&"can not find store instruction in mem function");
			}

			bool isSignedBinary(Instruction *);
			bool isSignedValue(Value *);

			void functionModuleDefineForMem(Function *);
			void printFunctionRegisterForMem(Function *, raw_fd_ostream *);
			void printFunctionStateTransitionForMem(Function *, raw_fd_ostream *);
			void printFunctionOperationForMem(Function *, raw_fd_ostream *);

//			raw_ostream &out;

			Module *module;
			VerilogConfigInfo *verilogConfigInfo;
			FSM *fsm;
			LoopPatternAnal *lpa;
			MemoryTable *memoryTable;
			CallTable *callTable;
			OperatorTable *operatorTable;
			BitWidthTable *bitWidthTable;

			PADriver *pa;

			set<Function *> targetFuncSet;
			set<RAM_ *> targetRAMSet;
			set<OperatorType> targetOPSet;

			set<Function *> setFunctions;
			set<Function *> cpyFunctions;
	};
}

#endif
