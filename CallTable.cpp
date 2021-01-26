#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/InstrTypes.h"

#include "CallTable.h"

#include <iostream>
#include <fstream>


using namespace llvm;
using namespace std;

namespace corelab
{
	static unsigned callId;
	static void initCallId(void) { callId = 0; }
	static unsigned getCallId(void) { return ++callId; }

	static unsigned funcId;
	static void initFuncId(void) { funcId = 0; }
	static unsigned getFuncId(void) { return ++funcId; }

	static unsigned getRequiredBits ( unsigned n )
	{
		assert ( n != 0 && " num of Something is zero ( state or function ) " );
		unsigned bits = 0;
		n = n - 1;
		if ( n == 0 )
			return 1;

		for ( ; n != 0; bits++)
			n = n >> 1;
		return bits;
	}

	/*
	void CallTableBuilder::setBitWidth(unsigned pointerBitWidth) {
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
		{
			Function *func = &*fi;
			if ( func->isDeclaration() )
				continue;

			unsigned argNum = 0;
			for ( auto argIter = func->arg_begin(); argIter != func->arg_end(); ++argIter )
			{
				Value *arg = (Value *)(&*argIter);
				assert(arg);

				Type *argType = arg->getType();
				//redefine
				if ( isa<PointerType>(argType) ) {
					callTable->setFuncArgBit(func, argNum, pointerBitWidth); 
					callTable->setMaxArgBitWidth(argNum, pointerBitWidth);
				}
				else if ( argType->isVoidTy() )
					callTable->setFuncArgBit(func, argNum, 1); 

				argNum++;
			}

			Type *returnType = func->getReturnType();
			if ( isa<PointerType>(returnType) ) {
				callTable->setFuncReturnBit(func, pointerBitWidth);
				callTable->setMaxBitWidth(pointerBitWidth); 
			}
			else if ( returnType->isVoidTy() )
				callTable->setFuncReturnBit(func, 1);
		}
	}*/

	CallTableBuilder::CallTableBuilder(Module *module_, VerilogConfigInfo *verilogConfigInfo_)
		: module(module_), verilogConfigInfo(verilogConfigInfo_)
	{
		callTable = new CallTable();

		// Build Call Table
		callTable->setCallBus(verilogConfigInfo->getCallBus());

		initFuncId();
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
		{
			Function *func = &*fi;
			if ( func->isDeclaration() )
				continue;

			callTable->setFuncId(func, getFuncId());

			
			unsigned argNum = 0;
			for ( auto argIter = func->arg_begin(); argIter != func->arg_end(); ++argIter )
			{
				Value *arg = (Value *)(&*argIter);
				assert(arg);
/*
				Type *argType = arg->getType();
				if ( IntegerType *intType = dyn_cast<IntegerType>(argType) ) {
					callTable->setFuncArgBit(func, argNum, intType->getBitWidth());
					callTable->setMaxArgBitWidth(argNum, intType->getBitWidth());
				}
				else {
					callTable->setFuncArgBit(func, argNum, 64); 
					callTable->setMaxArgBitWidth(argNum, 64); //CompositeType, FunctionType, PointerType
				}
*/
				argNum++;
			}

			//set max arg num
			callTable->setMaxArgNum(argNum);

			//set max bit width
/*
			Type *returnType = func->getReturnType();
			if ( IntegerType *intType = dyn_cast<IntegerType>(returnType) ) {
				callTable->setFuncReturnBit(func, intType->getBitWidth());
				callTable->setMaxBitWidth(intType->getBitWidth());
			}
			else {
				callTable->setFuncReturnBit(func, 64);
				callTable->setMaxBitWidth(64); //CompositeType, FunctionType, PointerType
			}
*/

			//init call id for each call instruction
			initCallId();

			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;
					if ( CallInst *callInst = dyn_cast<CallInst>(inst) ) {
						Function *callee = callInst->getCalledFunction();
						assert(callee);

						callTable->addCalleeFunction(func, callee);
						callTable->addCallerFunction(func, callee);

						callTable->addCallInst(func, callee, inst);

						callTable->setCallInstId(func, inst, getCallId());
					}
					else if ( InvokeInst *callInst = dyn_cast<InvokeInst>(inst) ) {
						Function *callee = callInst->getCalledFunction();
						//TODO: if ( !callee ) then automatically change to call bus system
						//      and search target functions
						//			callTable->setCallBus(true);
						assert(callee);

						callTable->addCalleeFunction(func, callee);
						callTable->addCallerFunction(func, callee);

						callTable->addCallInst(func, callee, inst);

						callTable->setCallInstId(func, inst, getCallId());
					}
				}
		}
		callTable->setFuncSize(getFuncId()-1);
		callTable->setFuncBitSize(getRequiredBits(callTable->getFuncSize()));


		//Print Call Table Debug File
		std::error_code ec_print = std::make_error_code(std::errc::io_error);
		raw_fd_ostream callFile("callTable.debug", ec_print, llvm::sys::fs::F_None);

		callFile << "@@@@@@@@@@@@@@ Call Table @@@@@@@@@@@\n\n";

		if ( callTable->isCallBus() ) {
			callFile << "The Architecture Uses Call Bus System\n\n";

			callFile << "The Maximum Number of Arguments : ";
			callFile << callTable->getMaxArgNum() << "\n";
			callFile << "The Maximum BitWidth of Return Val : ";
			callFile << callTable->getMaxReturnBitWidth() << "\n";
			callFile << "The Maximum BitWidth of Each Argument : \n";
			for ( unsigned i = 0; i < callTable->getMaxArgNum(); ++i )
				callFile << "\t" << i << " : " << callTable->getArgBitWidth(i) << "\n";
			callFile << "\n";
		}
		else
			callFile << "The Architecture Uses Direct Call System\n\n";

		callFile << "\n@@@@@@@@@@@@@@ Caller to Callee Table @@@@@@@@@@@\n\n";
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
		{
			Function *func = &*fi;
			if ( func->isDeclaration() )
				continue;

			callFile << "Caller : " << func->getName() << "\n";
			for ( auto callee : callTable->getCalleeFromFunction(func) )
				callFile << "\t" << callee->getName() << "\n";
			callFile << "\n";
		}

		callFile << "\n\n@@@@@@@@@@@@@@ Callee to Caller Table @@@@@@@@@@@\n\n";
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
		{
			Function *func = &*fi;
			if ( func->isDeclaration() )
				continue;

			callFile << "Callee : " << func->getName() << "\n";
			for ( auto caller : callTable->getCallerFromFunction(func) )
				callFile << "\t" << caller->getName() << "\n";
			callFile << "\n";
		}
	}

}
