#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/InstrTypes.h"

#include "OperatorTable.h"

#include <iostream>
#include <fstream>


using namespace llvm;
using namespace std;

namespace corelab
{

	void OperatorTableBuilder::setShareableOperator(OperatorTable *operatorTable) 
	{
		//TODO: Automatically Choose Shareable Operators

		operatorTable->addShareableOperator(genOperatorType(Instruction::FAdd, 32));
		operatorTable->addShareableOperator(genOperatorType(Instruction::FAdd, 64));

		operatorTable->addShareableOperator(genOperatorType(Instruction::FSub, 32));
		operatorTable->addShareableOperator(genOperatorType(Instruction::FSub, 64));

		operatorTable->addShareableOperator(genOperatorType(Instruction::FMul, 32));
		operatorTable->addShareableOperator(genOperatorType(Instruction::FMul, 64));

		operatorTable->addShareableOperator(genOperatorType(Instruction::FDiv, 32));
		operatorTable->addShareableOperator(genOperatorType(Instruction::FDiv, 64));

		operatorTable->addShareableOperator(genOperatorType(Instruction::FRem, 32));
		operatorTable->addShareableOperator(genOperatorType(Instruction::FRem, 64));
	}

	OperatorTableBuilder::OperatorTableBuilder(Module *module_, 
			VerilogConfigInfo *verilogConfigInfo_)
		: module(module_), verilogConfigInfo(verilogConfigInfo_)
	{
		operatorTable = new OperatorTable();

		operatorTable->setFlatOperator(verilogConfigInfo->getFlatOperator());

		setShareableOperator(operatorTable);
		
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
		{
			Function *func = &*fi;
			if ( func->isDeclaration() )
				continue;

			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;

					if ( BinaryOperator *binaryOp = dyn_cast<BinaryOperator>(inst) ) {
						unsigned opCode = inst->getOpcode();
						unsigned bitWidth;
						
						Type *instType = inst->getType();
						if ( IntegerType *intType = dyn_cast<IntegerType>(instType) )
							bitWidth = intType->getBitWidth();
						else if ( instType->isFloatTy() )
							bitWidth = 32;
						else if ( instType->isDoubleTy() )
							bitWidth = 64;
						else
							bitWidth = 64;

						if ( operatorTable->isShareableOperator(opCode, bitWidth) ) {
							OperatorType opType = genOperatorType(opCode, bitWidth);
							operatorTable->addUsedOperatorByFunction(func, opType);
							operatorTable->addUsedOperator(opType);

							operatorTable->addUserInst(func, opType, inst);

							operatorTable->addOperator2Function(opType, func);
						}
					}
				}

		}

		//Print Operator Table Debug File
		std::error_code ec_print = std::make_error_code(std::errc::io_error);
		raw_fd_ostream callFile("operatorTable.debug", ec_print, llvm::sys::fs::F_None);

		callFile << "@@@@@@@@@@@@@@ Operator Table @@@@@@@@@@@\n\n";
		callFile << "FAdd : " << Instruction::FAdd << "\n";
		callFile << "FSub: " << Instruction::FSub << "\n";
		callFile << "FMul : " << Instruction::FMul << "\n";
		callFile << "FDiv : " << Instruction::FDiv << "\n";
		callFile << "FRem : " << Instruction::FRem << "\n\n";

		callFile << "\n@@@@@@@@@@@@@@ Shareable Operator List @@@@@@@@@@@\n\n";
		for ( auto opType : operatorTable->getShareableOperator() )
			callFile << "OPCODE : " << opType.first << "\tBITWIDTH : " << opType.second << "\n";

		
		callFile << "\n\n@@@@@@@@@@@@@@ Used Operator @@@@@@@@@@@\n\n";
		for ( auto opType : operatorTable->getUsedOperator() )
			callFile << "OPCODE : " << opType.first << "\tBITWIDTH : " << opType.second<< "\n";


		callFile << "\n\n@@@@@@@@@@@@@@ Used Operator by Function @@@@@@@@@@@\n\n";
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
		{
			Function *func = &*fi;
			if ( func->isDeclaration() )
				continue;

			callFile << "Function : " << func->getName() << "\n";
			for ( auto opType : operatorTable->getUsedOperatorByFunction(func) )
				callFile << "\tOPCODE : " << opType.first << "\tBITWIDTH : " << opType.second << "\n";
			callFile << "\n";
		}

		callFile << "\n\n@@@@@@@@@@@@@@ Operator Users @@@@@@@@@@@\n\n";
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
		{
			Function *func = &*fi;
			if ( func->isDeclaration() )
				continue;

			callFile << "Function : " << func->getName() << "\n";
			for ( auto opType : operatorTable->getUsedOperatorByFunction(func) )
			{
				callFile << "\tOPCODE : " << opType.first << "\tBITWIDTH : " << opType.second << "\n";
				for ( auto instIter : operatorTable->getOperatorUserInst(func, opType) )
					callFile << "\t\t" << *instIter << "\n";

				callFile << "\n";
			}
			callFile << "\n";
		}

	}

}
