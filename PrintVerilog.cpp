#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/InstrTypes.h"

#include "PrintVerilog.h"

#include <iostream>
#include <fstream>

using namespace llvm;
using namespace std;

namespace corelab {

	static unsigned getRequiredBits ( unsigned n ) {
		assert ( n != 0 && " num of Something is zero ( state or function ) " );
		unsigned bits = 0;
		n = n - 1;
		if ( n == 0 )
			return 1;

		for ( ; n != 0; bits++)
			n = n >> 1;
		return bits;
	}

	static unsigned getNumOfElementsFromType(Type *ty)
	{
//		if ( ArrayType *aTy = dyn_cast<ArrayType>(ty) )
		if ( SequentialType *aTy = dyn_cast<SequentialType>(ty) )
		{
			unsigned numOfElements = aTy->getNumElements();
			Type *nestType = aTy->getElementType();

			unsigned nestNumOfElements = getNumOfElementsFromType(nestType);

			return numOfElements * nestNumOfElements;
		}
		else if ( StructType *sTy = dyn_cast<StructType>(ty) )
		{
			//XXX: This Struct Case is just for counting the all number of struct elements
			unsigned totalNestNumOfElements = 0;
			for ( int i = 0; i < sTy->getNumElements(); i++ )
				totalNestNumOfElements += getNumOfElementsFromType( sTy->getElementType(i) );
			return totalNestNumOfElements;
		}
		else if (  IntegerType *iTy = dyn_cast<IntegerType>(ty) )
			return 1;
		else if ( ty->isFloatTy() )
			return 1;
		else if ( ty->isDoubleTy() )
			return 1;
		else if ( isa<PointerType>(ty) )
			return 1;
		else
		{
			ty->dump();
			assert(0 &&"can not support this type of memory");
			return 0;
		}
	}

	bool PrintVerilog::isSignedValue(Value *v) {
		if ( Instruction *inst = dyn_cast<Instruction>(v) )
			return bitWidthTable->getSigned(inst);
		else
			return false;
	}

	void PrintVerilog::printRegDeclaration(Instruction *inst, raw_fd_ostream *outFile) {
		unsigned bitWidth = 0;

		/*
		Type *instTy = inst->getType();
		if ( instTy->isVoidTy() )
			bitWidth = 0;
		else if ( IntegerType *intType = dyn_cast<IntegerType>(instTy) )
			bitWidth = intType->getBitWidth();
		else
			bitWidth = 64;*/

		bitWidth = bitWidthTable->getWidth(inst);

		if ( bitWidth == 0 ) {
		}
		else if ( bitWidth == 1 )
			*outFile << "reg ";
		else
			*outFile << "reg [" << bitWidth -1 << ":0] ";
	}

	void PrintVerilog::printWireDeclaration(Instruction *inst, raw_fd_ostream *outFile) {
		unsigned bitWidth = 0;

		/*
		Type *instTy = inst->getType();
		if ( instTy->isVoidTy() )
			bitWidth = 0;
		else if ( IntegerType *intType = dyn_cast<IntegerType>(instTy) )
			bitWidth = intType->getBitWidth();
		else
			bitWidth = 64;*/

		bitWidth = bitWidthTable->getWidth(inst);

		if ( bitWidth == 0 ) {
		}
		else if ( bitWidth == 1 )
			*outFile << "wire ";
		else
			*outFile << "wire [" << bitWidth -1 << ":0] ";
	}

	void PrintVerilog::printNameOfInst(Instruction *inst, Function *func, 
																		raw_fd_ostream *outFile) {
		StringRef instName = inst->getName();

		//print Parent Function Name
		if ( isa<CallInst>(inst) || isa<InvokeInst>(inst) )
			*outFile << func->getName() << "_call_" << callTable->getCallId(func, inst) << "_";
		else
			*outFile << func->getName() << "_";

		StringRef noName("");
		//print instruction name
		if ( instName == noName )
			*outFile << fsm->getStateFromInst(inst)->getNoNameInstNum(inst);
		else {
			//if there is '.' in the name, replace it for '_'
			string replacedName = instName.str();
			unsigned i = 0;
			for ( auto ch : replacedName )
			{
				if ( (ch) == '.' || (ch) == '-' )
					replacedName.replace(i,1,"_");
				i++;
			}
		
			*outFile << replacedName;
		}
	}

	void PrintVerilog::printNameFromInst(Instruction *inst, Function *func, State *curState,
																			unsigned useTiming, raw_fd_ostream *outFile) {
		printNameOfInst(inst, func, outFile);

		//check timing for printing _reg mark
		State *instState = fsm->getStateFromInst(inst);
		unsigned instGenTiming = instState->getSchFromInst(inst) + 
															verilogConfigInfo->getOperationInfo(inst).latency[0];

		if ( instState->isPipelineState() )
			*outFile << "_reg_" << instGenTiming;
		else {
			if ( instState == curState &&
					useTiming == instGenTiming &&
					!isa<PHINode>(inst) ) {
			}
			else
				*outFile << "_reg";
		}
	}

	void PrintVerilog::printHex(uint64_t value, raw_fd_ostream *outFile) {
		if (value == 0)
			return;

		uint64_t rem = value % 16;
		value /= 16;
		printHex(value, outFile); //first execute recurency and print next value

		//after return print the less significant digit
		if (rem > 9)
			*outFile << (char)(rem - 10 + 'A');
		else
			*outFile << rem;
	}

	void PrintVerilog::printNameFromValue(Value *v, Function *func, State *curState,
			unsigned useTiming, raw_fd_ostream *outFile) {

		if ( ConstantInt *cInt = dyn_cast<ConstantInt>(v) ) {
			unsigned bitWidth = cInt->getBitWidth();
			uint64_t value = cInt->getSExtValue();
//			char buffer[64];
//			itoa(cInt->getSExtValue(), buffer, 16);
			if ( value == 0 )
				*outFile << "0";
			else {
				*outFile << "(" << bitWidth << "'h";
				printHex(cInt->getSExtValue(), outFile);		
				*outFile << ")";
			}
		}
		else if ( Function *callee = dyn_cast<Function>(v) ) {
			*outFile << callTable->getFuncId(callee);
		}
		else if ( v->getType()->isVoidTy() ) {
			*outFile << "0";
		}
		else if ( isa<ConstantPointerNull>(v) ) {
			*outFile << "0";
		}
		else if ( RAM_ *ram = memoryTable->getRAMFromValue(v) ) {
			if ( (memoryTable->getAddressAccessRAMList()).size() == 0 )
				*outFile << "0";
			else
				*outFile << "`RAM" << ram->getRAMId() << "_Address";
		}
		else if ( Instruction *inst = dyn_cast<Instruction>(v) ) {
			printNameFromInst(inst, func, curState, useTiming, outFile);
		}
		else if ( User *user = dyn_cast<User>(v) ) {
			if ( isa<BitCastOperator>(user) ) { // dont have to handle bitcast 
				Value *original = user->getOperand(0);
				printNameFromValue(original, func, curState, useTiming, outFile);
			}
			else if ( isa<GEPOperator>(user) ) {
				Value *pointer = user->getOperand(0);
				assert(pointer);
				Type *eTy = dyn_cast<PointerType>(pointer->getType())->getElementType();
				assert(eTy);

				*outFile << "(";
				printNameFromValue(pointer, func, curState, useTiming, outFile);

				unsigned numOps = user->getNumOperands();
				Value *operand;

				/*
				unsigned singleElementStruct = 0;
				bool singleElementIsSequential = false;
				if ( StructType *firstStructType = dyn_cast<StructType>(eTy) ) {
					singleElementStruct = firstStructType->getNumElements();
					if ( singleElementStruct == 1 ) {
						Type *elementType = firstStructType->getElementType(0);
						if ( isa<SequentialType>(elementType) ) {
							singleElementIsSequential = true;
							eTy = elementType;
						}
					}
				}*/

				if ( numOps > 2 ) {

//					if ( (dyn_cast<GEPOperator>(user)->isInBounds() && isa<SequentialType>(eTy)) ||
//							(singleElementStruct == 1 && singleElementIsSequential) ) {
					if ( dyn_cast<GEPOperator>(user)->isInBounds() && isa<CompositeType>(eTy) ) {
						Value *operand = user->getOperand(1);
						unsigned offset = getNumOfElementsFromType(eTy);
						*outFile << " + ( (" << offset << " * ";
						printNameFromValue(operand, func, curState, useTiming, outFile);
						*outFile << ") ";
						printByteAddressShift(eTy, outFile);
						*outFile << " )";
					}
					else {
						Value *zeroV = user->getOperand(1);
						assert(isa<ConstantInt>(zeroV));
						assert(dyn_cast<ConstantInt>(zeroV)->getSExtValue() == 0);
					}

					for (unsigned i = 2; i < numOps; ++i )
					{
						operand = user->getOperand(i);

						if ( StructType *sTy = dyn_cast<StructType>(eTy) ) {
							ConstantInt *offsetInt= dyn_cast<ConstantInt>(operand);
							assert(offsetInt && "struct type should have constant offset value");
							unsigned operandConst = offsetInt->getSExtValue();

							bool compactStruct = false;
							std::set<Value *> mset = pa->getPointedMemory(pointer);
							for ( auto iter : mset )
								if ( memoryTable->hasStruct(iter) )
									if ( memoryTable->isCompactStruct(iter) ) {
										Type *compactTy = iter->getType();
										PointerType *compactPTy = dyn_cast<PointerType>(compactTy);
										assert(compactPTy);
										Type *compactElementTy = compactPTy->getElementType();
										if ( compactElementTy == eTy )
											compactStruct = true;
									}

							eTy = sTy->getElementType(operandConst);
							unsigned offset = 0;

							for ( int k = 0; k < operandConst; ++k )
							{
								*outFile << " + ( " << getNumOfElementsFromType(sTy->getElementType(k));
								if ( compactStruct )
									printByteAddressShift(sTy->getElementType(k), outFile);
								else
									printByteAddressShift(sTy, outFile);
								*outFile << " ) ";
							}
						}
						else if ( SequentialType *aTy = dyn_cast<SequentialType>(eTy) ) {
							eTy = aTy->getElementType();
							unsigned offset = getNumOfElementsFromType(eTy);

							*outFile << " + ( (" << offset << " * ";
							printNameFromValue(operand, func, curState, useTiming, outFile);
							*outFile << ") ";
							printByteAddressShift(eTy, outFile);
							*outFile << " )";
						}
						else {
							assert(0 && "No Composite Type used");
						}
					}

				}
				else if ( numOps == 2 ) { //there is no struct
					assert(!isa<StructType>(eTy));
					operand = user->getOperand(1);
					unsigned offset = getNumOfElementsFromType(eTy);
					*outFile << " + ( (" << offset << " * ";
					printNameFromValue(operand, func, curState, useTiming, outFile);
					*outFile << ") ";
					printByteAddressShift(eTy, outFile);
					*outFile << " )";
				}

				*outFile << ")";
			}
		}
		else if ( isa<UndefValue>(v) )
		{
			*outFile << "0";
		}
		else { // for function argument // VALUE
			bool find = false;
			unsigned i = 0;
			State *sState = fsm->getStartStateFromFnc(func);
			for (  auto argIter = func->arg_begin(); argIter != func->arg_end(); ++argIter )
			{
				Value *arg = (Value *)(&*argIter);
				assert(arg);

				if ( arg == v ) {
					find = true;
					if ( (curState == sState) && (useTiming == 0) )
						*outFile << "arg_" << i;
					else
						*outFile << "arg_" << i << "_reg";
				}
				i++;
			}

			if ( !find ) {
				v->dump();
				assert(0&&"Can not Handle Other Value Types");
			}
		}

	}

	bool PrintVerilog::isSignedBinary(Instruction *inst) {
		if ( ICmpInst *cmpInst = dyn_cast<ICmpInst>(inst) ) {
			auto predicate = cmpInst->getPredicate();

			if ( predicate == CmpInst::ICMP_SGT ||
					predicate == CmpInst::ICMP_SGE ||
					predicate == CmpInst::ICMP_SLT ||
					predicate == CmpInst::ICMP_SLE )
				return true;
		}
		else if ( bitWidthTable->getSigned(inst) ) {
			if ( inst->getOpcode() == Instruction::Add ||
					inst->getOpcode() == Instruction::Sub ||
					inst->getOpcode() == Instruction::Mul ||
					inst->getOpcode() == Instruction::SDiv ||
					inst->getOpcode() == Instruction::SRem 
					)
				return true;
		}

		return false;
	}

	//Input for Only Binary Instruction
	void PrintVerilog::printBinaryOperator(Instruction *inst, raw_fd_ostream *out) {
		if ( inst->getOpcode() == Instruction::Add )
			*out << " + ";
		else if ( inst->getOpcode() == Instruction::Sub )
			*out << " - ";
		else if ( inst->getOpcode() == Instruction::Or )
			*out << " | ";
		else if ( inst->getOpcode() == Instruction::Xor )
			*out << " ^ ";
		else if ( inst->getOpcode() == Instruction::Shl )
			*out << " << ";
		else if ( inst->getOpcode() == Instruction::LShr )
			*out << " >> ";
		else if ( inst->getOpcode() == Instruction::AShr )
			*out << " >>> ";
		else if ( inst->getOpcode() == Instruction::And )
			*out << " & ";
		else if ( inst->getOpcode() == Instruction::Mul )
			*out << " * ";
		else if ( inst->getOpcode() == Instruction::SDiv )
			*out << " / ";
		else if ( inst->getOpcode() == Instruction::UDiv )
			*out << " / ";
		else if ( inst->getOpcode() == Instruction::SRem )
			*out << " % ";
		else if ( inst->getOpcode() == Instruction::URem )
			*out << " % ";
		else if ( ICmpInst *cmpInst = dyn_cast<ICmpInst>(inst) ) {
			auto predicate = cmpInst->getPredicate();

			if ( predicate == CmpInst::ICMP_EQ )
				*out << " == ";
			else if ( predicate == CmpInst::ICMP_NE )
				*out << " != ";
			else if ( predicate == CmpInst::ICMP_UGT ||
					predicate == CmpInst::ICMP_SGT )
				*out << " > ";
			else if ( predicate == CmpInst::ICMP_UGE ||
					predicate == CmpInst::ICMP_SGE )
				*out << " >= ";
			else if ( predicate == CmpInst::ICMP_ULT ||
					predicate == CmpInst::ICMP_SLT )
				*out << " < ";
			else if ( predicate == CmpInst::ICMP_ULE ||
					predicate == CmpInst::ICMP_SLE )
				*out << " <= ";
			else
				assert(0 &&"can not handle this cmp case\n");
		}
		else {
		inst->dump();
			assert(0 && "Can not handle this operator yet\n");
		}
	}

	unsigned PrintVerilog::getByteSize(Type *elementTy) {
		unsigned byteSize = 0;
//		if ( ArrayType *aTy = dyn_cast<ArrayType>(elementTy) )
		if ( SequentialType *aTy = dyn_cast<SequentialType>(elementTy) )
			return getByteSize(aTy->getElementType());
		else if ( IntegerType *iTy = dyn_cast<IntegerType>(elementTy) )
			byteSize = (iTy->getBitWidth())/8;
		else if ( elementTy->isFloatTy() )
			byteSize = 4;
		else if ( elementTy->isDoubleTy() )
			byteSize = 8;
		else if ( isa<PointerType>(elementTy) )
			byteSize = (memoryTable->getPointerBitSize())/8;
		else if ( StructType *sTy = dyn_cast<StructType>(elementTy) ) {
			for ( int i = 0; i < sTy->getNumElements(); i++ )
			{
				unsigned elementBSize = getByteSize(sTy->getElementType(i));
				if ( byteSize < elementBSize )
					byteSize = elementBSize; 
			}
		}
		else {
			elementTy->dump();
			assert(0 && "Can not Handle This Element Type for Printing Byte Address Shift");
		}	
		return byteSize;
	}

	void PrintVerilog::printByteAddressShift(Type *elementTy, raw_fd_ostream *out) {
		unsigned byteSize = 0;
//		if ( ArrayType *aTy = dyn_cast<ArrayType>(elementTy) ) {
		if ( SequentialType *aTy = dyn_cast<SequentialType>(elementTy) ) {
			printByteAddressShift(aTy->getElementType(), out);
			return;
		}
		else if ( IntegerType *iTy = dyn_cast<IntegerType>(elementTy) )
			byteSize = iTy->getBitWidth();
		else if ( elementTy->isFloatTy() )
			byteSize = 32;
		else if ( elementTy->isDoubleTy() )
			byteSize = 64;
		else if ( isa<PointerType>(elementTy) )
			byteSize = memoryTable->getPointerBitSize();
		else if ( isa<StructType>(elementTy) ) {
			byteSize = getByteSize(elementTy) * 8;
		}
		else {
			elementTy->dump();
			assert(0 && "Can not Handle This Element Type for Printing Byte Address Shift");
		}

		unsigned shiftSize;
		if ( byteSize == 8 )
			shiftSize = 0;
		else if ( byteSize == 16 )
			shiftSize = 1;
		else if ( byteSize == 32 )
			shiftSize = 2;
		else if ( byteSize == 64 )
			shiftSize = 3;
		else {
			elementTy->dump();
			errs() << byteSize << "\n";
			assert(0 && "Can not Handle This Byte Type for Printing Byte Address Shift");
		}

		assert( (shiftSize < 4) && (0 <= shiftSize) );

		if ( (shiftSize != 0) && memoryTable->isByteAddressedRAM() )
			*out << " << " << shiftSize;
	}

	void PrintVerilog::printStageTransition(PipelineState *state, raw_fd_ostream *out) {
		*out << "always @(posedge clk) begin\n";

		if ( memoryTable->isByteAddressedRAM() )
			*out << "\tif ( !memory_stall ) begin\n";

		*out << "\tif( is_State_" << state->getStateNumber() << "_0 )\n";
		*out << "\t\tstage_ing_" << state->getStateNumber() << " <= ";
		*out << state->getMaxEnd() + 1 << "'d1;\n";
		*out << "\telse if ( is_State_" << state->getStateNumber() << "_1 ) begin\n";

		*out << "\t\tif ( stage_ending_" << state->getStateNumber() << " ) begin\n";
		if ( state->getExitPoint() != 0 ) {
			*out << "\t\t\tif ( stage_ing_" << state->getStateNumber();
			*out << "[" << state->getExitPoint() << "]";
			*out << " && stage_ing_" << state->getStateNumber();
			*out << "[" << state->getExitPoint() -1 << ":0] == 0";
			*out << " ) begin\n";
			
			*out << "\t\t\t\tstage_ing_" << state->getStateNumber() << " <= 0;\n";
			*out << "\t\t\t\tpipeline_end_" << state->getStateNumber() << " <= 1;\n";
			*out << "\t\t\tend\n";

			*out << "\t\t\telse\n";
			*out << "\t\t\t\tstage_ing_" << state->getStateNumber() << " <= ";
			*out << "stage_ing_" << state->getStateNumber() << " << 1;\n";
		}
		*out << "\t\tend\n";


		*out << "\t\telse if ( stage_end_start_" << state->getStateNumber() << " ) begin\n";
		if ( state->getExitPoint() == 0 ) {
			*out << "\t\t\tstage_ending_" << state->getStateNumber() << " <= 1;\n";
			*out << "\t\t\tstage_ing_" << state->getStateNumber() << " <= 0;\n";
			*out << "\t\t\tpipeline_end_" << state->getStateNumber() << " <= 1;\n";
		}
		else {
			*out << "\t\t\tstage_ending_" << state->getStateNumber() << " <= 1;\n";
			*out << "\t\t\tstage_ing_" << state->getStateNumber() << " <= ";
			*out << "stage_ing_" << state->getStateNumber() << " << 1;\n";
		}
		*out << "\t\tend\n";
		*out << "\t\telse\n";
		*out << "\t\t\tstage_ing_" << state->getStateNumber() << " <= ";
		*out << "(stage_ing_" << state->getStateNumber() << " << 1) | 1;\n";

		*out << "\tend\n";

		*out << "\telse begin\n";//done pipeline state
		*out << "\t\tstage_ending_" << state->getStateNumber() << " <= 0;\n";
		*out << "\t\tstage_ing_" << state->getStateNumber() << " <= 0;\n";
		*out << "\t\tpipeline_end_" << state->getStateNumber() << " <= 0;\n";

		*out << "\tend\n";

		if ( memoryTable->isByteAddressedRAM() )
			*out << "\tend\n";

		*out << "end\n\n";

		//stage_start
		*out << "always @(posedge clk) begin\n";

		if ( memoryTable->isByteAddressedRAM() )
			*out << "\tif ( !memory_stall ) begin\n";

		*out << "\tif ( is_State_" << state->getStateNumber() << "_0 )\n";
		*out << "\t\tstage_start_" << state->getStateNumber() << " <= ";
		*out << state->getMaxEnd() + 1 << "'d1;\n";
		*out << "\telse if ( is_State_" << state->getStateNumber() << "_1 )\n";
		*out << "\t\tstage_start_" << state->getStateNumber() << " <= ";
		*out << "stage_start_" << state->getStateNumber() << " << 1;\n";
		*out << "\telse\n";
		*out << "\t\tstage_start_" << state->getStateNumber() << " <= 0;\n";

		if ( memoryTable->isByteAddressedRAM() )
			*out << "\tend\n";

		*out << "end\n\n";

		//II
		*out << "always @(posedge clk) begin\n";

		if ( memoryTable->isByteAddressedRAM() )
			*out << "\tif ( !memory_stall ) begin\n";

		*out << "\tif ( is_State_" << state->getStateNumber() << "_1 ) begin\n";

		*out << "\t\tif ( pipeline_ii_" << state->getStateNumber() << " == ";
		*out << getRequiredBits(state->getII()) << "'d" << state->getII() -1 << " )\n";
		*out << "\t\t\tpipeline_ii_" << state->getStateNumber() << " <= 0;\n";
		*out << "\t\telse\n";
		*out << "\t\t\tpipeline_ii_" << state->getStateNumber() << " <= ";
		*out << "pipeline_ii_" << state->getStateNumber() << " + 1;\n";

		*out << "\tend\n";

		*out << "\telse\n";
		*out << "\t\tpipeline_ii_" << state->getStateNumber() << " <= 0;\n";

		if ( memoryTable->isByteAddressedRAM() )
			*out << "\tend\n";

		*out << "end\n\n";
	}

	void PrintVerilog::printStageParameter(PipelineState *state, raw_fd_ostream *funcFile) {
		*funcFile << "reg [" << state->getMaxEnd() << ":0] stage_ing_";
		*funcFile<< state->getStateNumber() << ";\n";
		*funcFile<< "reg [" << state->getMaxEnd() << ":0] stage_start_";
		*funcFile<< state->getStateNumber() << ";\n";

		*funcFile<< "reg stage_end_start_" << state->getStateNumber() << ";\n"; //stage_end_start
		*funcFile<< "reg stage_ending_" << state->getStateNumber() << ";\n"; //stage_ending
		*funcFile<< "reg pipeline_end_" << state->getStateNumber() << ";\n";
		//pipeline_end // for stateTransition

		*funcFile<< "reg [" << getRequiredBits(state->getII()) -1 << ":0] pipeline_ii_";
		*funcFile<< state->getStateNumber() << ";\n\n";

		printStageTransition(state, funcFile);
	}

	void PrintVerilog::printFunctionStateTransition(Function *fnc, raw_fd_ostream *out)
	{
		bool hasMC = false;
		for ( auto ram : memoryTable->getAccessRAMFromFunction(fnc) )
			hasMC = hasMC || (ram->hasMultipleSize() || ram->hasExPort());

		list<State *> fncStateList = fsm->getStateListOfFunction(fnc);

		unsigned fncBits = callTable->getFuncBitSize();
		unsigned fncId = callTable->getFuncId(fnc);
		State *sState = fsm->getStartStateFromFnc(fnc);

		//reset & state update
		*out << "always @(posedge clk) begin\n";
		*out << "if (reset == 1'b1)\n";
		*out << "\tcur_state <= State_" << sState->getStateNumber() << "_0;\n";
		*out << "else\n\tcur_state <= next_state;\n";
		*out << "end\n\n";

		//state transition
		*out << "always @(*) begin\n";
		*out << "next_state = cur_state;\n";
		
//		if ( memoryTable->isByteAddressedRAM() ) {
		if ( hasMC ) {
			*out << "if ( memory_stall == 1 )\n";
			*out << "\tnext_state = cur_state;\n";
			*out << "else begin\n";
		}

		*out << "case(cur_state)\n";

		/*
		*out << "State_Wait:\n";
		if ( callTable->isCallBus() )
			*out << "\tif(start == " << fncBits << "'d" << fncId << ")\n";
		else
			*out << "\tif(start == 1'd1)\n";
		*out << "\t\tnext_state = State_" << sState->getStateNumber() << "_0;\n";
		*/

		for ( auto stateIter : fncStateList )
		{
			Instruction *exitInst = stateIter->getExitInst();
			PrevNextState nextStates = stateIter->getNext();

			for ( unsigned i = 0; i < stateIter->getLatency(); i++ )
			{
				*out << "State_" << stateIter->getStateNumber() << "_" << i << ":\n";

				//starting point
				if ( (sState == stateIter) && (i == 0) ) {
					if ( callTable->isCallBus() )
						*out << "if (start == " << fncBits << "'d" << fncId << ") begin\n";//TODO:start_reg
					else
						*out << "if (start == 1'd1 || start_reg == 1'd1) begin\n";
				}

				bool hasParallelCall = false;
				set<CallInst *> callSet;
				callSet.clear();
				for ( auto callIter : stateIter->getParallelCallSite() )
				{
					unsigned callTime = stateIter->getSchFromInst(callIter);
					if ( callTime == i ) {
						hasParallelCall = true;
						callSet.insert(callIter);
					}
				}

				//setting 
				stateIter->setParallelCallSiteTime(i, callSet);

				if ( hasParallelCall ) {
					*out << "\tif(1 ";
					for ( auto callInst : callSet )
					{
						Function *callee = callInst->getCalledFunction();
						assert(callee);

						*out << "&& (" << callee->getName() << "_finish ||";
						*out << callee->getName() << "_finish_reg)\n";
					}
					*out << ") begin\n";
				}

				if ( i != stateIter->getLatency() - 1)
				{
					*out << "\tnext_state = State_" << stateIter->getStateNumber();
					*out << "_" << i+1 << ";\n";
				}
				else
				{
					if ( stateIter->isPipelineState() ) {
						BranchInst *bInst = dyn_cast<BranchInst>(exitInst);
						assert(bInst);
						*out << "\tif(pipeline_end_" << stateIter->getStateNumber();
						*out << " == 1'd1)\n";
						*out << "\t\tnext_state = State_";
						*out << (nextStates.front())->getStateNumber() << "_0;\n";//only exit bb is stored
						*out << "\telse\n";
						*out << "\t\tnext_state = State_";
						*out << stateIter->getStateNumber() << "_1;\n";//Back to this state
						continue;
					}

					if ( BranchInst *bInst = dyn_cast<BranchInst>(exitInst) )
					{
						if ( bInst->isConditional() )
						{
							Value *cmpV = bInst->getCondition();
//							*out << "\tif(" << getNameFromValue(cmpV, fnc, stateIter, i);
							*out << "\tif(";
							printNameFromValue(cmpV, fnc, stateIter, i, out);
							*out << " == 1'd1)\n";
							*out << "\t\tnext_state = State_";
							*out << (nextStates.front())->getStateNumber() << "_0;\n";
							*out << "\telse\n";
							*out << "\t\tnext_state = State_";
							*out << (nextStates.back())->getStateNumber() << "_0;\n";
						}
						else
						{
							assert(nextStates.size() == 1);
							for ( auto nStateIter : nextStates )
							{
								*out << "\tnext_state = State_" <<  nStateIter->getStateNumber();
								*out << "_0;\n";
							}
						}
					}
					else if ( isa<ReturnInst>(exitInst) )
					{
						*out << "\tnext_state = State_" << sState->getStateNumber() << "_0;\n";
					}
					else if ( isa<SwitchInst>(exitInst) )
					{
						SwitchInst* swInst = dyn_cast<SwitchInst>(exitInst);
						assert(swInst);

						Value* conditionV = swInst->getCondition();
						assert(conditionV);

						BasicBlock* defaultBB = swInst->getDefaultDest(); 
						assert(defaultBB);
						Instruction* gpsInst = &*(defaultBB->begin());
						State *defaultState = fsm->getStateFromInst(gpsInst);
						assert(defaultState);

						*out << "\tcase(";
						printNameFromValue(conditionV, fnc, stateIter, i, out);
						*out << ")\n";
						for ( auto caseIter = swInst->case_begin(); caseIter != swInst->case_end(); caseIter++ )
						{
							Value* caseValue = caseIter->getCaseValue();
							assert(caseValue);
							BasicBlock* caseBB = caseIter->getCaseSuccessor();
							assert(caseBB);
							Instruction* caseFirstInst = &*(caseBB->begin());
							assert(caseFirstInst);
							State* caseState = fsm->getStateFromInst(caseFirstInst);
							assert(caseState);

							*out << "\t";
							printNameFromValue(caseValue, fnc, stateIter, i, out);
							*out << ":\tnext_state = State_";
							*out << caseState->getStateNumber() << "_0;\n";
						}

						*out << "\tdefault:\tnext_state = State_";
						*out << defaultState->getStateNumber() << "_0;\n";
						*out << "\tendcase\n";
					}
					else if ( InvokeInst *iInst = dyn_cast<InvokeInst>(exitInst) )
					{
//						Value *fcnPtr = iInst->getCalledValue();
						Function *nextF = iInst->getCalledFunction();
						assert(nextF && "indirect call");

						if ( callTable->isCallBus() ) {
							unsigned fncId = callTable->getFuncId(nextF);
							*out << "\tif(receive_finish == " << fncBits << "'d";
							*out << fncId << ")\n";
						}
						else {
							*out << "\tif(" << nextF->getName() << "_finish == 1)\n";
						}
//						*out << getNameFromValue(fcnPtr, fnc, stateIter, i) << ")\n";
						*out << "\t\tnext_state = State_" << (nextStates.front())->getStateNumber();
						*out << "_0;\n";
					}
					else if ( isa<CallInst>(exitInst) )
					{
						assert(nextStates.size() == 2); // don't consider indirect call
						Function *nextF = (nextStates.front())->getParentFunction();

						if ( callTable->isCallBus() ) {
							unsigned fncId = callTable->getFuncId(nextF);
							*out << "\tif(receive_finish == " << fncBits << "'d";
							*out << fncId << ")\n";
						}
						else {
							*out << "\tif(" << nextF->getName() << "_finish == 1)\n";
						}
						*out << "\t\tnext_state = State_" << (nextStates.back())->getStateNumber();
						*out << "_0;\n";
					}
					else
						assert(0&&"????\n");
				}

				if ( hasParallelCall )
					*out << "end\n";


				if ( (sState == stateIter) && (i == 0) ) 
					*out << "end\n";

			} // state latency loop
		}//state iter

		*out << "default:\n\tnext_state = cur_state;\n";
		*out << "endcase\n";
//		if ( memoryTable->isByteAddressedRAM() )
		if ( hasMC ) 
			*out << "end\n";
		*out << "end\n\n";
	}

	void PrintVerilog::printFunctionStateParameter(Function *func, raw_fd_ostream *funcFile) {
		list<State *> fncStateList = fsm->getStateListOfFunction(func);
		State *sState = fsm->getStartStateFromFnc(func);
		
		unsigned stateCount = 0; // 0 is wait state
		for ( auto stateIter : fncStateList )
			stateCount += stateIter->getLatency();

		*funcFile<< "reg [" << stateCount-1 << ":0] cur_state;\n";
		*funcFile<< "reg [" << stateCount-1 << ":0] next_state;\n";

		*funcFile<< "parameter State_Wait = " << stateCount << "'d0;\n";
		
		unsigned stateNum = 1;
		for ( auto stateIter : fncStateList )
			for ( unsigned i = 0; i < stateIter->getLatency(); i++ )
			{	
				*funcFile<< "parameter State_";
				*funcFile<< stateIter->getStateNumber() << "_" << i << " = ";
				*funcFile<< stateCount << "'b";
				for ( unsigned j = stateCount; j > 0; j-- )
				{
					if ( j == stateNum )
						*funcFile<< "1";
					else
						*funcFile<< "0";
				}
				*funcFile<< ";\n";
				stateNum++;
			}
		*funcFile<< "\n";

		stateNum = 0;
		for ( auto stateIter : fncStateList )
			for ( unsigned i = 0; i < stateIter->getLatency(); i++ )
			{
				*funcFile<< "wire is_State_";
				*funcFile<< stateIter->getStateNumber() << "_" << i << ";\n";
				*funcFile<< "assign is_State_" << stateIter->getStateNumber() << "_" << i;
				if ( (stateIter == sState) && (i == 0) )
					*funcFile<< " = cur_state[" << stateNum << "] && start;\n\n";
				else
					*funcFile<< " = cur_state[" << stateNum << "];\n\n";

				stateNum += 1;
			}
		*funcFile<< "\n";

		//For Pipelining
		for ( auto stateIter : fncStateList )
			if ( stateIter->isPipelineState() ) {
				PipelineState *pState = fsm->getPipelineState(stateIter);
				printStageParameter(pState, funcFile);
			}
	}

	void PrintVerilog::printPipelineRegister(PipelineState *state, Function *func,
																					raw_fd_ostream *funcFile) {
		for ( auto inst : state->getInstList() ) {
			unsigned genTiming = state->getESchFromInst(inst);
			for ( unsigned i = 0; i <= state->getMaxEnd(); ++i ) {
				if ( state->isUsedInThisStage(i, inst) || genTiming == i ) {
					if ( isa<StoreInst>(inst) )
						continue;

					printRegDeclaration(inst, funcFile);
					printNameOfInst(inst, func, funcFile);
					*funcFile << "_reg_" << i << ";\n";
				}
				if ( state->isUsedInPHINode(i, inst) ) {
					if ( isa<StoreInst>(inst) )
						continue;

					printRegDeclaration(inst, funcFile);
					printNameOfInst(inst, func, funcFile);
					*funcFile << "_reg_phi;\n";
				}
			}
		}
	}

	void PrintVerilog::printFunctionRegister(Function *func, raw_fd_ostream *funcFile) {
		*funcFile << "reg start_reg;\n";
		//Arguments
		for ( unsigned i = 0; i < func->arg_size(); ++i ) {
			*funcFile << "reg [" << callTable->getFuncArgBit(func, i);
			*funcFile << ":0] arg_" << i << "_reg;\n";
		}
		*funcFile << "\n";


		//Memories
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			bool isDual = memoryTable->isDualPortRAM(ram);
			bool hasMC = ram->hasMultipleSize() || ram->hasExPort();

			*funcFile << "wire [" << ram->getDataBitSize()-1 << ":0] ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_a;\n";
//			if ( memoryTable->isByteAddressedRAM() ) {
//			if ( hasMC ) {
				*funcFile << "reg [" << ram->getDataBitSize()-1 << ":0] ";
				*funcFile << "RAM" << ram->getRAMId() << "_data_in_a_stall;\n";
//			}
			*funcFile << "reg [" << ram->getDataBitSize()-1 << ":0] ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_a_call;\n";

			if ( isDual ) {
				*funcFile << "wire [" << ram->getDataBitSize()-1 << ":0] ";
				*funcFile << "RAM" << ram->getRAMId() << "_data_in_b;\n";
//				if ( memoryTable->isByteAddressedRAM() ) {
//				if ( hasMC ) {
					*funcFile << "reg [" << ram->getDataBitSize()-1 << ":0] ";
					*funcFile << "RAM" << ram->getRAMId() << "_data_in_b_stall;\n";
//				}
				*funcFile << "reg [" << ram->getDataBitSize()-1 << ":0] ";
				*funcFile << "RAM" << ram->getRAMId() << "_data_in_b_call;\n";
			}

			if ( (memoryTable->getPrivateAccessInst(func, ram)).size() == 0 )
				continue;

			*funcFile << "reg [" << ram->getElementBitSize()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_address_a_p;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_read_en_a_p;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_write_en_a_p;\n";
			*funcFile << "reg [" << ram->getDataBitSize()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_a_p;\n";
//			if ( memoryTable->isByteAddressedRAM() ) {
			if ( hasMC )
				*funcFile << "reg [3:0] RAM" << ram->getRAMId() << "_size_a_p;\n";

			if ( isDual ) {
				*funcFile << "reg [" << ram->getElementBitSize()-1;
				*funcFile << ":0] RAM" << ram->getRAMId() << "_address_b_p;\n";
				*funcFile << "reg RAM" << ram->getRAMId() << "_read_en_b_p;\n";
				*funcFile << "reg RAM" << ram->getRAMId() << "_write_en_b_p;\n";
				*funcFile << "reg [" << ram->getDataBitSize()-1;
				*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_b_p;\n";
				if ( hasMC )
					*funcFile << "reg [3:0] RAM" << ram->getRAMId() << "_size_b_p;\n";
			}

			*funcFile << "\n";
		}

		//XXX: Now, If There is Unresolved Instruction, all RAM in this container
		if ( (memoryTable->getAddressAccessRAMFromFunction(func)).size() != 0 ) {
			unsigned addressBitSize = memoryTable->getRAMBitSize() + memoryTable->getElementBitSize();
			unsigned bitWidth = memoryTable->getBitWidth();

			bool isDual = false;
			for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
				isDual = isDual || memoryTable->isDualPortRAM(ram);

			*funcFile << "reg [" << addressBitSize-1 << ":0] address_a;\n";
			*funcFile << "reg read_en_a;\n";
			*funcFile << "reg write_en_a;\n";
			*funcFile << "reg [" << bitWidth-1 << ":0] data_out_a;\n";
			*funcFile << "wire [" << bitWidth-1 << ":0] data_in_a;\n";
			*funcFile << "reg [" << bitWidth-1 << ":0] data_in_a_call;\n";
			if ( memoryTable->isByteAddressedRAM() )
				*funcFile << "reg [3:0] size_a;\n";

			if ( isDual ) {
				*funcFile << "reg [" << addressBitSize-1 << ":0] address_b;\n";
				*funcFile << "reg read_en_b;\n";
				*funcFile << "reg write_en_b;\n";
				*funcFile << "reg [" << bitWidth-1 << ":0] data_out_b;\n";
				*funcFile << "wire [" << bitWidth-1 << ":0] data_in_b;\n";
				*funcFile << "reg [" << bitWidth-1 << ":0] data_in_b_call;\n";
				if ( memoryTable->isByteAddressedRAM() ) 
					*funcFile << "reg [3:0] size_b;\n";
			}

			*funcFile << "\n";
		}
		for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
		{
			bool isDual = memoryTable->isDualPortRAM(ram);
			bool hasMC = ram->hasMultipleSize() || ram->hasExPort();

			*funcFile << "reg [" << ram->getElementBitSize()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_address_a_a;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_read_en_a_a;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_write_en_a_a;\n";
			*funcFile << "reg [" << ram->getDataBitSize()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_a_a;\n";
//			if ( memoryTable->isByteAddressedRAM() ) {
			if ( hasMC )
				*funcFile << "reg [3:0] RAM" << ram->getRAMId() << "_size_a_a;\n";

			if ( isDual ) {
				*funcFile << "reg [" << ram->getElementBitSize()-1;
				*funcFile << ":0] RAM" << ram->getRAMId() << "_address_b_a;\n";
				*funcFile << "reg RAM" << ram->getRAMId() << "_read_en_b_a;\n";
				*funcFile << "reg RAM" << ram->getRAMId() << "_write_en_b_a;\n";
				*funcFile << "reg [" << ram->getDataBitSize()-1;
				*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_b_a;\n";
				if ( hasMC ) 
					*funcFile << "reg [3:0] RAM" << ram->getRAMId() << "_size_b_a;\n";
			}


			*funcFile << "\n";
		}
		*funcFile << "\n";

		bool hasMC = false;
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			hasMC = hasMC || (ram->hasMultipleSize() || ram->hasExPort());

//		if ( memoryTable->isByteAddressedRAM() ) {
		if ( hasMC ) {
			*funcFile << "wire memory_stall;\n";
			*funcFile << "wire memory_stall_a;\n";
			*funcFile << "wire memory_stall_b;\n";
			*funcFile << "reg stall_before;\n";

			*funcFile << "assign memory_stall_a = ";
			for ( auto ram : memoryTable->getAccessRAMFromFunction(func) ) {
				if ( ram->hasMultipleSize() || ram->hasExPort() ) {
					*funcFile << "((RAM" << ram->getRAMId() << "_read_en_a | ";
					*funcFile << "RAM" << ram->getRAMId() << "_write_en_a) & ";
					*funcFile << "~RAM" << ram->getRAMId() << "_memory_valid_a) | ";
				}
			}
			*funcFile << "0;\n";
			*funcFile << "assign memory_stall_b = ";
			for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
				if ( ram->hasMultipleSize() || ram->hasExPort() ) {
				if ( memoryTable->isDualPortRAM(ram) ) {
					*funcFile << "((RAM" << ram->getRAMId() << "_read_en_b | ";
					*funcFile << "RAM" << ram->getRAMId() << "_write_en_b) & ";
					*funcFile << "~RAM" << ram->getRAMId() << "_memory_valid_b) | ";
				}
				}
			*funcFile << "0;\n";

			*funcFile << "assign memory_stall = memory_stall_a | memory_stall_b;\n\n";
		}

		//Call Related
		*funcFile << "wire call_function;\n";
		*funcFile << "reg call_function_reg;\n";
		*funcFile << "wire update_time;\n";
		*funcFile << "wire replace_time;\n";
		*funcFile << "assign call_function = (0";
		for ( auto callee : callTable->getCalleeFromFunction(func) )
			*funcFile << " || " << callee->getName() << "_start";
		*funcFile << ")\n && !(0";
		for ( auto callee : callTable->getCalleeFromFunction(func) )
			*funcFile << " || " << callee->getName() << "_finish";
		*funcFile << ");\n";
		*funcFile << "assign update_time = call_function && !call_function_reg;\n";
		*funcFile << "assign replace_time = (0";
		for ( auto callee : callTable->getCalleeFromFunction(func) )
			*funcFile << " || " << callee->getName() << "_start";
		*funcFile << ") && call_function_reg;\n\n";

		//parallel callsite related
		for ( auto callee : callTable->getCalleeFromFunction(func) )
		{
			bool parallelFunction = false;
			for ( auto inst : callTable->getCallInstFromFunction(func, callee) )
			{
				State *state = fsm->getStateFromInst(inst);
				if ( CallInst *callInst = dyn_cast<CallInst>(inst) )
					if ( state->isParallelCallSite(callInst) )
						parallelFunction = true;
			}

			if ( parallelFunction ) {
				*funcFile << "reg " << callee->getName() << "_start_reg;\n";
				*funcFile << "reg " << callee->getName() << "_finish_reg;\n";
			}
		}

		//Operations
		list<State *> fncStateList = fsm->getStateListOfFunction(func);
		for ( auto stateIter : fncStateList )
		{
			
			if ( stateIter->isPipelineState() ) {
				PipelineState *pState = fsm->getPipelineState(stateIter);
				printPipelineRegister(pState, func, funcFile);
				continue;
			}
			for ( auto inst : stateIter->getInstList() )
			{
				if ( isa<StoreInst>(inst) || isa<AllocaInst>(inst) )
					continue;

				if ( isa<PHINode>(inst) )
					printRegDeclaration(inst, funcFile);
				else if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst) ) {
					unsigned addressBitSize 
						= memoryTable->getGEPAddrSize(gep);
//						= memoryTable->getRAMBitSize() + memoryTable->getElementBitSize();
					*funcFile << "wire [" << addressBitSize-1 << ":0] ";
				}
				else if ( isa<CallInst>(inst) && (inst->getType()->isVoidTy()) ) {
				}
				else
					printWireDeclaration(inst, funcFile);

				//parallel callsite
				if ( !(isa<CallInst>(inst) && (inst->getType()->isVoidTy())) )
					printNameOfInst(inst, func, funcFile);
				*funcFile << ";\n";


				if ( stateIter->isUsedAT(inst) ) {
					if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst) ) {
					unsigned addressBitSize 
						= memoryTable->getGEPAddrSize(gep);
//						= memoryTable->getRAMBitSize() + memoryTable->getElementBitSize();
					*funcFile << "reg [" << addressBitSize-1 << ":0] ";
					}
					else
						printRegDeclaration(inst, funcFile);
					printNameOfInst(inst, func, funcFile);
					*funcFile << "_reg;\n";	
				}
				else if ( isa<CallInst>(inst) && !(inst->getType()->isVoidTy()) ) {//parallel callsite
					printRegDeclaration(inst, funcFile);
					printNameOfInst(inst, func, funcFile);
					*funcFile << "_reg;\n";	
				}
			}
			Instruction *exitInst = stateIter->getExitInst();
			assert(exitInst);
			if ( (isa<InvokeInst>(exitInst) || isa<CallInst>(exitInst)) &&
					!((exitInst->getType())->isVoidTy()) ) {
				printRegDeclaration(exitInst, funcFile);
				printNameOfInst(exitInst, func, funcFile);
				*funcFile << "_reg;\n";
			}
		}
		*funcFile << "\n\n";
	}

	void PrintVerilog::printFunctionDeclaration(Function *func, raw_fd_ostream *funcFile) {
		//For Address Space Define
		printDefinedParameter(funcFile);

		if ( verilogConfigInfo->getPrivateBuffer() )
			*funcFile << "(* keep_hierarchy = \"yes\" *)";

		*funcFile << "module " << func->getName() << " (\n";
		//Memory Related
		/*
		if ( verilogConfigInfo->getCentralizedMemory() ) {
			for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			{
				bool isPrivateRAM = true;
				for ( auto addRAM : memoryTable->getAddressAccessRAMFromFunction(func) )
					if ( ram == addRAM ) {
						isPrivateRAM = false;
						break;
					}

				if ( isPrivateRAM ) {
					bool isDual = memoryTable->isDualPortRAM(ram);
					bool hasMC = ram->hasMultipleSize() || ram->hasExPort();

					*funcFile << "output [" << ram->getElementBitSize()-1;
					*funcFile << ":0] RAM" << ram->getRAMId() << "_address_a,\n";
					*funcFile << "output RAM" << ram->getRAMId() << "_read_en_a,\n";
					*funcFile << "output RAM" << ram->getRAMId() << "_write_en_a,\n";
					*funcFile << "input [" << ram->getDataBitSize()-1;
					*funcFile << ":0] RAM" << ram->getRAMId() << "_data_in_a_input,\n";
					*funcFile << "output [" << ram->getDataBitSize()-1;
					*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_a,\n";
					//			if ( memoryTable->isByteAddressedRAM() ) {
					if ( hasMC ) {
						*funcFile << "input RAM" << ram->getRAMId() << "_memory_valid_a,\n";
						*funcFile << "output [3:0] RAM" << ram->getRAMId() << "_size_a,\n";
						if ( isUsedInMemFunc(ram) && isMemFunction(func) ) {
							*funcFile << "output [" << ram->getElementBitSize()-1;
							*funcFile << ":0] RAM" << ram->getRAMId() << "_address_a_ex,\n";
							*funcFile << "output RAM" << ram->getRAMId() << "_select_en_a_ex,\n";
							*funcFile << "output RAM" << ram->getRAMId() << "_write_en_a_ex,\n";
							*funcFile << "input [" << memoryTable->getMemBitWidth()-1;
							*funcFile << ":0] RAM" << ram->getRAMId() << "_data_in_a_ex,\n";
							*funcFile << "output [" << memoryTable->getMemBitWidth()-1;
							*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_a_ex,\n";
						}
					}

					if ( isDual ) {
						*funcFile << "output [" << ram->getElementBitSize()-1;
						*funcFile << ":0] RAM" << ram->getRAMId() << "_address_b,\n";
						*funcFile << "output RAM" << ram->getRAMId() << "_read_en_b,\n";
						*funcFile << "output RAM" << ram->getRAMId() << "_write_en_b,\n";
						*funcFile << "input [" << ram->getDataBitSize()-1;
						*funcFile << ":0] RAM" << ram->getRAMId() << "_data_in_b_input,\n";
						*funcFile << "output [" << ram->getDataBitSize()-1;
						*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_b,\n";
						//				if ( memoryTable->isByteAddressedRAM() ) {
						if ( hasMC ) {
							*funcFile << "input RAM" << ram->getRAMId() << "_memory_valid_b,\n";
							*funcFile << "output [3:0] RAM" << ram->getRAMId() << "_size_b,\n";
							if ( isUsedInMemFunc(ram) && isMemFunction(func) ) {
								*funcFile << "output [" << ram->getElementBitSize()-1;
								*funcFile << ":0] RAM" << ram->getRAMId() << "_address_b_ex,\n";
								*funcFile << "output RAM" << ram->getRAMId() << "_select_en_b_ex,\n";
								*funcFile << "output RAM" << ram->getRAMId() << "_write_en_b_ex,\n";
								*funcFile << "input [" << memoryTable->getMemBitWidth()-1;
								*funcFile << ":0] RAM" << ram->getRAMId() << "_data_in_b_ex,\n";
								*funcFile << "output [" << memoryTable->getMemBitWidth()-1;
								*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_b_ex,\n";
							}
						}
					}

				}
			}

			auto addRAMList = memoryTable->getAddressAccessRAMFromFunction(func);
			if ( addRAMList.size() != 0 ) {
				unsigned addressBitSize = memoryTable->getRAMBitSize() + memoryTable->getElementBitSize();
				unsigned bitWidth = memoryTable->getBitWidth();

				*funcFile << "output reg [" << addressBitSize -1 << ":0] RAM_address_a,\n";
				*funcFile << "output reg RAM_read_en_a,\n";
				*funcFile << "output reg RAM_write_en_a,\n";
				*funcFile << "input [" << bitWidth -1 << ":0] RAM_data_in_a,\n";
				*funcFile << "output reg [" << bitWidth -1 << ":0] RAM_data_out_a,\n";

				*funcFile << "output reg [" << addressBitSize -1 << ":0] RAM_address_b,\n";
				*funcFile << "output reg RAM_read_en_b,\n";
				*funcFile << "output reg RAM_write_en_b,\n";
				*funcFile << "input [" << bitWidth -1 << ":0] RAM_data_in_b,\n";
				*funcFile << "output reg [" << bitWidth -1 << ":0] RAM_data_out_b,\n";
			}

		}
		else {
		*/
			for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			{
				bool isDual = memoryTable->isDualPortRAM(ram);
				bool hasMC = ram->hasMultipleSize() || ram->hasExPort();

				*funcFile << "output [" << ram->getElementBitSize()-1;
				*funcFile << ":0] RAM" << ram->getRAMId() << "_address_a,\n";
				*funcFile << "output RAM" << ram->getRAMId() << "_read_en_a,\n";
				*funcFile << "output RAM" << ram->getRAMId() << "_write_en_a,\n";
				*funcFile << "input [" << ram->getDataBitSize()-1;
				*funcFile << ":0] RAM" << ram->getRAMId() << "_data_in_a_input,\n";
				*funcFile << "output [" << ram->getDataBitSize()-1;
				*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_a,\n";
				//			if ( memoryTable->isByteAddressedRAM() ) {
				if ( hasMC ) {
					*funcFile << "input RAM" << ram->getRAMId() << "_memory_valid_a,\n";
					*funcFile << "output [3:0] RAM" << ram->getRAMId() << "_size_a,\n";
					if ( isUsedInMemFunc(ram) && isMemFunction(func) ) {
						*funcFile << "output [" << ram->getElementBitSize()-1;
						*funcFile << ":0] RAM" << ram->getRAMId() << "_address_a_ex,\n";
						*funcFile << "output RAM" << ram->getRAMId() << "_select_en_a_ex,\n";
						*funcFile << "output RAM" << ram->getRAMId() << "_write_en_a_ex,\n";
						*funcFile << "input [" << memoryTable->getMemBitWidth()-1;
						*funcFile << ":0] RAM" << ram->getRAMId() << "_data_in_a_ex,\n";
						*funcFile << "output [" << memoryTable->getMemBitWidth()-1;
						*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_a_ex,\n";
					}
				}

				if ( isDual ) {
					*funcFile << "output [" << ram->getElementBitSize()-1;
					*funcFile << ":0] RAM" << ram->getRAMId() << "_address_b,\n";
					*funcFile << "output RAM" << ram->getRAMId() << "_read_en_b,\n";
					*funcFile << "output RAM" << ram->getRAMId() << "_write_en_b,\n";
					*funcFile << "input [" << ram->getDataBitSize()-1;
					*funcFile << ":0] RAM" << ram->getRAMId() << "_data_in_b_input,\n";
					*funcFile << "output [" << ram->getDataBitSize()-1;
					*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_b,\n";
					//				if ( memoryTable->isByteAddressedRAM() ) {
					if ( hasMC ) {
						*funcFile << "input RAM" << ram->getRAMId() << "_memory_valid_b,\n";
						*funcFile << "output [3:0] RAM" << ram->getRAMId() << "_size_b,\n";
						if ( isUsedInMemFunc(ram) && isMemFunction(func) ) {
							*funcFile << "output [" << ram->getElementBitSize()-1;
							*funcFile << ":0] RAM" << ram->getRAMId() << "_address_b_ex,\n";
							*funcFile << "output RAM" << ram->getRAMId() << "_select_en_b_ex,\n";
							*funcFile << "output RAM" << ram->getRAMId() << "_write_en_b_ex,\n";
							*funcFile << "input [" << memoryTable->getMemBitWidth()-1;
							*funcFile << ":0] RAM" << ram->getRAMId() << "_data_in_b_ex,\n";
							*funcFile << "output [" << memoryTable->getMemBitWidth()-1;
							*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_b_ex,\n";
						}
					}
				}

			}

//		}

		//Operator Related
		for ( auto opType : operatorTable->getUsedOperatorByFunction(func) )
		{
			unsigned opCode = opType.first;
			unsigned bitWidth = opType.second;
			*funcFile << "output reg [" << bitWidth -1 << ":0] OP_" << opCode << "_";
			*funcFile << bitWidth << "_arg_0,\n";
			*funcFile << "output reg OP_" << opCode << "_" << bitWidth << "_arg_0_valid,\n";
			*funcFile << "output reg [" << bitWidth -1 << ":0] OP_" << opCode << "_";
			*funcFile << bitWidth << "_arg_1,\n";
			*funcFile << "output reg OP_" << opCode << "_" << bitWidth << "_arg_1_valid,\n";
			*funcFile << "input [" << bitWidth -1 << ":0] OP_" << opCode << "_";
			*funcFile << bitWidth << "_return,\n";
		}

		//Call Related
		if ( callTable->isCallBus() && (callTable->getCalleeFromFunction(func)).size() != 0 ) {
			for ( unsigned i = 0; i < callTable->getMaxArgNum(); ++i ) 
				*funcFile << "input [" << callTable->getArgBitWidth(i) -1 << ":0] arg_" << i << ",\n";
			for ( unsigned i = 0; i < callTable->getMaxArgNum(); ++i )
			{
				*funcFile << "output reg [" << callTable->getArgBitWidth(i) -1 << ":0] send_arg_";
				*funcFile << i << ",\n";
			}
			*funcFile << "input [" << callTable->getFuncBitSize() -1 << ":0] start,\n";
			*funcFile << "output reg [" << callTable->getFuncBitSize() -1 << ":0] send_start,\n";
			*funcFile << "output reg [" << callTable->getFuncBitSize() -1 << ":0] finish,\n";
			*funcFile << "input [" << callTable->getFuncBitSize() -1 << ":0] receive_finish,\n";
			*funcFile << "output reg [" << callTable->getMaxReturnBitWidth() -1<< ":0] return_val,\n";
			*funcFile<< "input [" <<callTable->getMaxReturnBitWidth()-1<< ":0] receive_return_val,\n";
		}
		else {
			for ( auto callee : callTable->getCalleeFromFunction(func) )
			{
				for ( unsigned arg = 0; arg < callee->arg_size(); ++arg ) {
					*funcFile << "output reg [" << callTable->getFuncArgBit(callee, arg) -1;
					*funcFile << ":0] " << callee->getName() << "_arg_" << arg << ",\n";
				}
				*funcFile << "output reg " << callee->getName() << "_start,\n";
				*funcFile << "input " << callee->getName() << "_finish,\n";
				*funcFile << "input [" << callTable->getFuncReturnBit(callee)-1;
				*funcFile << ":0] " << callee->getName() << "_return_val,\n";
			}
			
			for ( unsigned arg = 0; arg < func->arg_size(); ++arg ) {
				*funcFile << "input [" << callTable->getFuncArgBit(func, arg) -1;
				*funcFile << ":0] arg_" << arg << ",\n";
			}

			*funcFile << "input start,\n";
			*funcFile << "output reg finish,\n";

			*funcFile << "output reg ["<<callTable->getFuncReturnBit(func) -1 << ":0] return_val,\n";
		}
		*funcFile << "input clk,\n";
		*funcFile << "input reset\n";
		*funcFile << ");\n\n";
	}

	void PrintVerilog::printFunctionNormalOperation(Function *func, raw_fd_ostream *funcFile) {
		// Argument Register
		*funcFile << "always @(posedge clk) begin\n";
		if ( callTable->isCallBus() ) {
			*funcFile << "\tif(start == " << callTable->getFuncBitSize() << "'d";
			*funcFile << callTable->getFuncId(func) << ") begin\n";
			for ( unsigned i = 0; i < callTable->getMaxArgNum(); ++i )
				*funcFile << "arg_" << i << "_reg <= arg_" << i << ";\n";
			*funcFile << "\tend\n";
		}
		else {
			*funcFile << "\tif(start == 1'd1) begin\n";
			for ( unsigned i = 0; i < func->arg_size(); ++i )
				*funcFile << "arg_" << i << "_reg <= arg_" << i << ";\n";
			*funcFile << "\tend\n";
		}
		*funcFile << "end\n\n";

		bool hasMC = false;
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			hasMC = hasMC || (ram->hasMultipleSize() || ram->hasExPort());

//		if ( memoryTable->isByteAddressedRAM() ) {
		if ( hasMC ) {
			*funcFile << "always @(posedge clk) begin\n";
			*funcFile << "\tstall_before <= memory_stall;\n";
			*funcFile << "end\n\n";

			*funcFile << "always @(posedge clk) begin\n";
			*funcFile << "\tif ( memory_stall ) begin\n";
			for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			{
//				if ( ram->hasMultipleSize() || ram->hasExPort() ) {
					*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_in_a_stall <= ";
					*funcFile << "RAM" << ram->getRAMId() << "_data_in_a_input;\n";
					if ( memoryTable->isDualPortRAM(ram) ) {
						*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_in_b_stall <= ";
						*funcFile << "RAM" << ram->getRAMId() << "_data_in_b_input;\n";
					}
//				}
			}
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";

			for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			{
//				if ( ram->hasMultipleSize() || ram->hasExPort() ) {
					*funcFile << "assign RAM" << ram->getRAMId() << "_data_in_a	= stall_before ? ";
					*funcFile << "RAM" << ram->getRAMId() << "_data_in_a_stall : ";
					*funcFile << "RAM" << ram->getRAMId() << "_data_in_a_input;\n";
					if ( memoryTable->isDualPortRAM(ram) ) {
						*funcFile << "assign RAM" << ram->getRAMId() << "_data_in_b	= stall_before ? ";
						*funcFile << "RAM" << ram->getRAMId() << "_data_in_b_stall : ";
						*funcFile << "RAM" << ram->getRAMId() << "_data_in_b_input;\n";
					}
//				}
			}
		}
		else {
			for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			{
				*funcFile << "assign RAM" << ram->getRAMId() << "_data_in_a	= ";
				*funcFile << "RAM" << ram->getRAMId() << "_data_in_a_input;\n";
				if ( memoryTable->isDualPortRAM(ram) ) {
					*funcFile << "assign RAM" << ram->getRAMId() << "_data_in_b	= ";
					*funcFile << "RAM" << ram->getRAMId() << "_data_in_b_input;\n";
				}
			}
		}
		*funcFile << "\n";

		//Memory Load Store and
		//Call Related ( Call timing == load & store timing )
		*funcFile << "always @(posedge clk) begin\n";
//		if ( memoryTable->isByteAddressedRAM() )
		if ( hasMC )
			*funcFile << "\tif ( !memory_stall )\n";
		*funcFile << "\t\tcall_function_reg <= call_function;\n";
		*funcFile << "\tif ( update_time ) begin\n";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_in_a_call <= ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_a;\n";
			if ( memoryTable->isDualPortRAM(ram) ) {
				*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_in_b_call <= ";
				*funcFile << "RAM" << ram->getRAMId() << "_data_in_b;\n";
			}
		}
		*funcFile << "\tend\n";
		if ( (memoryTable->getAddressAccessRAMFromFunction(func)).size() != 0 ) {
			*funcFile << "\tdata_in_a_call <= data_in_a;\n";
			bool isDual = false;
			for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
				isDual = isDual || memoryTable->isDualPortRAM(ram);

			if ( isDual )
				*funcFile << "\tdata_in_b_call <= data_in_b;\n";
		}
		*funcFile << "end\n\n";


		//Un Shareable Operation s
		list<State *> funcStateList = fsm->getStateListOfFunction(func);
		for ( auto state : funcStateList )
		{
			bool isPipelineState = false;
			if ( state->isPipelineState() ) {
				isPipelineState = true;
				//TODO : print Pipeline State
				//XXX: Pipeline State get private operators even if the operator is shareable.
				continue;
			}

			list<Instruction *> instList = state->getInstList();
			for ( auto inst : instList )
			{
				if ( operatorTable->isShareableUser(inst) ) 
					continue;

				unsigned useTiming = state->getSchFromInst(inst);

				if ( PHINode *phiInst = dyn_cast<PHINode>(inst) ) {
					*funcFile << "always @(*) begin\n";
					*funcFile << "//" << *inst << "\n";
					unsigned numIncoming = phiInst->getNumIncomingValues();
					for ( unsigned i = 0; i < numIncoming; ++i )
					{
						BasicBlock *comeBB = phiInst->getIncomingBlock(i);
						BasicBlock::iterator bi = comeBB->end();
						Instruction *lastInst = &*(--bi);
						State *comeState = fsm->getStateFromInst(lastInst);
						Value *comeV = phiInst->getIncomingValue(i);
						useTiming = comeState->getLatency()-1;

						*funcFile << "\tif(1'b1 == is_State_" << comeState->getStateNumber();
						*funcFile << "_" << comeState->getLatency()-1 << " && ~reset)\n";
						*funcFile << "\t\t";
						printNameOfInst(inst, func, funcFile);
						*funcFile << " = ";
						if ( isSignedValue( comeV ) )
							*funcFile << "$signed(";
						printNameFromValue(comeV, func, comeState, useTiming, funcFile);
						if ( isSignedValue( comeV ) )
							*funcFile << ")";
						*funcFile << ";\n";
					}
					*funcFile << "end\n\n";
				}
				else if ( GetElementPtrInst * gepInst = dyn_cast<GetElementPtrInst>(inst) ) {
					Value *pV = gepInst->getPointerOperand();
					assert(pV);
					Type *ty = pV->getType();
					PointerType *pTy = dyn_cast<PointerType>(ty);
					assert(pTy && "Type of GelementPtr should be pointer type\n");
					Type *eTy = pTy->getElementType();
					assert(eTy && "Cannot find Element Type\n");

					*funcFile << "//" << *inst << "\n";
					*funcFile << "assign ";
					printNameOfInst(inst, func, funcFile);
					*funcFile << " = (";
					//Pointer Value
					printNameFromValue(pV, func, state, useTiming, funcFile);

					unsigned numOps = gepInst->getNumOperands();
					Value *operand;

					/*
					unsigned singleElementStruct = 0;
					bool singleElementIsSequential = false;
					if ( StructType *firstStructType = dyn_cast<StructType>(eTy) ) {
						singleElementStruct = firstStructType->getNumElements();
						if ( singleElementStruct == 1 ) {
							Type *elementType = firstStructType->getElementType(0);
							if ( isa<SequentialType>(elementType) ) {
								singleElementIsSequential = true;
								eTy = elementType;
							}
						}
					}*/

					if ( numOps > 2 ) {

						//XXX: Assumption : inbound sign -> do not skip first operand
						// no inbound sign -> skip first operand

//						if ( (gepInst->isInBounds() && isa<SequentialType>(eTy)) ||
//								(singleElementStruct == 1 && singleElementIsSequential) ) {

						if ( gepInst->isInBounds() && isa<CompositeType>(eTy) ) {
							Value *operand = gepInst->getOperand(1);
							unsigned offset = getNumOfElementsFromType(eTy);
							*funcFile << " + ( (" << offset << " * ";
							printNameFromValue(operand, func, state, useTiming, funcFile);
							*funcFile << ") ";
							printByteAddressShift(eTy, funcFile);
							*funcFile << " )";
						}
						else {
							Value *zeroV = gepInst->getOperand(1);
							if ( !isa<ConstantInt>(zeroV) ) {
								inst->dump();
								zeroV->dump();
							}
							assert(isa<ConstantInt>(zeroV));
							assert(dyn_cast<ConstantInt>(zeroV)->getSExtValue() == 0);
						}

						for (unsigned i = 2; i < numOps; ++i )
						{
							operand = gepInst->getOperand(i);

							if ( StructType *sTy = dyn_cast<StructType>(eTy) ) {
								ConstantInt *offsetInt= dyn_cast<ConstantInt>(operand);
								assert(offsetInt && "struct type should have constant offset value");
								unsigned operandConst = offsetInt->getSExtValue();

								bool compactStruct = false;
								std::set<Value *> mset = pa->getPointedMemory(pV);
								for ( auto iter : mset )
									if ( memoryTable->hasStruct(iter) )
										if ( memoryTable->isCompactStruct(iter) ) {
											Type *compactTy = iter->getType();
											PointerType *compactPTy = dyn_cast<PointerType>(compactTy);
											assert(compactPTy);
											Type *compactElementTy = compactPTy->getElementType();
											if ( compactElementTy == eTy )
												compactStruct = true;
										}


								eTy = sTy->getElementType(operandConst);
								unsigned offset = 0;
								for ( int k = 0; k < operandConst; ++k )
								{
									*funcFile << " + ( " << getNumOfElementsFromType(sTy->getElementType(k));
									if ( compactStruct ) 
										printByteAddressShift(sTy->getElementType(k), funcFile);
									else
										printByteAddressShift(sTy, funcFile);
									*funcFile << " ) ";
								}
							}
							else if ( SequentialType *aTy = dyn_cast<SequentialType>(eTy) ) {
								eTy = aTy->getElementType();
								unsigned offset = getNumOfElementsFromType(eTy);

								*funcFile << " + ( (" << offset << " * ";
								printNameFromValue(operand, func, state, useTiming, funcFile);
								*funcFile << ") ";
								printByteAddressShift(eTy, funcFile);
								*funcFile << " )";
							}
							else
								assert(0 && "No Composite Type Used");
						}
					}
					else if ( numOps == 2 ) {
//XXX: I think this type of gep operation is generated because of 
//	failure of pointer tacking by llvm front-end
//						assert(isa<ArrayType>(eTy));
						assert(!isa<StructType>(eTy));
						operand = gepInst->getOperand(1);
						unsigned offset = getNumOfElementsFromType(eTy);
						*funcFile << " + ( (" << offset << " * ";
						printNameFromValue(operand, func, state, useTiming, funcFile);
						*funcFile << ") ";
						printByteAddressShift(eTy, funcFile);
						*funcFile << " )";
					}

					*funcFile << ");\n";
				}
				else if ( isa<SelectInst>(inst) ) {
					//					*funcFile << "always @(*) begin\n";
					*funcFile << "//" << *inst << "\n";
					*funcFile << "assign ";
					printNameOfInst(inst, func, funcFile);
					*funcFile << " = ";
					printNameFromValue(inst->getOperand(0), func, state, useTiming, funcFile);
					*funcFile << " ? ";
					printNameFromValue(inst->getOperand(1), func, state, useTiming, funcFile);
					*funcFile << " : ";
					printNameFromValue(inst->getOperand(2), func, state, useTiming, funcFile);
					*funcFile << ";\n";
					//					end\n\n";
				}
				else if ( isa<SExtInst>(inst) ) {
//					*funcFile << "always @(*) begin\n";
					*funcFile << "//" << *inst << "\n";
					*funcFile << "assign ";
					printNameOfInst(inst, func, funcFile);
					*funcFile << " = $signed(";
					printNameFromValue(inst->getOperand(0), func, state, useTiming, funcFile);
					*funcFile << ");\n";
//					end\n\n";
				}
				else if ( isa<ZExtInst>(inst) ) {
//					*funcFile << "always @(*) begin\n";
					*funcFile << "//" << *inst << "\n";
					*funcFile << "assign ";
					printNameOfInst(inst, func, funcFile);
					*funcFile << " = $unsigned(";
					printNameFromValue(inst->getOperand(0), func, state, useTiming, funcFile);
					*funcFile << ");\n";
//					end\n\n";
				}
				else if ( isa<TruncInst>(inst) 
						|| isa<BitCastInst>(inst) || isa<PtrToIntInst>(inst) ) {
					//This Instructions are automatically handled by register declaration
					
//					*funcFile << "always @(*) begin\n";
					*funcFile << "//" << *inst << "\n";
					*funcFile << "assign ";
					printNameOfInst(inst, func, funcFile);
					*funcFile << " = ";
					printNameFromValue(inst->getOperand(0), func, state, useTiming, funcFile);
					*funcFile << ";\n";
//					end\n\n";
				}
				else if ( AllocaInst *alloca = dyn_cast<AllocaInst>(inst) ) {
					//XXX: Don't have to do ?
				}
				else if ( isa<LoadInst>(inst) || isa<StoreInst>(inst) ) {
					//at printMemory...
				}
				else if ( CallInst *callInst = dyn_cast<CallInst>(inst) ) {
					//call instruction in this list is parallel callsite instruction.
				}
				else {
					//Binary Operations
//					*funcFile << "always @(*) begin\n";
					*funcFile << "//" << *inst << "\n";
					*funcFile << "assign ";
					printNameOfInst(inst, func, funcFile);
					*funcFile << " = ";
					if ( inst->getOpcode() == Instruction::AShr || isSignedBinary(inst) )
						*funcFile << "$signed(";
					printNameFromValue(inst->getOperand(0), func, state, useTiming, funcFile);
					if ( inst->getOpcode() == Instruction::AShr || isSignedBinary(inst) )
						*funcFile << ")";
					printBinaryOperator(inst, funcFile);
					
					if ( isSignedBinary(inst) )
						*funcFile << "$signed(";
					printNameFromValue(inst->getOperand(1), func, state, useTiming, funcFile);
					if ( isSignedBinary(inst) )
						*funcFile << ")";
					*funcFile << ";\n";
//					end\n\n";
				}

				//AT - register including load instruction 
				if ( isa<PHINode>(inst) ) {
					OperationInfo opInfo = verilogConfigInfo->getOperationInfo(inst);
					*funcFile << "always @(posedge clk) begin\n";
					*funcFile << "//" << *inst << "\n";
					*funcFile << "\tif(next_state == State_" << state->getStateNumber();
					*funcFile << "_" << state->getSchFromInst(inst) + opInfo.latency[0];
//					if ( memoryTable->isByteAddressedRAM() )
					if ( hasMC )
						*funcFile << " && !memory_stall";
					*funcFile << ")\n\t\t";
					printNameOfInst(inst, func, funcFile);
					*funcFile << "_reg <= ";
					printNameOfInst(inst, func, funcFile);
					*funcFile << ";\nend\n\n";
				}
				else if ( state->isUsedAT(inst) && !(isa<AllocaInst>(inst)||isa<CallInst>(inst)) ) {
					OperationInfo opInfo = verilogConfigInfo->getOperationInfo(inst);
					*funcFile << "always @(posedge clk) begin\n";
					*funcFile << "//" << *inst << "\n";
					*funcFile << "\tif(1'b1 == is_State_" << state->getStateNumber();
					*funcFile << "_" << state->getSchFromInst(inst) + opInfo.latency[0];
//					if ( memoryTable->isByteAddressedRAM() )
					if ( hasMC )
						*funcFile << " && !memory_stall";
					*funcFile << ")\n\t\t";
					printNameOfInst(inst, func, funcFile);
					*funcFile << "_reg <= ";
					printNameOfInst(inst, func, funcFile);
					*funcFile << ";\nend\n\n";
				}
				else if ( isa<CallInst>(inst) && !(inst->getType()->isVoidTy()) ) {
					//parallel callsite , return val should be done in here
					//start, arg ... signaling will be done in printFunctionCallInstruction
					CallInst *cInst = dyn_cast<CallInst>(inst);
					assert(cInst);
					Function *callee = cInst->getCalledFunction();
					assert(callee && "CallInst return NULL Function PTR?");

					*funcFile << "assign ";
					printNameOfInst(cInst, func, funcFile);
					*funcFile << " = (" << callee->getName() << "_finish && ";
					*funcFile << "!" << callee->getName() << "_finish_reg) || (!" << callee->getName();
					*funcFile << "_finish_reg && (1";
					for ( auto callIter : state->getParallelCallSiteTime(state->getSchFromInst(inst)) )
					{
						Function *thisCallee = callIter->getCalledFunction();
						assert(thisCallee);

						*funcFile << "&& (" << thisCallee->getName() << "_finish ||";
						*funcFile << thisCallee->getName() << "_finish_reg) ";
					}	
					*funcFile << ")) ? ";
					*funcFile << callee->getName() << "_return_val : ";
					printNameOfInst(cInst, func, funcFile);
					*funcFile << "_reg;\n";
					
					*funcFile << "always @(posedge clk) begin\n";
					*funcFile << "//" << *inst << "\n";
					*funcFile << "\tif ( reset ) \n";
					printNameOfInst(cInst, func, funcFile);
					*funcFile << "_reg <= 0;\n\telse ";
					*funcFile << "if( (" << callee->getName() << "_finish && ";
					*funcFile << "!" << callee->getName() << "_finish_reg) || (!" << callee->getName();
					*funcFile << "_finish_reg && (1";
					for ( auto callIter : state->getParallelCallSiteTime(state->getSchFromInst(inst)) )
					{
						Function *thisCallee = callIter->getCalledFunction();
						assert(thisCallee);

						*funcFile << "&& (" << thisCallee->getName() << "_finish ||";
						*funcFile << thisCallee->getName() << "_finish_reg) ";
					}	
					*funcFile << ")) )\n\t\t\t";
					printNameOfInst(cInst, func, funcFile);
					*funcFile << "_reg <= " << callee->getName() << "_return_val;\n";
					*funcFile << "end\n\n";
				}
			}// normal instruction list end

			//exit inst
			Instruction *exitI = state->getExitInst();
			assert(exitI);
			if ( !(exitI->getType())->isVoidTy() ) { // if void type , there is no register declar
				if ( CallInst *cInst = dyn_cast<CallInst>(exitI) ) {
					Function *callee = cInst->getCalledFunction();
					assert(callee && "CallInst return NULL Function PTR?");

					*funcFile << "always @(posedge clk) begin\n";
					*funcFile << "//" << *cInst << "\n";
					*funcFile << "\tif(1'b1 == is_State_" << state->getStateNumber() << "_";
					*funcFile << state->getLatency()-1 << ")\n";
					if ( callTable->isCallBus() ) {
						unsigned funcBits = callTable->getFuncBitSize();
						unsigned funcId = callTable->getFuncId(callee);
						*funcFile << "\t\tif(receive_finish == "<<funcBits<<"'d"<<funcId<<")\n\t\t\t";
						printNameOfInst(cInst, func, funcFile);
						*funcFile << "_reg <= receive_return_val;\n";
					}
					else {
						*funcFile << "\t\tif(" << callee->getName() << "_finish == 1'd1)\n\t\t\t";
						printNameOfInst(cInst, func, funcFile);
						*funcFile << "_reg <= " << callee->getName() << "_return_val;\n";
					}
					*funcFile << "end\n\n";
				}
				else if ( InvokeInst *iInst = dyn_cast<InvokeInst>(exitI) ) {
						//TODO: Indirect Call					
						//				Value *fcnPtr = iInst->getCalledValue();

						Function *callee = iInst->getCalledFunction();
						assert(callee && "TODO : Indirect Call");

						*funcFile << "always @(posedge clk) begin\n";
						*funcFile << "//" << *iInst << "\n";
						*funcFile << "\tif(1'b1 == is_State_" << state->getStateNumber() << "_";
						*funcFile << state->getLatency()-1 << ")\n";
						if ( callTable->isCallBus() ) {
						unsigned funcBits = callTable->getFuncBitSize();
						unsigned funcId = callTable->getFuncId(callee);
						*funcFile << "\t\tif(receive_finish == "<<funcBits<<"'d"<<funcId<<")\n\t\t\t";
						printNameOfInst(iInst, func, funcFile);
						*funcFile << "_reg <= receive_return_val;\n";
						}
						else {
						*funcFile << "\t\tif(" << callee->getName() << "_finish == 1'd1)\n\t\t\t";
						printNameOfInst(iInst, func, funcFile);
						*funcFile << "_reg <= " << callee->getName() << "_return_val;\n";
						}
						*funcFile << "end\n\n";
				}
			}

		}//state list end
	}

	void PrintVerilog::printFunctionCallInstruction(Function *func, raw_fd_ostream *funcFile) {
		if ( callTable->isCallBus() && (callTable->getCalleeFromFunction(func)).size() != 0 ) {
			unsigned funcBits = callTable->getFuncBitSize();
			list<State *> funcStateList = fsm->getStateListOfFunction(func);

			bool firstIf = true;
			*funcFile << "always @(*) begin\n";
			*funcFile << "\tsend__start = 0;\n";
			for ( unsigned i = 0; i < callTable->getMaxArgNum(); ++i )
				*funcFile << "\tsend_arg_" << i << " = 0;\n";
			*funcFile << "\n";
			for ( auto state : funcStateList )
			{
				//No Call Inst in PipelineState
				Instruction *exitI = state->getExitInst();
				assert(exitI);
				unsigned useTiming = state->getLatency() -1;

				if ( CallInst *cInst = dyn_cast<CallInst>(exitI) ) { //TODO: Parallel
					Function *callee = cInst->getCalledFunction();
					unsigned funcId = callTable->getFuncId(callee);
					if ( firstIf ) {
						firstIf = false;
						*funcFile << "\t";
					}
					else
						*funcFile << "\telse ";
					*funcFile << "if (1'b1 == is_State_" << state->getStateNumber();
					*funcFile << "_" << state->getLatency()-1 << ") begin\n";
					*funcFile << "//" << *exitI << "\n";
					*funcFile << "\t\tsend_start = " << funcBits << "'d" << funcId << ";\n";
					for ( unsigned i = 0; i < cInst->getNumArgOperands(); ++i )
					{
						Value *operand = cInst->getArgOperand(i);
						*funcFile << "\t\tsend_arg_" << i << " = ";
						printNameFromValue(operand, func, state, useTiming, funcFile);
						*funcFile << ";\n";
					}
					*funcFile << "\tend\n";
				}
				else if ( InvokeInst *cInst = dyn_cast<InvokeInst>(exitI) ) {
					//TODO: Indirect Call
					Function *callee = cInst->getCalledFunction();
					unsigned funcId = callTable->getFuncId(callee);
					if ( firstIf ) {
						firstIf = false;
						*funcFile << "\t";
					}
					else
						*funcFile << "\telse ";
					*funcFile << "if (1'b1 == is_State_" << state->getStateNumber();
					*funcFile << "_" << state->getLatency()-1 << ") begin\n";
					*funcFile << "//" << *exitI << "\n";
					*funcFile << "\t\tsend_start = " << funcBits << "'d" << funcId << ";\n";
					for ( unsigned i = 0; i < cInst->getNumArgOperands(); ++i )
					{
						Value *operand = cInst->getArgOperand(i);
						*funcFile << "\t\tsend_arg_" << i << " = ";
						printNameFromValue(operand, func, state, useTiming, funcFile);
						*funcFile << ";\n";
					}
					*funcFile << "\tend\n";
				}
			}

			*funcFile << "end\n\n";
		}
		else { // No Bus System
			for ( auto callee : callTable->getCalleeFromFunction(func) )
			{
				bool firstIf = true;
				*funcFile << "//" << callee->getName() << " Function Call\n";
				*funcFile << "always @(*) begin\n";
				*funcFile << "\t" << callee->getName() << "_start = 0;\n";
				for ( unsigned i = 0; i < callee->arg_size(); ++i )
					*funcFile << "\t" << callee->getName() << "_arg_" << i << " = 0;\n";
				*funcFile << "\n";
				for ( auto inst : callTable->getCallInstFromFunction(func, callee) )
				{
					State *state = fsm->getStateFromInst(inst);
					unsigned useTiming = state->getLatency() -1;
					if ( state->isParallelCallSite(dyn_cast<CallInst>(inst)) ) {//parallel callsite
						useTiming = state->getSchFromInst(inst);
					}

					if ( firstIf ) {
						firstIf = false;
						*funcFile << "\t";
					}
					else
						*funcFile << "\telse ";

					if ( state->isParallelCallSite(dyn_cast<CallInst>(inst)) ) {//parallel callsite
						*funcFile << "if (1'b1 == is_State_" << state->getStateNumber();
						*funcFile << "_" << useTiming << " && !" << callee->getName();
						*funcFile << "_start_reg) begin\n";
					}
					else {
						*funcFile << "if (1'b1 == is_State_" << state->getStateNumber();
						*funcFile << "_" << state->getLatency()-1 << ") begin\n";
					}
					*funcFile << "//" << *inst<< "\n";
					*funcFile << "\t\t" << callee->getName() << "_start = 1;\n";
					for ( unsigned i = 0; i < callee->arg_size(); ++i )
					{
						Value *operand = inst->getOperand(i);
						*funcFile << "\t\t" << callee->getName() << "_arg_" << i << " = ";
						printNameFromValue(operand, func, state, useTiming, funcFile);
						*funcFile << ";\n";
					}
					*funcFile << "\tend\n";
				}

				*funcFile << "end\n\n";

				////////////parallel callsite
				for ( auto inst : callTable->getCallInstFromFunction(func, callee) )
				{
					State *state = fsm->getStateFromInst(inst);
					unsigned useTiming = state->getLatency() -1;
					if ( state->isParallelCallSite(dyn_cast<CallInst>(inst)) ) {//parallel callsite
						useTiming = state->getSchFromInst(inst);

						*funcFile << "always @(posedge clk) begin\n";
						*funcFile << "\tif ( reset ) begin\n";
						*funcFile << "\t\t" << callee->getName() << "_start_reg <= 0;\n";
						*funcFile << "\t\t" << callee->getName() << "_finish_reg <= 0;\n";
						*funcFile << "\tend\n\telse begin\n";
						//start reg <= 0
						*funcFile << "\t\tif (1 ";
						for ( auto callIter : state->getParallelCallSiteTime(useTiming) )
						{
							Function *thisCallee = callIter->getCalledFunction();
							assert(thisCallee);

							*funcFile << "&& (" << thisCallee->getName() << "_finish ||";
							*funcFile << thisCallee->getName() << "_finish_reg)\n";
						}
						*funcFile << ")\n";
						*funcFile << "\t\t\t" << callee->getName() << "_start_reg <= 0;\n";
						//start reg <= 1
						*funcFile << "\t\telse if (1'b1 == is_State_" << state->getStateNumber();
						*funcFile << "_" << useTiming << ")\n";
						*funcFile << "\t\t\t" << callee->getName() << "_start_reg <= 1;\n\n";

						//finish reg <= 0
						*funcFile << "\t\tif (1 ";
						for ( auto callIter : state->getParallelCallSiteTime(useTiming) )
						{
							Function *thisCallee = callIter->getCalledFunction();
							assert(thisCallee);

							*funcFile << "&& (" << thisCallee->getName() << "_finish ||";
							*funcFile << thisCallee->getName() << "_finish_reg)\n";
						}
						*funcFile << ")\n";
						*funcFile << "\t\t\t" << callee->getName() << "_finish_reg <= 0;\n";
						//finish reg <= 1
						*funcFile << "\t\telse if (" << callee->getName() << "_finish) \n";
						*funcFile << "\t\t\t" << callee->getName() << "_finish_reg <= 1;\n";

						*funcFile << "\tend\nend\n\n";
					}
				}

			}
		}

		//TODO: call bus
		*funcFile << "always @(posedge clk) begin\n";
		*funcFile << "\tif(reset)\n\t\tstart_reg <= 0;\n";
		*funcFile << "\telse begin\n";
		*funcFile << "\t\tif(finish)\n\t\t\tstart_reg <= 0;\n";
		*funcFile << "\t\telse if (start)\n\t\t\tstart_reg <= 1;\n";
		*funcFile << "\tend\nend\n\n";


		//Return Instructions
		list<State *> funcStateList = fsm->getStateListOfFunction(func);

		bool firstIf = true;
		*funcFile << "always @(*) begin\n";
		*funcFile << "\tfinish = 0;\n";
		*funcFile << "\treturn_val = 0;\n\n";

		/////////////////////
		*funcFile << "\tif ( reset ) begin\n";
		*funcFile << "\t\tfinish = 0;\n";
		*funcFile << "\t\treturn_val = 0;\n";
		*funcFile << "\tend\n";

		firstIf = false;
		/////////////////////

		for ( auto state : funcStateList )
		{
			Instruction *exitI = state->getExitInst();

			if ( ReturnInst *rInst = dyn_cast<ReturnInst>(exitI) ) {
				unsigned useTiming = state->getLatency() -1;
				if ( firstIf ) {
					firstIf = false;
					*funcFile << "\t";
				}
				else
					*funcFile << "\telse ";
				*funcFile << "if (1'b1 == is_State_" << state->getStateNumber();
				*funcFile << "_" << state->getLatency()-1 << ") begin\n";
				//XXX: Profiling
				if ( verilogConfigInfo->getBluetoothUse() ) {
					*funcFile << "\t\t$display(\"Function " << func->getName() << " End\");\n";
				}

				*funcFile << "\t\tfinish = ";
				if ( callTable->isCallBus() )
					*funcFile<<callTable->getFuncBitSize()<<"'d" << callTable->getFuncId(func) << ";\n";
				else
					*funcFile << "1;\n";
				
				if ( Value *returnV = rInst->getReturnValue() ) {
					*funcFile << "\t\treturn_val = ";
					printNameFromValue(returnV, func, state, useTiming, funcFile);
					*funcFile << ";\n";
				}
				else
					*funcFile << "\t\treturn_val = 0;\n";

				*funcFile << "\tend\n";
			}
		}

		*funcFile << "end\n\n";
	}

	void PrintVerilog::printFunctionShareableOperation(Function *func, raw_fd_ostream *funcFile) {
		for ( auto opTy : operatorTable->getUsedOperatorByFunction(func) )
		{
			unsigned opCode = opTy.first;
			unsigned bitWidth = opTy.second;
			bool firstIf = true;
			*funcFile << "always @(*) begin\n";
			*funcFile << "\tOP_" << opCode << "_" << bitWidth << "_arg_0 = 0;\n";
			*funcFile << "\tOP_" << opCode << "_" << bitWidth << "_arg_0_valid = 0;\n";
			*funcFile << "\tOP_" << opCode << "_" << bitWidth << "_arg_1 = 0;\n";
			*funcFile << "\tOP_" << opCode << "_" << bitWidth << "_arg_1_valid = 0;\n";

			for ( auto inst : operatorTable->getOperatorUserInst(func, opTy) )
			{
				State *state = fsm->getStateFromInst(inst);
				unsigned useTiming = state->getSchFromInst(inst);

				//If This State is pipeline related??
				//XXX: Pipeline State get private operators even if the operator is shareable.
				if ( state->isPipelineState() )
					continue;

				if ( firstIf ) {
					firstIf = false;
					*funcFile << "\t";
				}
				else
					*funcFile << "\telse ";
				*funcFile << "if(1'b1 == is_State_" << state->getStateNumber();
				*funcFile << "_" << useTiming << ") begin\n";
				*funcFile << "//" << *inst << "\n";

				*funcFile << "\t\tOP_" << opCode << "_" << bitWidth << "_arg_0 = ";
				printNameFromValue(inst->getOperand(0), func, state, useTiming, funcFile);
				*funcFile << ";\n";
				*funcFile << "\t\tOP_" << opCode << "_" << bitWidth << "_arg_0_valid = 1;\n";
				*funcFile << "\t\tOP_" << opCode << "_" << bitWidth << "_arg_1 = ";
				printNameFromValue(inst->getOperand(1), func, state, useTiming, funcFile);
				*funcFile << ";\n";
				*funcFile << "\t\tOP_" << opCode << "_" << bitWidth << "_arg_1_valid = 1;\n";

				*funcFile << "\tend\n";
			}
			*funcFile << "end\n\n";

			//assign
			for ( auto inst : operatorTable->getOperatorUserInst(func, opTy) )
			{
				*funcFile << "assign ";
				printNameOfInst(inst, func, funcFile);
				*funcFile << " = ";
				*funcFile << "OP_" << opCode << "_" << bitWidth << "_return;";
			}

			//AT
			for ( auto inst : operatorTable->getOperatorUserInst(func, opTy) )
			{
				State *state = fsm->getStateFromInst(inst);
				if ( state->isUsedAT(inst) && !state->isPipelineState() ) {
					OperationInfo opInfo = verilogConfigInfo->getOperationInfo(inst);
					//unsigned latency = operatorTable->getLatency(opTy);
					*funcFile << "always @(posedge clk) begin\n";
					*funcFile << "//" << *inst << "\n";
					*funcFile << "\tif(1'b1 == is_State_" << state->getStateNumber();
					*funcFile << "_" << state->getSchFromInst(inst) + opInfo.latency[0];
					if ( memoryTable->isByteAddressedRAM() )
						*funcFile << " && !memory_stall";
					*funcFile << ")\n\t\t";
					printNameOfInst(inst, func, funcFile);
					*funcFile << "_reg <= ";
//					*funcFile << "OP_" << opCode << "_" << bitWidth << "_return";
					printNameOfInst(inst, func, funcFile);
					*funcFile << ";\nend\n\n";
				}
			}
		}
	}

	void PrintVerilog::printFunctionMemoryController(Function *func, raw_fd_ostream *funcFile) {
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			unsigned ramId = ram->getRAMId();
			bool privateAccessed = 
				((memoryTable->getPrivateAccessInst(func, ram)).size() != 0);
			auto aSet = memoryTable->getAddressAccessRAMFromFunction(func);
			bool addressAccessed = ( aSet.find(ram) != aSet.end() );

			bool isDual = memoryTable->isDualPortRAM(ram);
			bool hasMC = ram->hasMultipleSize() || ram->hasExPort();

			if ( privateAccessed && addressAccessed ) {
				*funcFile << "assign RAM" << ramId << "_address_a = ";
				*funcFile << "RAM" << ramId << "_address_a_p | ";
				*funcFile << "RAM" << ramId << "_address_a_a;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_address_b = ";
				*funcFile << "RAM" << ramId << "_address_b_p | ";
				*funcFile << "RAM" << ramId << "_address_b_a;\n";
				}

				*funcFile << "assign RAM" << ramId << "_read_en_a = ";
				*funcFile << "RAM" << ramId << "_read_en_a_p | ";
				*funcFile << "RAM" << ramId << "_read_en_a_a;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_read_en_b = ";
				*funcFile << "RAM" << ramId << "_read_en_b_p | ";
				*funcFile << "RAM" << ramId << "_read_en_b_a;\n";
				}

				*funcFile << "assign RAM" << ramId << "_write_en_a = ";
				*funcFile << "RAM" << ramId << "_write_en_a_p | ";
				*funcFile << "RAM" << ramId << "_write_en_a_a;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_write_en_b = ";
				*funcFile << "RAM" << ramId << "_write_en_b_p | ";
				*funcFile << "RAM" << ramId << "_write_en_b_a;\n";
				}

				*funcFile << "assign RAM" << ramId << "_data_out_a = ";
				*funcFile << "RAM" << ramId << "_data_out_a_p | ";
				*funcFile << "RAM" << ramId << "_data_out_a_a;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_data_out_b = ";
				*funcFile << "RAM" << ramId << "_data_out_b_p | ";
				*funcFile << "RAM" << ramId << "_data_out_b_a;\n";
				}

//				if ( memoryTable->isByteAddressedRAM() ) {
				if ( hasMC ) {
					*funcFile << "assign RAM" << ramId << "_size_a = ";
					*funcFile << "RAM" << ramId << "_size_a_p | ";
					*funcFile << "RAM" << ramId << "_size_a_a;\n";
					if ( isDual ) {
					*funcFile << "assign RAM" << ramId << "_size_b = ";
					*funcFile << "RAM" << ramId << "_size_b_p | ";
					*funcFile << "RAM" << ramId << "_size_b_a;\n";
					}
				}
			}
			else if ( privateAccessed ) {
				*funcFile << "assign RAM" << ramId << "_address_a = ";
				*funcFile << "RAM" << ramId << "_address_a_p;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_address_b = ";
				*funcFile << "RAM" << ramId << "_address_b_p;\n";
				}

				*funcFile << "assign RAM" << ramId << "_read_en_a = ";
				*funcFile << "RAM" << ramId << "_read_en_a_p;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_read_en_b = ";
				*funcFile << "RAM" << ramId << "_read_en_b_p;\n";
				}

				*funcFile << "assign RAM" << ramId << "_write_en_a = ";
				*funcFile << "RAM" << ramId << "_write_en_a_p;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_write_en_b = ";
				*funcFile << "RAM" << ramId << "_write_en_b_p;\n";
				}

				*funcFile << "assign RAM" << ramId << "_data_out_a = ";
				*funcFile << "RAM" << ramId << "_data_out_a_p;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_data_out_b = ";
				*funcFile << "RAM" << ramId << "_data_out_b_p;\n";
				}

//				if ( memoryTable->isByteAddressedRAM() ) {
				if ( hasMC ) {
					*funcFile << "assign RAM" << ramId << "_size_a = ";
					*funcFile << "RAM" << ramId << "_size_a_p;\n";
					if ( isDual ) {
					*funcFile << "assign RAM" << ramId << "_size_b = ";
					*funcFile << "RAM" << ramId << "_size_b_p;\n";
					}
				}
			}
			else if ( addressAccessed ) {
				*funcFile << "assign RAM" << ramId << "_address_a = ";
				*funcFile << "RAM" << ramId << "_address_a_a;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_address_b = ";
				*funcFile << "RAM" << ramId << "_address_b_a;\n";
				}

				*funcFile << "assign RAM" << ramId << "_read_en_a = ";
				*funcFile << "RAM" << ramId << "_read_en_a_a;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_read_en_b = ";
				*funcFile << "RAM" << ramId << "_read_en_b_a;\n";
				}

				*funcFile << "assign RAM" << ramId << "_write_en_a = ";
				*funcFile << "RAM" << ramId << "_write_en_a_a;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_write_en_b = ";
				*funcFile << "RAM" << ramId << "_write_en_b_a;\n";
				}

				*funcFile << "assign RAM" << ramId << "_data_out_a = ";
				*funcFile << "RAM" << ramId << "_data_out_a_a;\n";
				if ( isDual ) {
				*funcFile << "assign RAM" << ramId << "_data_out_b = ";
				*funcFile << "RAM" << ramId << "_data_out_b_a;\n";
				}

//				if ( memoryTable->isByteAddressedRAM() ) {
				if ( hasMC ) {
					*funcFile << "assign RAM" << ramId << "_size_a = ";
					*funcFile << "RAM" << ramId << "_size_a_a;\n";
					if ( isDual ) {
					*funcFile << "assign RAM" << ramId << "_size_b = ";
					*funcFile << "RAM" << ramId << "_size_b_a;\n";
					}
				}
			}
			*funcFile << "\n";
		}

		//address -> RAM_address
		if ( (memoryTable->getAddressAccessRAMFromFunction(func)).size() != 0 ) {
			unsigned ramBitSize = memoryTable->getRAMBitSize();
			unsigned elementBitSize = memoryTable->getElementBitSize();
			unsigned addressSize = ramBitSize + elementBitSize;
			unsigned bitWidth = memoryTable->getBitWidth();

			bool isDual = false;
			for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
				isDual = isDual || memoryTable->isDualPortRAM(ram);

			*funcFile << "assign data_in_a = ";
			for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
				*funcFile << "RAM" << ram->getRAMId() << "_data_in_a | ";
			*funcFile << "0;\n";

			if ( isDual ) {
				*funcFile << "assign data_in_b = ";
				for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
					if ( memoryTable->isDualPortRAM(ram) )
						*funcFile << "RAM" << ram->getRAMId() << "_data_in_b | ";
				*funcFile << "0;\n";
				*funcFile << "\n";
			}

			bool firstIf = true;
			//A
			*funcFile << "always @(*) begin\n";
			for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
			{
				*funcFile << "\tRAM" << ram->getRAMId() << "_address_a_a = 0;\n";
				*funcFile << "\tRAM" << ram->getRAMId() << "_read_en_a_a = 0;\n";
				*funcFile << "\tRAM" << ram->getRAMId() << "_write_en_a_a = 0;\n";
				*funcFile << "\tRAM" << ram->getRAMId() << "_data_out_a_a = 0;\n";
//				if ( memoryTable->isByteAddressedRAM() )
				if ( ram->hasMultipleSize() || ram->hasExPort() )
					*funcFile << "\tRAM" << ram->getRAMId() << "_size_a_a = 0;\n";
			}
			*funcFile << "\n";
			for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
			{
				if ( firstIf ) {
					firstIf = false;
					*funcFile << "\t";
				}
				else
					*funcFile << "\telse ";
				*funcFile << "if ( address_a[" << addressSize-1 << ":" << addressSize-ramBitSize;
				*funcFile << "] == " << ramBitSize << "'d" << ram->getRAMId() << " ) begin\n";
				*funcFile << "\t\tRAM" << ram->getRAMId() << "_address_a_a = address_a";
				*funcFile << "[" << ram->getElementBitSize()-1 << ":0];\n";
				*funcFile << "\t\tRAM" << ram->getRAMId() << "_read_en_a_a = read_en_a;\n";
				*funcFile << "\t\tRAM" << ram->getRAMId() << "_write_en_a_a = write_en_a;\n";
				*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_out_a_a = data_out_a";
				*funcFile << "[" << ram->getDataBitSize()-1 << ":0];\n";
//				if ( memoryTable->isByteAddressedRAM() )
				if ( ram->hasMultipleSize() || ram->hasExPort() )
					*funcFile << "\tRAM" << ram->getRAMId() << "_size_a_a = size_a;\n";
				*funcFile << "end\n";
			}
			*funcFile << "end\n\n";

			if ( isDual ) {
				firstIf = true;
				//B
				*funcFile << "always @(*) begin\n";
				for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
					if ( memoryTable->isDualPortRAM(ram) )
					{
						*funcFile << "\tRAM" << ram->getRAMId() << "_address_b_a = 0;\n";
						*funcFile << "\tRAM" << ram->getRAMId() << "_read_en_b_a = 0;\n";
						*funcFile << "\tRAM" << ram->getRAMId() << "_write_en_b_a = 0;\n";
						*funcFile << "\tRAM" << ram->getRAMId() << "_data_out_b_a = 0;\n";
//						if ( memoryTable->isByteAddressedRAM() )
						if ( ram->hasMultipleSize() || ram->hasExPort() )
							*funcFile << "\tRAM" << ram->getRAMId() << "_size_b_a = 0;\n";
					}
				*funcFile << "\n";
				for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
					if ( memoryTable->isDualPortRAM(ram) )
					{
						if ( firstIf ) {
							firstIf = false;
							*funcFile << "\t";
						}
						else
							*funcFile << "\telse ";
						*funcFile << "if ( address_b[" << addressSize-1 << ":" << addressSize-ramBitSize;
						*funcFile << "] == " << ramBitSize << "'d" << ram->getRAMId() << " ) begin\n";
						*funcFile << "\t\tRAM" << ram->getRAMId() << "_address_b_a = address_b";
						*funcFile << "[" << ram->getElementBitSize()-1 << ":0];\n";
						*funcFile << "\t\tRAM" << ram->getRAMId() << "_read_en_b_a = read_en_b;\n";
						*funcFile << "\t\tRAM" << ram->getRAMId() << "_write_en_b_a = write_en_b;\n";
						*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_out_b_a = data_out_b";
						*funcFile << "[" << ram->getDataBitSize()-1 << ":0];\n";
//						if ( memoryTable->isByteAddressedRAM() )
						if ( ram->hasMultipleSize() || ram->hasExPort() )
							*funcFile << "\tRAM" << ram->getRAMId() << "_size_b_a = size_b;\n";
						*funcFile << "end\n";
					}
				*funcFile << "end\n\n";
			}
		}

		*funcFile << "\n";
	}

	void PrintVerilog::printFunctionPrivateAccess(Function *func, raw_fd_ostream *funcFile) {

		//For Memory A Port
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			list<Instruction *> privateAccessList = memoryTable->getPrivateAccessInst(func, ram);
			if ( privateAccessList.size() == 0 )
				continue;

			bool isDual = memoryTable->isDualPortRAM(ram);
			bool hasMC = ram->hasMultipleSize() || ram->hasExPort();

			bool firstIf = true;
			unsigned ramId = ram->getRAMId();
			*funcFile << "// RAM" << ramId << " Private Memory Access A Port\n";
			*funcFile << "always @(*) begin\n"; 
			*funcFile << "\tRAM" << ramId << "_address_a_p = 0;\n";
			*funcFile << "\tRAM" << ramId << "_read_en_a_p = 0;\n";
			*funcFile << "\tRAM" << ramId << "_write_en_a_p = 0;\n";
			*funcFile << "\tRAM" << ramId << "_data_out_a_p = 0;\n";
//			if ( memoryTable->isByteAddressedRAM() )
			if ( hasMC )
				*funcFile << "\tRAM" << ramId << "_size_a_p = 0;\n";
			for ( auto inst : privateAccessList )
			{
				State *state = fsm->getStateFromInst(inst);
				if ( state->getLoadPortNum(inst) ) {
					unsigned useTiming = state->getSchFromInst(inst);
					bool callExitTime = state->isCallExitTime(useTiming);

					if ( firstIf ) {
						firstIf = false;
						*funcFile << "\t";
					}
					else
						*funcFile << "\telse ";
					*funcFile << "if(1'b1 == is_State_" << state->getStateNumber();
					*funcFile << "_" << useTiming;
					if ( callExitTime )
						*funcFile << " && update_time"; // Load is Never in this timing
					*funcFile << ") begin\n";
					*funcFile << "//" << *inst << "\n";
					if ( LoadInst *lInst = dyn_cast<LoadInst>(inst) ) {
						*funcFile << "\t\tRAM" << ramId << "_address_a_p = ";
						printNameFromValue(lInst->getPointerOperand(), func, state, useTiming, funcFile);
						*funcFile << ";\n";
						*funcFile << "\t\tRAM" << ramId << "_read_en_a_p = 1;\n";

//						if ( memoryTable->isByteAddressedRAM() ) {
						if ( hasMC ) {
							Type *elementTy = 
								dyn_cast<PointerType>(lInst->getPointerOperand()->getType())->getElementType();
							*funcFile << "\t\tRAM" << ramId << "_size_a_p = " << getByteSize(elementTy);
							*funcFile << ";\n";
						}
					}
					else if ( StoreInst *sInst = dyn_cast<StoreInst>(inst) ) {
						*funcFile << "\t\tRAM" << ramId << "_address_a_p = ";
						printNameFromValue(sInst->getPointerOperand(), func, state, useTiming, funcFile);
						*funcFile << ";\n";
						*funcFile << "\t\tRAM" << ramId << "_read_en_a_p = 1;\n";
						*funcFile << "\t\tRAM" << ramId << "_write_en_a_p = 1;\n";
						*funcFile << "\t\tRAM" << ramId << "_data_out_a_p = ";
						if ( isSignedValue( sInst->getValueOperand() ) )
							*funcFile << "$signed(";
						printNameFromValue(sInst->getValueOperand(), func, state, useTiming, funcFile);
						if ( isSignedValue( sInst->getValueOperand() ) )
							*funcFile << ")";
						*funcFile << ";\n";

//						if ( memoryTable->isByteAddressedRAM() ) {
						if ( hasMC ) {
							Type *elementTy = sInst->getValueOperand()->getType();
							*funcFile << "\t\tRAM" << ramId << "_size_a_p = " << getByteSize(elementTy);
							*funcFile << ";\n";
						}
					}
					*funcFile << "\tend\n";
				}
			}
			if ( firstIf ) {
				*funcFile << "\tif ( reset ) begin\n";
				*funcFile << "\t\tRAM" << ramId << "_address_a_p = 0;\n";
				*funcFile << "\t\tRAM" << ramId << "_read_en_a_p = 0;\n";
				*funcFile << "\t\tRAM" << ramId << "_write_en_a_p = 0;\n";
				*funcFile << "\t\tRAM" << ramId << "_data_out_a_p = 0;\n";
//				if ( memoryTable->isByteAddressedRAM() )
				if ( hasMC )
					*funcFile << "\t\tRAM" << ramId << "_size_a_p = 0;\n";
				*funcFile << "\tend\n";
			}
			*funcFile << "end\n\n";

			//wire for Load
			for ( auto inst : privateAccessList )
			{
				State *state = fsm->getStateFromInst(inst);
				if ( state->getLoadPortNum(inst) ) {
					if ( isa<LoadInst>(inst) ) {
						unsigned endTiming = state->getSchFromInst(inst) + 1; //XXX: load latency = 1
						bool callExitTime = state->isCallExitTime(endTiming);

						*funcFile << "//" << *inst << "\n\t";
						*funcFile << "assign ";
						printNameOfInst(inst, func, funcFile);
						*funcFile << " = ";
						if ( callExitTime ) {
							*funcFile << "replace_time ? RAM" << ramId << "_data_in_a_call : ";
							*funcFile << "RAM" << ramId << "_data_in_a;\n";
						}
						else
							*funcFile << "RAM" << ramId << "_data_in_a;\n";
					}
				}
			}
		} // ram iteration end

		//For Memory B Port
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			list<Instruction *> privateAccessList = memoryTable->getPrivateAccessInst(func, ram);
			if ( privateAccessList.size() == 0 )
				continue;

			bool isDual = memoryTable->isDualPortRAM(ram);
			bool hasMC = ram->hasMultipleSize() || ram->hasExPort();

			if ( isDual ) {

			bool firstIf = true;
			unsigned ramId = ram->getRAMId();
			*funcFile << "// RAM" << ramId << " Private Memory Access B Port\n";
			*funcFile << "always @(*) begin\n"; 
			*funcFile << "\tRAM" << ramId << "_address_b_p = 0;\n";
			*funcFile << "\tRAM" << ramId << "_read_en_b_p = 0;\n";
			*funcFile << "\tRAM" << ramId << "_write_en_b_p = 0;\n";
			*funcFile << "\tRAM" << ramId << "_data_out_b_p = 0;\n\n";
//			if ( memoryTable->isByteAddressedRAM() )
			if ( hasMC )
				*funcFile << "\tRAM" << ramId << "_size_b_p = 0;\n";
			for ( auto inst : privateAccessList )
			{
				State *state = fsm->getStateFromInst(inst);
				if ( !state->getLoadPortNum(inst) ) {
					unsigned useTiming = state->getSchFromInst(inst);
					bool callExitTime = state->isCallExitTime(useTiming);

					if ( firstIf ) {
						firstIf = false;
						*funcFile << "\t";
					}
					else
						*funcFile << "\telse ";
					*funcFile << "if(1'b1 == is_State_" << state->getStateNumber();
					*funcFile << "_" << useTiming;
					if ( callExitTime )
						*funcFile << " && update_time";
					*funcFile << ") begin\n";
					*funcFile << "//" << *inst << "\n";
					if ( LoadInst *lInst = dyn_cast<LoadInst>(inst) ) {
						*funcFile << "\t\tRAM" << ramId << "_address_b_p = ";
						printNameFromValue(lInst->getPointerOperand(), func, state, useTiming, funcFile);
						*funcFile << ";\n";
						*funcFile << "\t\tRAM" << ramId << "_read_en_b_p = 1;\n";

//						if ( memoryTable->isByteAddressedRAM() ) {
						if ( hasMC ) {
							Type *elementTy = 
								dyn_cast<PointerType>(lInst->getPointerOperand()->getType())->getElementType();
							*funcFile << "\t\tRAM" << ramId << "_size_b_p = " << getByteSize(elementTy);
							*funcFile << ";\n";
						}
					}
					else if ( StoreInst *sInst = dyn_cast<StoreInst>(inst) ) {
						*funcFile << "\t\tRAM" << ramId << "_address_b_p = ";
						printNameFromValue(sInst->getPointerOperand(), func, state, useTiming, funcFile);
						*funcFile << ";\n";
						*funcFile << "\t\tRAM" << ramId << "_read_en_b_p = 1;\n";
						*funcFile << "\t\tRAM" << ramId << "_write_en_b_p = 1;\n";
						*funcFile << "\t\tRAM" << ramId << "_data_out_b_p = ";
						if ( isSignedValue( sInst->getValueOperand() ) )
							*funcFile << "$signed(";
						printNameFromValue(sInst->getValueOperand(), func, state, useTiming, funcFile);
						if ( isSignedValue( sInst->getValueOperand() ) )
							*funcFile << ")";
						*funcFile << ";\n";

//						if ( memoryTable->isByteAddressedRAM() ) {
						if ( hasMC ) {
							Type *elementTy = sInst->getValueOperand()->getType();
							*funcFile << "\t\tRAM" << ramId << "_size_b_p = " << getByteSize(elementTy);
							*funcFile << ";\n";
						}
					}
					*funcFile << "\tend\n";
				}
			}
			if ( firstIf ) {
				*funcFile << "\tif ( reset ) begin\n";
				*funcFile << "\t\tRAM" << ramId << "_address_b_p = 0;\n";
				*funcFile << "\t\tRAM" << ramId << "_read_en_b_p = 0;\n";
				*funcFile << "\t\tRAM" << ramId << "_write_en_b_p = 0;\n";
				*funcFile << "\t\tRAM" << ramId << "_data_out_b_p = 0;\n";
//				if ( memoryTable->isByteAddressedRAM() )
				if ( hasMC )
					*funcFile << "\t\tRAM" << ramId << "_size_b_p = 0;\n";
				*funcFile << "\tend\n";
			}
			*funcFile << "end\n\n";

			//wire for Load
			for ( auto inst : privateAccessList )
			{
				State *state = fsm->getStateFromInst(inst);
				if ( !state->getLoadPortNum(inst) ) {
					if ( isa<LoadInst>(inst) ) {
						unsigned endTiming = state->getSchFromInst(inst) + 1; //XXX: load latency = 1
						bool callExitTime = state->isCallExitTime(endTiming);

						*funcFile << "//" << *inst << "\n\t";
						*funcFile << "assign ";
						printNameOfInst(inst, func, funcFile);
						*funcFile << " = ";
						if ( callExitTime ) {
							*funcFile << "replace_time ? RAM" << ramId << "_data_in_b_call : ";
							*funcFile << "RAM" << ramId << "_data_in_b;\n";
						}
						else
							*funcFile << "RAM" << ramId << "_data_in_b;\n";
					}
				}
			}

			}//dual port end
		} // ram iteration end

	}

	void PrintVerilog::printFunctionAddressAccess(Function *func, raw_fd_ostream *funcFile) {
		if ( (memoryTable->getAddressAccessRAMFromFunction(func)).size() != 0 ) {

			bool isDual = false;
			for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
				isDual = isDual || memoryTable->isDualPortRAM(ram);

			bool firstIf = true;

			//For Memory A Port
			*funcFile << "// RAM Address Accessed Memory A Port\n";
			*funcFile << "always @(*) begin\n";
			*funcFile << "\taddress_a = 0;\n";
			*funcFile << "\tread_en_a = 0;\n";
			*funcFile << "\twrite_en_a = 0;\n";
			*funcFile << "\tdata_out_a = 0;\n";
			if ( memoryTable->isByteAddressedRAM() )
				*funcFile << "\tsize_a = 0;\n";
			for ( auto inst : memoryTable->getAddressAccessInst(func) )
			{
				State *state = fsm->getStateFromInst(inst);
				if ( state->getLoadPortNum(inst) ) {
					unsigned useTiming = state->getSchFromInst(inst);
					bool callExitTime = state->isCallExitTime(useTiming);

					if ( firstIf ) {
						firstIf = false;
						*funcFile << "\t";
					}
					else
						*funcFile << "\telse ";
					*funcFile << "if (1'b1 == is_State_" << state->getStateNumber();
					*funcFile << "_" << useTiming;
					if ( callExitTime )
						*funcFile << " && update_time";
					*funcFile << ") begin\n";
					*funcFile << "//" << *inst << "\n";
					if ( LoadInst *lInst = dyn_cast<LoadInst>(inst) ) {
						*funcFile << "\t\taddress_a = ";
						printNameFromValue(lInst->getPointerOperand(), func, state, useTiming, funcFile);
						*funcFile << ";\n";
						*funcFile << "\t\tread_en_a = 1;\n";

						if ( memoryTable->isByteAddressedRAM() ) {
							Type *elementTy = 
								dyn_cast<PointerType>(lInst->getPointerOperand()->getType())->getElementType();
							*funcFile << "\t\tsize_a = " << getByteSize(elementTy);
							*funcFile << ";\n";
						}
					}
					else if ( StoreInst *sInst = dyn_cast<StoreInst>(inst) ) {
						*funcFile << "\t\taddress_a = ";
						printNameFromValue(sInst->getPointerOperand(), func, state, useTiming, funcFile);
						*funcFile << ";\n";
						*funcFile << "\t\tread_en_a = 1;\n";
						*funcFile << "\t\twrite_en_a = 1;\n";
						*funcFile << "\t\tdata_out_a = ";
						if ( isSignedValue( sInst->getValueOperand() ) )
							*funcFile << "$signed(";
						printNameFromValue(sInst->getValueOperand(), func, state, useTiming, funcFile);
						if ( isSignedValue( sInst->getValueOperand() ) )
							*funcFile << ")";
						*funcFile << ";\n";

						if ( memoryTable->isByteAddressedRAM() ) {
							Type *elementTy = sInst->getValueOperand()->getType();
							*funcFile << "\t\tsize_a = " << getByteSize(elementTy);
							*funcFile << ";\n";
						}
					}
					*funcFile << "\tend\n";
				}
			}
			if ( firstIf ) {
				*funcFile << "\tif (reset) begin\n";
				*funcFile << "\t\taddress_a = 0;\n";
				*funcFile << "\t\tread_en_a = 0;\n";
				*funcFile << "\t\twrite_en_a = 0;\n";
				*funcFile << "\t\tdata_out_a = 0;\n";
				if ( memoryTable->isByteAddressedRAM() )
					*funcFile << "\t\tsize_a = 0;\n";
				*funcFile << "\tend\n";
			}
			*funcFile << "end\n\n";

			//wire for Load
			for ( auto inst : memoryTable->getAddressAccessInst(func) )
			{
				State *state = fsm->getStateFromInst(inst);
				if ( state->getLoadPortNum(inst) ) {
					if ( isa<LoadInst>(inst) ) {
						unsigned endTiming = state->getSchFromInst(inst) + 1; //XXX: load latency = 1
						bool callExitTime = state->isCallExitTime(endTiming);

						*funcFile << "//" << *inst << "\n";
						*funcFile << "assign ";
						printNameOfInst(inst, func, funcFile);
						*funcFile << " = ";
						if ( callExitTime )
							*funcFile << "replace_time ? data_in_a_call : data_in_a;\n";
						else
							*funcFile << "data_in_a;\n";
					}
				}
			}

			if ( isDual ) {

			firstIf = true;
			//For Memory B Port
			*funcFile << "// RAM Address Accessed Memory B Port\n";
			*funcFile << "always @(*) begin\n";
			*funcFile << "\taddress_b = 0;\n";
			*funcFile << "\tread_en_b = 0;\n";
			*funcFile << "\twrite_en_b = 0;\n";
			*funcFile << "\tdata_out_b = 0;\n\n";
			if ( memoryTable->isByteAddressedRAM() )
				*funcFile << "\tsize_b = 0;\n";
			for ( auto inst : memoryTable->getAddressAccessInst(func) )
			{
				State *state = fsm->getStateFromInst(inst);
				if ( !state->getLoadPortNum(inst) ) {
					unsigned useTiming = state->getSchFromInst(inst);
					bool callExitTime = state->isCallExitTime(useTiming);

					if ( firstIf ) {
						firstIf = false;
						*funcFile << "\t";
					}
					else
						*funcFile << "\telse ";
					*funcFile << "if (1'b1 == is_State_" << state->getStateNumber();
					*funcFile << "_" << useTiming;
					if ( callExitTime )
						*funcFile << " && update_time";
					*funcFile << ") begin\n";
					*funcFile << "//" << *inst << "\n";
					if ( LoadInst *lInst = dyn_cast<LoadInst>(inst) ) {
						*funcFile << "\t\taddress_b = ";
						printNameFromValue(lInst->getPointerOperand(), func, state, useTiming, funcFile);
						*funcFile << ";\n";
						*funcFile << "\t\tread_en_b = 1;\n";

						if ( memoryTable->isByteAddressedRAM() ) {
							Type *elementTy = 
								dyn_cast<PointerType>(lInst->getPointerOperand()->getType())->getElementType();
							*funcFile << "\t\tsize_b = " << getByteSize(elementTy);
							*funcFile << ";\n";
						}
					}
					else if ( StoreInst *sInst = dyn_cast<StoreInst>(inst) ) {
						*funcFile << "\t\taddress_b = ";
						printNameFromValue(sInst->getPointerOperand(), func, state, useTiming, funcFile);
						*funcFile << ";\n";
						*funcFile << "\t\tread_en_b = 1;\n";
						*funcFile << "\t\twrite_en_b = 1;\n";
						*funcFile << "\t\tdata_out_b = ";
						if ( isSignedValue( sInst->getValueOperand() ) )
							*funcFile << "$signed(";
						printNameFromValue(sInst->getValueOperand(), func, state, useTiming, funcFile);
						if ( isSignedValue( sInst->getValueOperand() ) )
							*funcFile << ")";
						*funcFile << ";\n";

						if ( memoryTable->isByteAddressedRAM() ) {
							Type *elementTy = sInst->getValueOperand()->getType();
							*funcFile << "\t\tsize_b = " << getByteSize(elementTy);
							*funcFile << ";\n";
						}
					}
					*funcFile << "\tend\n";
				}
			}
			if ( firstIf ) {
				*funcFile << "\tif ( reset ) begin\n";
				*funcFile << "\t\taddress_b = 0;\n";
				*funcFile << "\t\tread_en_b = 0;\n";
				*funcFile << "\t\twrite_en_b = 0;\n";
				*funcFile << "\t\tdata_out_b = 0;\n";
				if ( memoryTable->isByteAddressedRAM() )
					*funcFile << "\t\tsize_b = 0;\n";
				*funcFile << "\tend\n";
			}
			*funcFile << "end\n\n";

			//wire for Load
			for ( auto inst : memoryTable->getAddressAccessInst(func) )
			{
				State *state = fsm->getStateFromInst(inst);
				if ( !state->getLoadPortNum(inst) ) {
					if ( isa<LoadInst>(inst) ) {
						unsigned endTiming = state->getSchFromInst(inst) + 1; //XXX: load latency = 1
						bool callExitTime = state->isCallExitTime(endTiming);

						*funcFile << "//" << *inst << "\n";
						*funcFile << "assign ";
						printNameOfInst(inst, func, funcFile);
						*funcFile << " = ";
						if ( callExitTime )
							*funcFile << "replace_time ? data_in_b_call : data_in_b;\n";
						else
							*funcFile << "data_in_b;\n";
					}
				}
			}

			}//isDual End

		}// If a function has Address Accesses
	}

	void PrintVerilog::printFunctionUnresolvedAccess(Function *func, raw_fd_ostream *funcFile) {
		//TODO: Merge to Address Access...	
		if ( (memoryTable->getUnresolvedInst(func)).size() != 0 )
			assert(0 && "Can not Handle Unresolved Memory Instruction");
	}


	void PrintVerilog::functionModuleDefine(Function *func) {
		StringRef fileName = func->getName();
		SmallString<256> nameBuf;
		nameBuf.clear();
		fileName = Twine( Twine(fileName) + Twine(".v") ).toStringRef(nameBuf);

		std::error_code ec_print = std::make_error_code(std::errc::io_error);
		raw_fd_ostream funcFile(fileName.str(), ec_print, llvm::sys::fs::F_None);

		errs() << "\t" << func->getName() << " Function Module Declaration\n";
		printFunctionDeclaration(func, &funcFile);

		errs() << "\t" << func->getName() << " Function Register\n";
		printFunctionRegister(func, &funcFile);

		errs() << "\t" << func->getName() << " Function State Parameter\n";
		printFunctionStateParameter(func, &funcFile);

		errs() << "\t" << func->getName() << " Function State Transition\n";
		printFunctionStateTransition(func, &funcFile);

		errs() << "\t" << func->getName() << " Function Normal Operation\n";
		printFunctionNormalOperation(func, &funcFile);

		errs() << "\t" << func->getName() << " Function Shareable Operation\n";
		printFunctionShareableOperation(func, &funcFile);

		errs() << "\t" << func->getName() << " Function Call Instruction\n";
		printFunctionCallInstruction(func, &funcFile);

		errs() << "\t" << func->getName() << " Function Memory Controller\n";
		printFunctionMemoryController(func, &funcFile);

		errs() << "\t" << func->getName() << " Function Private Access\n";
		printFunctionPrivateAccess(func, &funcFile);

		errs() << "\t" << func->getName() << " Function Address Access\n";
		printFunctionAddressAccess(func, &funcFile);
 
		errs() << "\t" << func->getName() << " Function Unresolved Access\n";
		printFunctionUnresolvedAccess(func, &funcFile);

		funcFile << "\nendmodule\n\n";

		funcFile.close();
	}

	//Define each function module
	void PrintVerilog::functionModuleGeneration(void) {
			for ( auto func : targetFuncSet )
			{
				if ( memoryTable->isByteAddressedRAM() && isMemFunction(func) )
					functionModuleDefineForMem(func);
				else
					functionModuleDefine(func);
			}
	}


	void PrintVerilog::operatorModuleGeneration(void) {
		if ( (operatorTable->getUsedOperator()).size() != 0 ) {
			std::error_code ec_print = std::make_error_code(std::errc::io_error);
			raw_fd_ostream out("operatorModule.v", ec_print, llvm::sys::fs::F_None);

			for ( auto opTy : targetOPSet )
			{
				unsigned opCode = opTy.first;
				unsigned bitWidth = opTy.second;

				if ( opCode == Instruction::Mul ) {
					out << "module OP_" << opCode << "_" << bitWidth << " (\n";
					out << "input [" << bitWidth-1 << ":0] arg_0,\n";
					out << "input [" << bitWidth-1 << ":0] arg_1,\n";
					out << "output [" << bitWidth-1 << ":0] return\n);\n";
					out << "\tassign return = arg_0 * arg_1;\n";
					out << "endmodule\n";
				}
				else if ( opCode == Instruction::UDiv ) {
					out << "module OP_" << opCode << "_" << bitWidth << " (\n";
					out << "input [" << bitWidth-1 << ":0] arg_0,\n";
					out << "input [" << bitWidth-1 << ":0] arg_1,\n";
					out << "output [" << bitWidth-1 << ":0] return\n);\n";
					out << "\tassign return = arg_0 / arg_1;\n";
					out << "endmodule\n";
				}
				else if ( opCode == Instruction::SDiv ) {
					out << "module OP_" << opCode << "_" << bitWidth << " (\n";
					out << "input [" << bitWidth-1 << ":0] arg_0,\n";
					out << "input [" << bitWidth-1 << ":0] arg_1,\n";
					out << "output [" << bitWidth-1 << ":0] return\n);\n";
					out << "\tassign return = arg_0 / arg_1;\n";
					out << "endmodule\n";
				}
				else if ( opCode == Instruction::URem ) {
					out << "module OP_" << opCode << "_" << bitWidth << " (\n";
					out << "input [" << bitWidth-1 << ":0] arg_0,\n";
					out << "input [" << bitWidth-1 << ":0] arg_1,\n";
					out << "output [" << bitWidth-1 << ":0] return\n);\n";
					out << "\tassign return = arg_0 % arg_1;\n";
					out << "endmodule\n";
				}
				else if ( opCode == Instruction::SRem ) {
					out << "module OP_" << opCode << "_" << bitWidth << " (\n";
					out << "input [" << bitWidth-1 << ":0] arg_0,\n";
					out << "input [" << bitWidth-1 << ":0] arg_1,\n";
					out << "output [" << bitWidth-1 << ":0] return\n);\n";
					out << "\tassign return = arg_0 % arg_1;\n";
					out << "endmodule\n";
				}
				out << "\n\n";
			}

			out.close();
		}
	}

	//Print Function ID ( only for Call Bus System )
	//Print Memory ID
	void PrintVerilog::printDefinedParameter(raw_fd_ostream *topFile) {
		*topFile << "`timescale 1ns / 1ps\n\n";

		if ( callTable->isCallBus() ) {
			*topFile << "//\tFunction ID for Call Bus System\n";
			for ( auto func : targetFuncSet )
				*topFile << "//\t\t" << func->getName() << " : " << callTable->getFuncId(func) << "\n";
			*topFile << "\n";
		}

		for ( auto ram : memoryTable->getAllRAMList() )
		{
				*topFile << "`define RAM" << ram->getRAMId() << "_Address {";
				*topFile << memoryTable->getRAMBitSize() << "'d" << ram->getRAMId() << ", ";
				*topFile << memoryTable->getElementBitSize() << "'d0}\n";
		}
		*topFile << "\n";
	}


	//TODO : valid memory -> memory controller generation
	void PrintVerilog::printInstantiation(raw_fd_ostream *topFile) {
		//Memory Instantiation
		for ( auto ram : targetRAMSet )
			printRAMInstance(ram, topFile);

		//Operator Instantiation
		for ( auto opTy : targetOPSet)
		{
			unsigned opCode = opTy.first;
			unsigned bitWidth = opTy.second;

			*topFile << "wire arg_0_valid_OP_" << opCode << "_" << bitWidth << ";\n";
			*topFile << "wire arg_0_ready_OP_" << opCode << "_" << bitWidth << ";\n";
			*topFile << "wire [" << bitWidth-1 << ":0] arg_0_";
			*topFile << "OP_" << opCode << "_" << bitWidth << ";\n";
			*topFile << "wire arg_1_valid_OP_" << opCode << "_" << bitWidth << ";\n";
			*topFile << "wire arg_1_ready_OP_" << opCode << "_" << bitWidth << ";\n";
			*topFile << "wire [" << bitWidth-1 << ":0] arg_1_";
			*topFile << "OP_" << opCode << "_" << bitWidth << ";\n";
			*topFile << "wire return_valid_OP_" << opCode << "_" << bitWidth << ";\n";
			*topFile << "wire [" << bitWidth-1 << ":0] return_";
			*topFile << "OP_" << opCode << "_" << bitWidth << ";\n";

			if ( opCode == Instruction::FAdd && bitWidth == 32 )
				*topFile << "floating_point_add_32 ";
			else if ( opCode == Instruction::FAdd && bitWidth == 64 )
				*topFile << "floating_point_add_64 ";
			else if ( opCode == Instruction::FSub&& bitWidth == 32 )
				*topFile << "floating_point_sub_32 ";
			else if ( opCode == Instruction::FSub && bitWidth == 64 )
				*topFile << "floating_point_sub_64 ";
			else if ( opCode == Instruction::FMul && bitWidth == 32 )
				*topFile << "floating_point_mul_32 ";
			else if ( opCode == Instruction::FMul && bitWidth == 64 )
				*topFile << "floating_point_mul_64 ";
			else if ( opCode == Instruction::FDiv && bitWidth == 32 )
				*topFile << "floating_point_div_32 ";
			else if ( opCode == Instruction::FDiv && bitWidth == 64 )
				*topFile << "floating_point_div_64 ";
			else
				assert(0);

			*topFile << "OP_" << opCode << "_" << bitWidth << "_instance (\n";
			*topFile << ".aclk(clk),\n";
			*topFile << ".s_axis_a_tvalid(arg_0_valid_OP_" << opCode << "_" << bitWidth << "),\n";
			*topFile << ".s_axis_a_tready(arg_0_ready_OP_" << opCode << "_" << bitWidth << "),\n";
			*topFile << ".s_axis_a_tdata(arg_0_OP_" << opCode << "_" << bitWidth << "),\n";
			*topFile << ".s_axis_b_tvalid(arg_1_valid_OP_" << opCode << "_" << bitWidth << "),\n";
			*topFile << ".s_axis_b_tready(arg_1_ready_OP_" << opCode << "_" << bitWidth << "),\n";
			*topFile << ".s_axis_b_tdata(arg_1_OP_" << opCode << "_" << bitWidth << "),\n";
			*topFile << ".m_axis_result_tvalid(return_valid_OP_"<<opCode<<"_"<<bitWidth << "),\n";
			*topFile << ".m_axis_result_tdata(return_OP_"<<opCode<<"_"<<bitWidth << "),\n";
			*topFile << ");\n\n";
		}

		//Function Instantiation

		if ( callTable->isCallBus() ) {
			for ( unsigned i = 0; i < callTable->getMaxArgNum(); ++i )
				*topFile << "wire ["<<callTable->getArgBitWidth(i)-1<<":0] arg_"<<i<<"_function;\n";
			*topFile << "wire [" << callTable->getFuncBitSize()-1<< ":0] start_function";
			*topFile << "wire [" << callTable->getFuncBitSize()-1<< ":0] receive_finish_function";
			*topFile<<"wire ["<<callTable->getMaxReturnBitWidth()-1<<":0] receive_return_val_function";
		}

		for ( auto func : targetFuncSet )
		{
			//Wire Registeration
			//MemoryAccess
			for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			{
				bool isDual = memoryTable->isDualPortRAM(ram);
				bool hasMC = ram->hasMultipleSize() || ram->hasExPort();

				*topFile << "wire [" << ram->getElementBitSize()-1 << ":0] ";
				*topFile << "RAM" << ram->getRAMId() << "_address_a_" << func->getName() << ";\n";
				*topFile << "wire RAM" << ram->getRAMId() << "_read_en_a_" << func->getName() << ";\n";
				*topFile << "wire RAM" << ram->getRAMId() << "_write_en_a_" << func->getName() << ";\n";
				*topFile << "wire [" << ram->getDataBitSize()-1 << ":0] ";
				*topFile << "RAM" << ram->getRAMId() << "_data_out_a_" << func->getName() << ";\n";
//				if ( memoryTable->isByteAddressedRAM() ) {
				if ( hasMC ) {
					*topFile << "wire [3:0] RAM" << ram->getRAMId() << "_size_a_";
					*topFile << func->getName() << ";\n";
				}
				if ( isUsedInMemFunc(ram) && isMemFunction(func) ) {
					*topFile << "wire [" << ram->getElementBitSize()-1 << ":0] ";
					*topFile << "RAM" << ram->getRAMId() << "_address_a_ex_" << func->getName() << ";\n";
					*topFile << "wire RAM"<<ram->getRAMId()<<"_select_en_a_ex_"<<func->getName()<<";\n";
					*topFile << "wire RAM"<<ram->getRAMId()<<"_write_en_a_ex_"<<func->getName()<<";\n";
					*topFile << "wire [" << memoryTable->getMemBitWidth()-1 << ":0] ";
					*topFile << "RAM" << ram->getRAMId()<<"_data_out_a_ex_"<<func->getName() << ";\n\n";
				}

				if ( isDual ) {
					*topFile << "wire [" << ram->getElementBitSize()-1 << ":0] ";
					*topFile << "RAM" << ram->getRAMId() << "_address_b_" << func->getName() << ";\n";
					*topFile << "wire RAM" << ram->getRAMId() << "_read_en_b_" <<func->getName()<< ";\n";
					*topFile << "wire RAM" << ram->getRAMId() << "_write_en_b_" <<func->getName()<< ";\n";
					*topFile << "wire [" << ram->getDataBitSize()-1 << ":0] ";
					*topFile << "RAM" << ram->getRAMId() << "_data_out_b_" <<func->getName()<<";\n\n";
//					if ( memoryTable->isByteAddressedRAM() ) {
					if ( hasMC ) {
						*topFile << "wire [3:0] RAM" << ram->getRAMId() << "_size_b_";
						*topFile << func->getName() << ";\n\n";
					}
					if ( isUsedInMemFunc(ram) && isMemFunction(func) ) {
						*topFile << "wire [" << ram->getElementBitSize()-1 << ":0] ";
						*topFile << "RAM" << ram->getRAMId() << "_address_b_ex_" << func->getName()<<";\n";
						*topFile << "wire RAM"<<ram->getRAMId()<<"_select_en_b_ex_"<<func->getName()<<";\n";
						*topFile << "wire RAM"<<ram->getRAMId()<<"_write_en_b_ex_"<<func->getName()<<";\n";
						*topFile << "wire [" << memoryTable->getMemBitWidth()-1 << ":0] ";
						*topFile << "RAM" << ram->getRAMId()<<"_data_out_b_ex_"<<func->getName()<<";\n\n";
					}
				}
			}

			//Operator Access
			for ( auto opType : operatorTable->getUsedOperatorByFunction(func) )
			{
				unsigned opCode = opType.first;
				unsigned bitWidth = opType.second;
				*topFile << "wire [" << bitWidth -1 << ":0] OP_" << opCode << "_";
				*topFile << bitWidth << "_arg_0_" << func->getName() << ";\n";
				*topFile << "wire OP_" << opCode << "_" << bitWidth << "_arg_0_valid_";
				*topFile << func->getName() << ";\n";
				*topFile << "wire [" << bitWidth -1 << ":0] OP_" << opCode << "_";
				*topFile << bitWidth << "_arg_1_" << func->getName() << ";\n";
				*topFile << "wire OP_" << opCode << "_" << bitWidth << "_arg_1_valid_";
				*topFile << func->getName() << ";\n";
			}

			//Call Related
			if ( callTable->isCallBus() && (callTable->getCalleeFromFunction(func)).size() != 0 ) {
				for ( unsigned i = 0; i < callTable->getMaxArgNum(); ++i ) {
					*topFile << "wire [" << callTable->getArgBitWidth(i)-1 << ":0] send_arg_" << i;
					*topFile << "_" << func->getName() << ";\n";
				}
				*topFile << "wire [" << callTable->getFuncBitSize() -1 << ":0] send_start";
				*topFile << "_" << func->getName() << ";\n";
				*topFile << "wire [" << callTable->getFuncBitSize() -1 << ":0] finish";
				*topFile << "_" << func->getName() << ";\n";
				*topFile << "wire [" << callTable->getMaxReturnBitWidth() -1<< ":0] return_val";
				*topFile << "_" << func->getName() << ";\n";

				if ( verilogConfigInfo->getValidationMode() )
					assert(0&&"not implemented yet\n");
			}
			else {
				for ( auto callee : callTable->getCalleeFromFunction(func) )
				{
					for ( unsigned arg = 0; arg < callee->arg_size(); ++arg ) {
						*topFile << "wire [" << callTable->getFuncArgBit(callee, arg)-1<< ":0] ";
						*topFile << callee->getName() << "_arg_" << arg << "_" << func->getName() << ";\n";
					}
					*topFile << "wire " << callee->getName() << "_start_" << func->getName() << ";\n";
				}

				for ( unsigned arg = 0; arg < func->arg_size(); ++arg ) {
					*topFile << "wire [" << callTable->getFuncArgBit(func, arg) -1;
					*topFile << ":0] arg_" << arg << "_" << func->getName() << ";\n";
				}
				
				*topFile << "wire start_" << func->getName() << ";\n";

				if ( verilogConfigInfo->getValidationMode() ) {
					*topFile << "reg finish_" << func->getName() << "_reg;\n";
					*topFile << "reg ["<<callTable->getFuncReturnBit(func)-1<<":0] return_val_";
					*topFile << func->getName() << "_reg;\n\n";
				}
				*topFile << "wire finish_" << func->getName() << ";\n";
				*topFile << "wire ["<<callTable->getFuncReturnBit(func)-1<<":0] return_val_";
				*topFile << func->getName() << ";\n\n";

			}
		}

		//Module Instantiation
		for ( auto func : targetFuncSet )
		{

		if ( verilogConfigInfo->getPrivateBuffer() )
			*topFile << "(* keep_hierarchy = \"yes\" *)";

			//Module Instantiation
			*topFile << func->getName() << " " << func->getName() << "_instance (\n";
			//Memory Access
			for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			{
				bool isDual = memoryTable->isDualPortRAM(ram);
				bool hasMC = ram->hasMultipleSize() || ram->hasExPort();

				*topFile << ".RAM" << ram->getRAMId() << "_address_a(";
				*topFile << "RAM" << ram->getRAMId() << "_address_a_" << func->getName() << "),\n";
				*topFile << ".RAM" << ram->getRAMId() << "_read_en_a(";
				*topFile << "RAM" << ram->getRAMId() << "_read_en_a_" << func->getName() << "),\n";
				*topFile << ".RAM" << ram->getRAMId() << "_write_en_a(";
				*topFile << "RAM" << ram->getRAMId() << "_write_en_a_" << func->getName() << "),\n";
				*topFile << ".RAM" << ram->getRAMId() << "_data_in_a_input(";
				*topFile << "data_out_a_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".RAM" << ram->getRAMId() << "_data_out_a(";
				*topFile << "RAM" << ram->getRAMId() << "_data_out_a_" << func->getName() << "),\n";
//				if ( memoryTable->isByteAddressedRAM() ) {
				if ( hasMC ) {
					*topFile << ".RAM" << ram->getRAMId() << "_memory_valid_a(";
					*topFile << "valid_a_RAM" << ram->getRAMId() << "),\n";
					*topFile << ".RAM" << ram->getRAMId() << "_size_a(";
					*topFile << "RAM" << ram->getRAMId() << "_size_a_"<<func->getName()<<"),\n";
				}
				if ( isUsedInMemFunc(ram) && isMemFunction(func) ) {
					*topFile << ".RAM" << ram->getRAMId() << "_address_a_ex(";
					*topFile << "RAM" << ram->getRAMId() << "_address_a_ex_" << func->getName() << "),\n";
					*topFile << ".RAM" << ram->getRAMId() << "_select_en_a_ex(";
					*topFile << "RAM" << ram->getRAMId() << "_select_en_a_ex_"<<func->getName() << "),\n";
					*topFile << ".RAM" << ram->getRAMId() << "_write_en_a_ex(";
					*topFile << "RAM" << ram->getRAMId() <<"_write_en_a_ex_"<<func->getName()<<"),\n";
					*topFile << ".RAM" << ram->getRAMId() << "_data_in_a_ex(";
					*topFile << "data_out_a_ex_RAM" << ram->getRAMId() << "),\n";
					*topFile << ".RAM" << ram->getRAMId() << "_data_out_a_ex(";
					*topFile << "RAM" << ram->getRAMId() <<"_data_out_a_ex_"<<func->getName()<<"),\n";
				}

				if ( isDual ) {
				*topFile << ".RAM" << ram->getRAMId() << "_address_b(";
				*topFile << "RAM" << ram->getRAMId() << "_address_b_" << func->getName() << "),\n";
				*topFile << ".RAM" << ram->getRAMId() << "_read_en_b(";
				*topFile << "RAM" << ram->getRAMId() << "_read_en_b_" << func->getName() << "),\n";
				*topFile << ".RAM" << ram->getRAMId() << "_write_en_b(";
				*topFile << "RAM" << ram->getRAMId() << "_write_en_b_" << func->getName() << "),\n";
				*topFile << ".RAM" << ram->getRAMId() << "_data_in_b_input(";
				*topFile << "data_out_b_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".RAM" << ram->getRAMId() << "_data_out_b(";
				*topFile << "RAM" << ram->getRAMId() << "_data_out_b_" << func->getName() << "),\n";
//				if ( memoryTable->isByteAddressedRAM() ) {
				if ( hasMC ) {
					*topFile << ".RAM" << ram->getRAMId() << "_memory_valid_b(";
					*topFile << "valid_b_RAM" << ram->getRAMId() << "),\n";
					*topFile << ".RAM" << ram->getRAMId() << "_size_b(";
					*topFile << "RAM" << ram->getRAMId() << "_size_b_"<<func->getName()<<"),\n";
				}
				if ( isUsedInMemFunc(ram) && isMemFunction(func) ) {
					*topFile << ".RAM" << ram->getRAMId() << "_address_b_ex(";
					*topFile << "RAM" << ram->getRAMId() << "_address_b_ex_" << func->getName() << "),\n";
					*topFile << ".RAM" << ram->getRAMId() << "_select_en_b_ex(";
					*topFile << "RAM" << ram->getRAMId() << "_select_en_b_ex_"<<func->getName() << "),\n";
					*topFile << ".RAM" << ram->getRAMId() << "_write_en_b_ex(";
					*topFile << "RAM" << ram->getRAMId() <<"_write_en_b_ex_"<<func->getName()<<"),\n";
					*topFile << ".RAM" << ram->getRAMId() << "_data_in_b_ex(";
					*topFile << "data_out_b_ex_RAM" << ram->getRAMId() << "),\n";
					*topFile << ".RAM" << ram->getRAMId() << "_data_out_b_ex(";
					*topFile << "RAM" << ram->getRAMId() <<"_data_out_b_ex_"<<func->getName()<<"),\n";
				}
				}
			}
			//Operator Access
			for ( auto opType : operatorTable->getUsedOperatorByFunction(func) )
			{
				unsigned opCode = opType.first;
				unsigned bitWidth = opType.second;
				*topFile << ".OP_" << opCode << "_" << bitWidth << "_arg_0(";
				*topFile << "OP_" << opCode << "_" << bitWidth <<"_arg_0_"<< func->getName() << "),\n";
				*topFile << ".OP_" << opCode << "_" << bitWidth << "_arg_0_valid(";
				*topFile << "OP_" << opCode << "_" << bitWidth <<"_arg_0_valid_";
				*topFile << func->getName() << "),\n";
				*topFile << ".OP_" << opCode << "_" << bitWidth << "_arg_1(";
				*topFile << "OP_" << opCode << "_" << bitWidth <<"_arg_1_"<< func->getName() << "),\n";
				*topFile << ".OP_" << opCode << "_" << bitWidth << "_arg_1_valid(";
				*topFile << "OP_" << opCode << "_" << bitWidth <<"_arg_1_valid_";
				*topFile << func->getName() << "),\n";
				*topFile << ".OP_" << opCode << "_" << bitWidth << "_return(";
				*topFile << "return_OP_"<< opCode << "_" << bitWidth << "),\n";
			}
			//Call Related
			if ( callTable->isCallBus() && (callTable->getCalleeFromFunction(func)).size() != 0 ) {
				for ( unsigned i = 0; i < callTable->getMaxArgNum(); ++i )
					*topFile << ".arg_" << i << "(arg_" << i << "_function),\n";
				for ( unsigned i = 0; i < callTable->getMaxArgNum(); ++i )
					*topFile << ".send_arg_"<<i<<"(send_arg_" << i << "_" << func->getName() << "),\n";
				*topFile << ".start(start_function),\n";
				*topFile << ".send_start(send_start_" << func->getName() << "),\n";
				*topFile << ".finish(finish_" << func->getName() << "),\n";
				*topFile << ".receive_finish(receive_finish_function),\n";
				*topFile << ".return_val(return_val_" << func->getName() << "),\n";
				*topFile << ".receive_return_val(receive_return_val_function),\n";
			}
			else { // No Call Bus
				for ( auto callee : callTable->getCalleeFromFunction(func) )
				{
					for ( unsigned arg = 0; arg < callee->arg_size(); ++arg ) {
						*topFile << "." << callee->getName() << "_arg_" << arg << "(";
						*topFile << callee->getName() << "_arg_" << arg << "_" << func->getName() << "),\n";
					}
					*topFile << "." << callee->getName() << "_start(";
					*topFile << callee->getName() << "_start_" << func->getName() << "),\n";
					if ( verilogConfigInfo->getValidationMode() ) {
						*topFile << "." << callee->getName() << "_finish(";
						*topFile << "finish_" << callee->getName() << "_reg),\n";
						*topFile << "." << callee->getName() << "_return_val(";
						*topFile << "return_val_" << callee->getName() << "_reg),\n";
					}
					else {
						*topFile << "." << callee->getName() << "_finish(";
						*topFile << "finish_" << callee->getName() << "),\n";
						*topFile << "." << callee->getName() << "_return_val(";
						*topFile << "return_val_" << callee->getName() << "),\n";
					}
				}

				for ( unsigned arg = 0; arg < func->arg_size(); ++arg ) 
					*topFile << ".arg_" << arg << "(arg_" << arg << "_" << func->getName() << "),\n";
				*topFile << ".start(start_" << func->getName() << "),\n";

				*topFile << ".finish(finish_" << func->getName() << "),\n";
				*topFile << ".return_val(return_val_" << func->getName() << "),\n";
			}

			*topFile << ".clk(clk),\n";
			*topFile << ".reset(reset)\n";

			*topFile << ");\n\n";
		} //Done : Function Instantiation

	}

	//TODO: valid memory -> memory controller connection 
	void PrintVerilog::printConnection(raw_fd_ostream *topFile) {

		if ( verilogConfigInfo->getValidationMode() ) {
			*topFile << "reg [31:0] print_counter;\n";
			for ( auto func : targetFuncSet )
				*topFile << "reg finish_" << func->getName() << "_tmp;\n";

			*topFile << "always @(posedge clk) begin\n";
			*topFile << "\tif (reset) begin\n";

			*topFile << "\t\tprint_counter <= 0;\n";
			for ( auto ram : targetRAMSet )
				*topFile << "\t\tRAM" << ram->getRAMId() << "_print <= 0;\n";
			
			for ( auto func : targetFuncSet )
			{
				*topFile << "\t\tfinish_" << func->getName() << "_tmp <= 0;\n";
				*topFile << "\t\tfinish_" << func->getName() << "_reg <= 0;\n";
				*topFile << "\t\treturn_val_" << func->getName() << "_reg <= 0;\n";
			}

			*topFile << "\tend\n";
			*topFile << "\telse begin\n";

			//finish transition
			for ( auto func : targetFuncSet ) 
			{
				*topFile << "\t\tif (finish_" << func->getName() << ") begin\n";
				*topFile << "\t\t\t$display(\"Function " << func->getName() << " End\");\n";
				*topFile << "\t\tfinish_" << func->getName() << "_tmp <= 1;\n";
				*topFile << "\t\treturn_val_" << func->getName() << "_reg <= ";
				*topFile << "return_val_" << func->getName() << ";\n";
				*topFile << "\t\tend\n";
				*topFile << "\t\telse if ( print_counter == 0 && finish_" << func->getName();
				*topFile << "_tmp == 1 ) begin\n";
				*topFile << "\t\tfinish_" << func->getName() << "_reg <= 1;\n";
				*topFile << "\t\tfinish_" << func->getName() << "_tmp <= 0;\n";
				*topFile << "\t\tend\n";
				*topFile << "\t\telse begin\n";
				*topFile << "\t\tfinish_" << func->getName() << "_reg <= 0;\n";
				*topFile << "\t\tend\n\n";
			}

			//print_counter
			*topFile << "\t\tif ( 0";
			for ( auto func : targetFuncSet )
				*topFile << " || finish_" << func->getName();
			*topFile << ")\n";
			*topFile << "\t\t\tprint_counter <= " << targetRAMSet.size() << ";\n";
			*topFile << "\t\telse if ( print_counter != 0 )\n";
			*topFile << "\t\t\tprint_counter <= print_counter -1;\n";

			//RAM_print
			for ( auto ram : targetRAMSet )
			{
				*topFile << "\t\tif ( (print_counter == " << ram->getRAMId() << ") && (0";
				for ( auto func : memoryTable->getFunctionFromRAM(ram) )
					*topFile << " || finish_" << func->getName() << "_tmp";
				*topFile << ") )\n";
				*topFile << "\t\t\tRAM" << ram->getRAMId() << "_print <= 1;\n";
				*topFile << "\t\telse\n";
				*topFile << "\t\t\tRAM" << ram->getRAMId() << "_print <= 0;\n";
			}

			*topFile << "\tend\nend\n\n";
		}

		//Function Call
		for ( auto callee : targetFuncSet )
		{
			//TODO : TOP CONNECTION : Call Bus
			if ( callee == module->getFunction("main") ||
					callee == verilogConfigInfo->getAccelFunction() ) {
				*topFile << "assign start_" << callee->getName() << " = start;\n";
				for ( unsigned i = 0; i < callee->arg_size(); ++i )
				{
					*topFile << "assign arg_" << i << "_" << callee->getName() << " = ";
					*topFile << "arg_" << i << "_ex;\n";
				}
			}
			else {
				//XXX: Exectime Profiling
				if ( verilogConfigInfo->getBluetoothUse() ) {
					*topFile << "\nreg [31:0] count_" << callee->getName() << ";\n";
					*topFile << "always @(posedge clk) begin\n";
					*topFile << "\tif(reset)\n";
					*topFile << "\t\tcount_" << callee->getName() << " <= 0;\n";
					*topFile << "\telse begin\n";
					*topFile << "\t\tif(start_" << callee->getName();
					for ( auto child : callTable->getCalleeFromFunction(callee) )
						*topFile << " && !start_" << child->getName();
					*topFile << ")\n";
					*topFile << "\t\t\tcount_" << callee->getName() << " <= count_";
					*topFile << callee->getName() << "+1;\n";
					*topFile << "\t\tif(finish)\n";
					*topFile << "\t\t\t$display(\"" << callee->getName() << "\t\%d\",count_";
					*topFile << callee->getName() << ");\n";
					*topFile << "\tend\nend\n\n";
				}

				if ( (callTable->getCallerFromFunction(callee)).size() != 0 ) {
					if ( callTable->isCallBus() ) {
						//Arguments
						for ( unsigned i = 0; i < callTable->getMaxArgNum(); ++i )
						{
							*topFile << "assign arg_" << i << "_function = ";
							for ( auto caller : targetFuncSet )
								*topFile << "send_arg_"<<i<<"_"<<caller->getName()<<" | ";
							*topFile << " 0;\n";
						}

						//Start
						*topFile << "assign start_function = ";
						for ( auto caller : targetFuncSet )
							*topFile << "send_start_" << caller->getName() << " | ";
						*topFile << " 0;\n";

						//Finish
						*topFile << "assign receive_finish_function = ";
						for ( auto caller : targetFuncSet )
							*topFile << "finish_" << caller->getName() << " | ";
						*topFile << " 0;\n";

						//Return val
						*topFile << "assign receive_return_val_function = ";
						for ( auto caller : targetFuncSet )
							*topFile << "return_val_" << caller->getName() << " | ";
						*topFile << " 0;\n\n";
					}
					else {
						for ( unsigned arg = 0; arg < callee->arg_size(); ++arg ) {
							*topFile << "assign arg_" << arg << "_" << callee->getName() << " = ";
							for ( auto caller : callTable->getCallerFromFunction(callee) )
								*topFile <<callee->getName()<<"_arg_"<<arg<<"_"<<caller->getName() << " | ";
							*topFile << "0;\n";
						}

						if ( verilogConfigInfo->getValidationMode() ) {
							*topFile << "assign start_" << callee->getName() << " = ("; 
							for ( auto caller : callTable->getCallerFromFunction(callee) )
								*topFile << callee->getName() << "_start_" << caller->getName() << " | ";
							*topFile << "0) && !(finish_" << callee->getName() << "_tmp || ";
							*topFile << "finish_" << callee->getName() << "_reg);\n\n";
						}
						else {
							*topFile << "assign start_" << callee->getName() << " = "; 
							for ( auto caller : callTable->getCallerFromFunction(callee) )
								*topFile << callee->getName() << "_start_" << caller->getName() << " | ";
							*topFile << "0;\n\n";
						}
					}
				}
			}
		} // Done : Function Call

		//Function to Memory Instance Connection
		for ( auto ram : targetRAMSet )
		{
			bool isDual = memoryTable->isDualPortRAM(ram);
			bool hasMC = ram->hasMultipleSize() || ram->hasExPort();

			*topFile << "assign address_a_RAM" << ram->getRAMId() << " = ";
			for ( auto func : memoryTable->getFunctionFromRAM(ram) )
				if ( targetFuncSet.find(func) != targetFuncSet.end() )
					*topFile << "RAM" << ram->getRAMId() << "_address_a_" << func->getName() << " | ";
			*topFile << "0;\n";

			if ( isDual ) {
				*topFile << "assign address_b_RAM" << ram->getRAMId() << " = ";
				for ( auto func : memoryTable->getFunctionFromRAM(ram) )
					if ( targetFuncSet.find(func) != targetFuncSet.end() )
						*topFile << "RAM" << ram->getRAMId() << "_address_b_" << func->getName() << " | ";
				*topFile << "0;\n";
			}

			*topFile << "assign select_en_a_RAM" << ram->getRAMId() << " = ";
			for ( auto func : memoryTable->getFunctionFromRAM(ram) )
				if ( targetFuncSet.find(func) != targetFuncSet.end() )
					*topFile << "RAM" << ram->getRAMId() << "_read_en_a_" << func->getName() << " | ";
			*topFile << "0;\n";

			if ( isDual ) {
				*topFile << "assign select_en_b_RAM" << ram->getRAMId() << " = ";
				for ( auto func : memoryTable->getFunctionFromRAM(ram) )
					if ( targetFuncSet.find(func) != targetFuncSet.end() )
						*topFile << "RAM" << ram->getRAMId() << "_read_en_b_" << func->getName() << " | ";
				*topFile << "0;\n";
			}

//			if ( ram->isReadOnly() && verilogConfigInfo->getValidMemory() &&
//					memoryTable->isByteAddressedRAM() ) {
			if ( ram->isReadOnly() ) {
			}
			else {

				*topFile << "assign write_en_a_RAM" << ram->getRAMId() << " = ";
				for ( auto func : memoryTable->getFunctionFromRAM(ram) )
					if ( targetFuncSet.find(func) != targetFuncSet.end() )
						*topFile << "RAM" << ram->getRAMId() << "_write_en_a_" << func->getName() << " | ";
				*topFile << "0;\n";

				if ( isDual ) {
					*topFile << "assign write_en_b_RAM" << ram->getRAMId() << " = ";
					for ( auto func : memoryTable->getFunctionFromRAM(ram) )
						if ( targetFuncSet.find(func) != targetFuncSet.end() )
							*topFile << "RAM" << ram->getRAMId() <<"_write_en_b_"<<func->getName()<<" | ";
					*topFile << "0;\n";
				}

				*topFile << "assign data_in_a_RAM" << ram->getRAMId() << " = ";
				for ( auto func : memoryTable->getFunctionFromRAM(ram) )
					if ( targetFuncSet.find(func) != targetFuncSet.end() )
						*topFile << "RAM" << ram->getRAMId() << "_data_out_a_" << func->getName() << " | ";
				*topFile << "0;\n";

				if ( isDual ) {
					*topFile << "assign data_in_b_RAM" << ram->getRAMId() << " = ";
					for ( auto func : memoryTable->getFunctionFromRAM(ram) )
						if ( targetFuncSet.find(func) != targetFuncSet.end() )
							*topFile << "RAM" << ram->getRAMId() <<"_data_out_b_"<<func->getName()<<" | ";
					*topFile << "0;\n\n";
				}

			}

//			if ( memoryTable->isByteAddressedRAM() ) {
			if ( hasMC ) {
				*topFile << "assign size_a_RAM" << ram->getRAMId() << " = ";
				for ( auto func : memoryTable->getFunctionFromRAM(ram) )
					if ( targetFuncSet.find(func) != targetFuncSet.end() )
						*topFile << "RAM" << ram->getRAMId() << "_size_a_" << func->getName() << " | ";
				*topFile << "0;\n";

				if ( isDual ) {
				*topFile << "assign size_b_RAM" << ram->getRAMId() << " = ";
				for ( auto func : memoryTable->getFunctionFromRAM(ram) )
					if ( targetFuncSet.find(func) != targetFuncSet.end() )
						*topFile << "RAM" << ram->getRAMId() << "_size_b_" << func->getName() << " | ";
				*topFile << "0;\n\n";
				}

				if ( isUsedInMemFunc(ram) ) {
					*topFile << "assign address_a_ex_RAM" << ram->getRAMId() << " = ";
					for ( auto func : memoryTable->getFunctionFromRAM(ram) )
						if ( targetFuncSet.find(func) != targetFuncSet.end() )
							if ( isMemFunction(func) )
								*topFile<<"RAM"<<ram->getRAMId()<<"_address_a_ex_"<<func->getName()<<" | ";
					*topFile << "0;\n";

					if ( isDual ) {
						*topFile << "assign address_b_ex_RAM" << ram->getRAMId() << " = ";
						for ( auto func : memoryTable->getFunctionFromRAM(ram) )
							if ( targetFuncSet.find(func) != targetFuncSet.end() )
								if ( isMemFunction(func) )
									*topFile<<"RAM"<<ram->getRAMId()<<"_address_b_ex_"<<func->getName()<<" | ";
						*topFile << "0;\n";
					}

					*topFile << "assign select_en_a_ex_RAM" << ram->getRAMId() << " = ";
					for ( auto func : memoryTable->getFunctionFromRAM(ram) )
						if ( targetFuncSet.find(func) != targetFuncSet.end() )
							if ( isMemFunction(func) )
								*topFile<<"RAM"<<ram->getRAMId()<<"_select_en_a_ex_"<<func->getName()<<" | ";
					*topFile << "0;\n";

					if ( isDual ) {
						*topFile << "assign select_en_b_ex_RAM" << ram->getRAMId() << " = ";
						for ( auto func : memoryTable->getFunctionFromRAM(ram) )
							if ( targetFuncSet.find(func) != targetFuncSet.end() )
								if ( isMemFunction(func) )
									*topFile<<"RAM"<<ram->getRAMId()<<"_select_en_b_ex_"<<func->getName()<<" | ";
						*topFile << "0;\n";
					}

					*topFile << "assign write_en_a_ex_RAM" << ram->getRAMId() << " = ";
					for ( auto func : memoryTable->getFunctionFromRAM(ram) )
						if ( targetFuncSet.find(func) != targetFuncSet.end() )
							if ( isMemFunction(func) )
								*topFile<<"RAM"<<ram->getRAMId()<<"_write_en_a_ex_"<<func->getName()<<" | ";
					*topFile << "0;\n";

					if ( isDual ) {
						*topFile << "assign write_en_b_ex_RAM" << ram->getRAMId() << " = ";
						for ( auto func : memoryTable->getFunctionFromRAM(ram) )
							if ( targetFuncSet.find(func) != targetFuncSet.end() )
								if ( isMemFunction(func) )
									*topFile<<"RAM"<<ram->getRAMId()<<"_write_en_b_ex_"<<func->getName()<<" | ";
						*topFile << "0;\n";
					}

					*topFile << "assign data_in_a_ex_RAM" << ram->getRAMId() << " = ";
					for ( auto func : memoryTable->getFunctionFromRAM(ram) )
						if ( targetFuncSet.find(func) != targetFuncSet.end() )
							if ( isMemFunction(func) )
								*topFile<<"RAM"<<ram->getRAMId()<<"_data_out_a_ex_"<<func->getName()<<" | ";
					*topFile << "0;\n";

					if ( isDual ) {
						*topFile << "assign data_in_b_ex_RAM" << ram->getRAMId() << " = ";
						for ( auto func : memoryTable->getFunctionFromRAM(ram) )
							if ( targetFuncSet.find(func) != targetFuncSet.end() )
								if ( isMemFunction(func) )
									*topFile<<"RAM"<<ram->getRAMId()<<"_data_out_b_ex_"<<func->getName()<<" | ";
						*topFile << "0;\n";
					}

				}
			}

		} // Done : Memory

		//Function to Operator
		for ( auto opTy : targetOPSet )
		{
			unsigned opCode = opTy.first;
			unsigned bitWidth = opTy.second;

			*topFile << "assign arg_0_OP_" << opCode << "_" << bitWidth << " = ";
			for ( auto func : operatorTable->getFunctionFromOperator(opTy) )
				if ( targetFuncSet.find(func) != targetFuncSet.end() )
					*topFile<<"OP_"<<opCode<<"_"<< bitWidth << "_arg_0_" << func->getName() << " | ";
			*topFile << "0;\n";

			*topFile << "assign arg_0_valid_OP_" << opCode << "_" << bitWidth << " = ";
			for ( auto func : operatorTable->getFunctionFromOperator(opTy) )
				if ( targetFuncSet.find(func) != targetFuncSet.end() )
					*topFile<<"OP_"<<opCode<<"_"<< bitWidth << "_arg_0_valid_"<<func->getName()<<" | ";
			*topFile << "0;\n";
		
			*topFile << "assign arg_1_OP_" << opCode << "_" << bitWidth << " = ";
			for ( auto func : operatorTable->getFunctionFromOperator(opTy) )
				if ( targetFuncSet.find(func) != targetFuncSet.end() )
					*topFile<<"OP_"<<opCode<<"_"<< bitWidth << "_arg_1_" << func->getName() << " | ";
			*topFile << "0;\n";

			*topFile << "assign arg_1_valid_OP_" << opCode << "_" << bitWidth << " = ";
			for ( auto func : operatorTable->getFunctionFromOperator(opTy) )
				if ( targetFuncSet.find(func) != targetFuncSet.end() )
					*topFile<<"OP_"<<opCode<<"_"<< bitWidth << "_arg_1_valid_"<<func->getName()<<" | ";
			*topFile << "0;\n";
		}

	}

	//Top Module is a first hierarchy for all resource
	//The role of the top module is 
	//i) instantiating resources
	//ii) connecting all resources efficiently
	void PrintVerilog::topModuleGeneration(void) {
		std::error_code ec_print = std::make_error_code(std::errc::io_error);
		raw_fd_ostream topFile(
				verilogConfigInfo->getNoMC() ? "topModule_no_mc.v" : "topModule.v",
				ec_print, llvm::sys::fs::F_None);

//		printDefinedParameter(&topFile);
		topFile << "`timescale 1ns / 1ps\n\n";

		topFile << "module top" << " (\n";
		topFile << "input clk,\n";
		topFile << "input reset,\n";
		//FIXME 
		/*
		if ( verilogConfigInfo->getAccel() ) {
			for ( auto ram : targetRAMSet )
			{
				topFile << "input [" << ram->getElementBitSize()-1 << ":0] address_a_RAM";
				topFile << ram->getRAMId() << "_ex,\n";
				topFile << "input [" << ram->getElementBitSize()-1 << ":0] address_b_RAM";
				topFile << ram->getRAMId() << "_ex,\n";
				topFile << "input  select_en_a_RAM" << ram->getRAMId() << "_ex,\n";
				topFile << "input  select_en_b_RAM" << ram->getRAMId() << "_ex,\n";
				topFile << "input  write_en_a_RAM" << ram->getRAMId() << "_ex,\n";
				topFile << "input  write_en_b_RAM" << ram->getRAMId() << "_ex,\n";
				topFile << "input  [" << memoryTable->getMemBitWidth()-1 << ":0] data_in_a_RAM";
				topFile << ram->getRAMId() << "_ex,\n";
				topFile << "input  [" << memoryTable->getMemBitWidth()-1 << ":0] data_in_b_RAM";
				topFile << ram->getRAMId() << "_ex,\n";
				topFile << "output [" << memoryTable->getMemBitWidth()-1 << ":0] data_out_a_RAM";
				topFile << ram->getRAMId() << "_ex,\n";
				topFile << "output [" << memoryTable->getMemBitWidth()-1 << ":0] data_out_b_RAM";
				topFile << ram->getRAMId() << "_ex,\n";
				topFile << "output valid_a_RAM" << ram->getRAMId() << "_ex,\n";
				topFile << "output valid_b_RAM" << ram->getRAMId() << "_ex,\n";
			}
		}*/

		topFile << "input start,\n";
		Function *func = NULL;
		if ( verilogConfigInfo->getAccel() )
			func = verilogConfigInfo->getAccelFunction();
		else
			func = module->getFunction("main");
		assert(func);

		unsigned numArg = func->arg_size();
		for ( unsigned i = 0; i < numArg; ++i )
		{
			topFile << "input [" << callTable->getFuncArgBit(func, i) -1;
			topFile << ":0] arg_" << i << "_ex,\n";
		}
		topFile << "output finish,\n";
		topFile << "output [" << callTable->getFuncReturnBit(func)-1 << ":0] return_val\n";
		topFile << ");\n\n";

		printInstantiation(&topFile);

		printConnection(&topFile);

		//TODO : TOP CONNECTION : Call Bus 
		if ( verilogConfigInfo->getAccel() ) {
			Function *target = verilogConfigInfo->getAccelFunction();
			topFile << "assign finish = finish_" << target->getName() << ";\n";
			topFile << "assign return_val = return_val_" << target->getName() << ";\n";
		}
		else {
			if ( verilogConfigInfo->getValidationMode() ) {
				topFile << "assign finish = finish_main_reg;\n";
				topFile << "assign return_val = return_val_main_reg;\n";
			}
			else {
				topFile << "assign finish = finish_main;\n";
				topFile << "assign return_val = return_val_main;\n";
			}
		}

		topFile << "endmodule\n";

		topFile.close();
	}

	void PrintVerilog::targetFuncSetting(Function *target) {
		targetFuncSet.insert(target);
		for ( auto callee : callTable->getCalleeFromFunction(target) )
			targetFuncSetting(callee);
	}

	void PrintVerilog::targetSetting(void) {
		if ( verilogConfigInfo->getAccel() ) {
			Function *targetFunc = verilogConfigInfo->getAccelFunction();
			targetFuncSetting(targetFunc);

			for ( auto func : targetFuncSet )
			{
				for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
					targetRAMSet.insert(ram);
				for ( auto op : operatorTable->getUsedOperatorByFunction(func) )
					targetOPSet.insert(op);
			}
		}
		else {
			for ( auto fi = module->begin(); fi != module->end(); ++fi )
			{
				if ( (&*fi)->isDeclaration() )
					continue;
				targetFuncSet.insert(&*fi);
			}
			for ( auto ram : memoryTable->getAllRAMList() )
				targetRAMSet.insert(ram);
			for ( auto op : operatorTable->getUsedOperator() )
				targetOPSet.insert(op);
		}

		//find memset, memcpy ( optimized version codes )
		if ( !verilogConfigInfo->getNoMF() ) {
			if ( memoryTable->isByteAddressedRAM() ) {
				for ( auto func : targetFuncSet )
				{
					std::string nameStr = func->getName().str();
					nameStr.resize(14);
					if ( nameStr == "corelab_memset" )
						setFunctions.insert(func);
					if ( nameStr == "corelab_memcpy" )
						cpyFunctions.insert(func);
				}
			}
		}
	}

	bool PrintVerilog::isUsedInMemFunc(RAM_ *ram) {
		for ( auto func : setFunctions )
			for ( auto ramIter : memoryTable->getAccessRAMFromFunction(func) )
				if ( ram == ramIter )
					return true;
		for ( auto func : cpyFunctions )
			for ( auto ramIter : memoryTable->getAccessRAMFromFunction(func) )
				if ( ram == ramIter )
					return true;
		return false;
	}

	void PrintVerilog::functionModuleDefineForMem(Function *func) {
		StringRef fileName = func->getName();
		SmallString<256> nameBuf;
		nameBuf.clear();
		fileName = Twine( Twine(fileName) + Twine(".v") ).toStringRef(nameBuf);

		std::error_code ec_print = std::make_error_code(std::errc::io_error);
		raw_fd_ostream funcFile(fileName.str(), ec_print, llvm::sys::fs::F_None);

		errs() << "\t" << func->getName() << " Function Module Declaration\n";
		printFunctionDeclaration(func, &funcFile);

		printFunctionRegisterForMem(func, &funcFile);

		printFunctionStateTransitionForMem(func, &funcFile);

		printFunctionOperationForMem(func, &funcFile);

		funcFile << "endmodule\n";
	}


	void PrintVerilog::printFunctionRegisterForMem(Function *func, raw_fd_ostream *funcFile) {
		for ( unsigned i = 0; i < func->arg_size(); ++i ) {
			*funcFile << "reg [" << callTable->getFuncArgBit(func, i);
			*funcFile << ":0] arg_" << i << "_reg;\n";
		}
		*funcFile << "\n";

		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			*funcFile << "reg [" << ram->getElementBitSize()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_address_a_reg;\n";
			*funcFile << "reg [" << ram->getElementBitSize()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_address_b_reg;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_read_en_a_reg;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_read_en_b_reg;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_write_en_a_reg;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_write_en_b_reg;\n";
			*funcFile << "reg [" << ram->getDataBitSize()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_a_reg;\n";
			*funcFile << "reg [" << ram->getDataBitSize()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_b_reg;\n";
			*funcFile << "reg [3:0] RAM" << ram->getRAMId() << "_size_a_reg;\n";
			*funcFile << "reg [3:0] RAM" << ram->getRAMId() << "_size_b_reg;\n";

			*funcFile << "reg [" << ram->getElementBitSize()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_address_a_ex_reg;\n";
			*funcFile << "reg [" << ram->getElementBitSize()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_address_b_ex_reg;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_select_en_a_ex_reg;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_select_en_b_ex_reg;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_write_en_a_ex_reg;\n";
			*funcFile << "reg RAM" << ram->getRAMId() << "_write_en_b_ex_reg;\n";
			*funcFile << "reg [" << memoryTable->getMemBitWidth()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_a_ex_reg;\n";
			*funcFile << "reg [" << memoryTable->getMemBitWidth()-1;
			*funcFile << ":0] RAM" << ram->getRAMId() << "_data_out_b_ex_reg;\n";

			*funcFile << "assign RAM" << ram->getRAMId() << "_address_a = RAM";
			*funcFile << ram->getRAMId() << "_address_a_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_address_b = RAM";
			*funcFile << ram->getRAMId() << "_address_b_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_read_en_a = RAM";
			*funcFile << ram->getRAMId() << "_read_en_a_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_read_en_b = RAM";
			*funcFile << ram->getRAMId() << "_read_en_b_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_write_en_a = RAM";
			*funcFile << ram->getRAMId() << "_write_en_a_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_write_en_b = RAM";
			*funcFile << ram->getRAMId() << "_write_en_b_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_data_out_a = RAM";
			*funcFile << ram->getRAMId() << "_data_out_a_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_data_out_b = RAM";
			*funcFile << ram->getRAMId() << "_data_out_b_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_size_a = RAM";
			*funcFile << ram->getRAMId() << "_size_a_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_size_b = RAM";
			*funcFile << ram->getRAMId() << "_size_b_reg;\n";

			*funcFile << "assign RAM" << ram->getRAMId() << "_address_a_ex = RAM";
			*funcFile << ram->getRAMId() << "_address_a_ex_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_address_b_ex = RAM";
			*funcFile << ram->getRAMId() << "_address_b_ex_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_select_en_a_ex = RAM";
			*funcFile << ram->getRAMId() << "_select_en_a_ex_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_select_en_b_ex = RAM";
			*funcFile << ram->getRAMId() << "_select_en_b_ex_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_write_en_a_ex = RAM";
			*funcFile << ram->getRAMId() << "_write_en_a_ex_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_write_en_b_ex = RAM";
			*funcFile << ram->getRAMId() << "_write_en_b_ex_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_data_out_a_ex = RAM";
			*funcFile << ram->getRAMId() << "_data_out_a_ex_reg;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_data_out_b_ex = RAM";
			*funcFile << ram->getRAMId() << "_data_out_b_ex_reg;\n\n";
		}

		unsigned memBitWidth = memoryTable->getMemBitWidth();
		unsigned memBlockAddressSize = getRequiredBits(memBitWidth/8);
		unsigned addressBitSize 
			= memoryTable->getRAMBitSize() + memoryTable->getElementBitSize();
		unsigned dataBitWidth = memoryTable->getBitWidth();

		*funcFile << "reg [" << addressBitSize-1 << ":0] address_a;\n";
		*funcFile << "reg [" << addressBitSize-1 << ":0] address_b;\n";
		*funcFile << "reg read_en_a;\n";
		*funcFile << "reg read_en_b;\n";
		*funcFile << "reg write_en_a;\n";
		*funcFile << "reg write_en_b;\n";
		*funcFile << "reg [" << dataBitWidth-1 << ":0] data_out_a;\n";
		*funcFile << "reg [" << dataBitWidth-1 << ":0] data_out_b;\n";
		*funcFile << "reg [3:0] size_a;\n";
		*funcFile << "reg [3:0] size_b;\n";
		*funcFile << "reg [" << addressBitSize-1 << ":0] address_a_ex;\n";
		*funcFile << "reg [" << addressBitSize-1 << ":0] address_b_ex;\n";
		*funcFile << "reg select_en_a_ex;\n";
		*funcFile << "reg select_en_b_ex;\n";
		*funcFile << "reg write_en_a_ex;\n";
		*funcFile << "reg write_en_b_ex;\n";
		*funcFile << "reg [" << memBitWidth-1 << ":0] data_out_a_ex;\n";
		*funcFile << "reg [" << memBitWidth-1 << ":0] data_out_b_ex;\n\n";

		*funcFile << "wire [" << dataBitWidth-1 << ":0] data_in_a;\n";
		*funcFile << "wire [" << dataBitWidth-1 << ":0] data_in_b;\n";
		*funcFile << "wire [" << memBitWidth-1 << ":0] data_in_a_ex;\n";
		*funcFile << "wire [" << memBitWidth-1 << ":0] data_in_b_ex;\n\n";

		*funcFile << "wire memory_stall;\n";
		*funcFile << "wire memory_stall_a;\n";
		*funcFile << "wire memory_stall_b;\n";
		*funcFile << "reg stall_before;\n";

		*funcFile << "assign memory_stall_a = ";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) ) {
			if ( ram->hasMultipleSize() || ram->hasExPort() ) {
				*funcFile << "((RAM" << ram->getRAMId() << "_read_en_a | ";
				*funcFile << "RAM" << ram->getRAMId() << "_write_en_a) & ";
				*funcFile << "~RAM" << ram->getRAMId() << "_memory_valid_a) | ";
			}
		}
		*funcFile << "0;\n";
		*funcFile << "assign memory_stall_b = ";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) ) {
			if ( ram->hasMultipleSize() || ram->hasExPort() ) {
				if ( memoryTable->isDualPortRAM(ram) ) {
					*funcFile << "((RAM" << ram->getRAMId() << "_read_en_b | ";
					*funcFile << "RAM" << ram->getRAMId() << "_write_en_b) & ";
					*funcFile << "~RAM" << ram->getRAMId() << "_memory_valid_b) | ";
				}
			}
		}
		*funcFile << "0;\n";

		*funcFile << "assign memory_stall = memory_stall_a | memory_stall_b;\n\n";

		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			*funcFile << "wire [" << ram->getDataBitSize()-1 << ":0] ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_a;\n";
			*funcFile << "wire [" << ram->getDataBitSize()-1 << ":0] ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_b;\n";
			*funcFile << "reg [" << ram->getDataBitSize()-1 << ":0] ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_a_stall;\n";
			*funcFile << "reg [" << ram->getDataBitSize()-1 << ":0] ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_b_stall;\n\n";
		}

		unsigned argBit[3];
		for ( unsigned arg = 0; arg < func->arg_size(); ++arg )
			argBit[arg] = callTable->getFuncArgBit(func, arg);

		unsigned memAccessSize = getMemFunctionBitSize(func);

		if ( isSetFunction(func) ) {
			*funcFile << "reg [" << memAccessSize-1 << ":0] set_value_reg;\n";
			*funcFile << "reg [" << argBit[2]-1 << ":0] size_reg;\n";
			*funcFile << "reg [" << addressBitSize-1 << ":0] dst_ptr_reg;\n";
			*funcFile << "reg [" << memAccessSize-1 << ":0] set_value_next;\n";
			*funcFile << "reg [" << argBit[2]-1 << ":0] size_next;\n";
			*funcFile << "reg [" << addressBitSize-1 << ":0] dst_ptr_next;\n\n";

			*funcFile << "wire isFinish;\n";
			*funcFile << "wire isShort;\n";
			*funcFile << "wire isEx;\n";
			*funcFile << "wire isExD;\n";
			*funcFile << "wire isAligned;\n";
			*funcFile << "assign isFinish = size_next == 0;\n";
			*funcFile << "assign isShort = size_next < ";
			*funcFile << memoryTable->getMemBitWidth()/8 << ";\n";
			*funcFile << "assign isEx = " << memBitWidth/8;
			*funcFile << " <= size_next;\n";
			*funcFile << "assign isExD = " << (memBitWidth/8)*2;
			*funcFile << " <= size_next;\n";
			*funcFile << "assign isAligned = dst_ptr_next[" << memBlockAddressSize-1;
			*funcFile << ":0] == " << memBlockAddressSize << "'d0;\n\n";

			*funcFile << "reg [9:0] cur_state;\n";
			*funcFile << "reg [9:0] next_state;\n";
			*funcFile << "parameter State_Wait = 10'd0;\n";
			*funcFile << "parameter State_Op = 10'b00000_00001;\n";
			*funcFile << "parameter State_Ex = 10'b00000_00010;\n";
			*funcFile << "parameter State_ExD = 10'b00000_00100;\n";
			*funcFile << "parameter State_Finish = 10'b00000_01000;\n\n";
		}
		else if ( isCpyFunction(func) ) {
			*funcFile << "reg [" << addressBitSize-1 << ":0] dst_ptr_reg;\n";
			*funcFile << "reg [" << addressBitSize-1 << ":0] src_ptr_reg;\n";
			*funcFile << "reg [" << argBit[2]-1 << ":0] size_reg;\n";
			*funcFile << "reg [" << addressBitSize-1 << ":0] dst_ptr_next;\n";
			*funcFile << "reg [" << addressBitSize-1 << ":0] src_ptr_next;\n";
			*funcFile << "reg [" << argBit[2]-1 << ":0] size_next;\n";


			*funcFile << "wire isFinish;\n";
			*funcFile << "wire isOp;\n";
//			*funcFile << "wire isOpD;\n";
			*funcFile << "wire isEx;\n";
//			*funcFile << "wire isExD;\n";
			*funcFile << "wire isAligned;\n";
			*funcFile << "wire isSameOffset;\n";
			*funcFile << "assign isFinish = size_next == 0;\n";
			*funcFile << "assign isOp = size_next < ";
			*funcFile << memoryTable->getMemBitWidth()/8 << ";\n";
//			*funcFile << "assign isOpD = isOp && " << dataByteWidth *2 << " <= size_next;\n";
			*funcFile << "assign isEx = " << memBitWidth/8;
			*funcFile << " <= size_next;\n";
//			*funcFile << "assign isExD = " << (memBitWidth/8)*2;
//			*funcFile << " <= size_next;\n";
			*funcFile << "assign isAligned = dst_ptr_next[" << memBlockAddressSize-1;
			*funcFile << ":0] == " << memBlockAddressSize << "'d0;\n";
			*funcFile << "assign isSameOffset = dst_ptr_next[" << memBlockAddressSize-1;
			*funcFile << ":0] == src_ptr_next[" << memBlockAddressSize-1 << ":0];\n\n";

			*funcFile << "reg [9:0] cur_state;\n";
			*funcFile << "reg [9:0] next_state;\n";
			*funcFile << "parameter State_Wait = 10'd0;\n";
			*funcFile << "parameter State_Op = 10'b00000_00001;\n";
			*funcFile << "parameter State_OpT = 10'b00000_00010;\n";
			*funcFile << "parameter State_Ex = 10'b00000_00100;\n";
			*funcFile << "parameter State_ExT = 10'b00000_01000;\n";
			*funcFile << "parameter State_Finish = 10'b00000_10000;\n\n";
		}
	}

	void PrintVerilog::printFunctionStateTransitionForMem(Function *func, raw_fd_ostream *funcFile){
		*funcFile << "always @(posedge clk) begin\n";
		*funcFile << "if (reset == 1'b1)\n";
		*funcFile << "\tcur_state <= State_Wait;\n";
		*funcFile << "else\n\tcur_state <= next_state;\n";
		*funcFile << "end\n\n";

		unsigned memBitWidth = memoryTable->getMemBitWidth();
		unsigned memBlockAddressSize = getRequiredBits(memBitWidth/8);
		unsigned addressBitSize 
			= memoryTable->getRAMBitSize() + memoryTable->getElementBitSize();

		if ( isSetFunction(func) ) {
			*funcFile << "always @(*) begin\n";
			*funcFile << "next_state = cur_state;\n";
			*funcFile << "if ( memory_stall == 1 )\n";
			*funcFile << "\tnext_state = cur_state;\n";
			*funcFile << "else begin\n";
			*funcFile << "case(cur_state)\n";
			*funcFile << "State_Wait:\n";
			*funcFile << "\tif (start == 1'd1) begin\n";
			*funcFile << "\tif ( arg_0[" << memBlockAddressSize-1 << ":0] == 0 ) begin\n";

			*funcFile << "\t\tif ( arg_2 < " << memBitWidth/8 << " )\n";
			*funcFile << "\t\t\tnext_state = State_Op;\n";
			*funcFile << "\t\telse if ( " << (memBitWidth/8)*2 << " <= arg_2 )\n";
			*funcFile << "\t\t\tnext_state = State_ExD;\n";
			*funcFile << "\t\telse if ( " << (memBitWidth/8) << " <= arg_2 )\n";
			*funcFile << "\t\t\tnext_state = State_Ex;\n";

			*funcFile << "\tend\n";
			*funcFile << "\telse\n";
			*funcFile << "\t\tnext_state = State_Op;\n";

			*funcFile << "\tend\n";
			*funcFile << "\telse\n";
			*funcFile << "\t\tnext_state = State_Wait;\n";

			*funcFile << "State_Op:\n";
			*funcFile << "\tif ( isFinish )\n";
			*funcFile << "\t\tnext_state = State_Finish;\n";
			*funcFile << "\telse if ( isAligned && isExD )\n";
			*funcFile << "\t\tnext_state = State_ExD;\n";
			*funcFile << "\telse if ( isAligned && isEx )\n";
			*funcFile << "\t\tnext_state = State_Ex;\n";
			*funcFile << "\telse\n";
			*funcFile << "\t\tnext_state = State_Op;\n";


			*funcFile << "State_ExD:\n";
			*funcFile << "\tif ( isFinish )\n";
			*funcFile << "\t\tnext_state = State_Finish;\n";
			*funcFile << "\telse if ( isShort )\n";
			*funcFile << "\t\tnext_state = State_Op;\n";
			*funcFile << "\telse if ( isExD )\n";
			*funcFile << "\t\tnext_state = State_ExD;\n";
			*funcFile << "\telse if ( isEx )\n";
			*funcFile << "\t\tnext_state = State_Ex;\n";

			*funcFile << "State_Ex:\n";
			*funcFile << "\tif ( isFinish )\n";
			*funcFile << "\t\tnext_state = State_Finish;\n";
			*funcFile << "\telse if ( isShort )\n";
			*funcFile << "\t\tnext_state = State_Op;\n";
			*funcFile << "\telse if ( isEx )\n";
			*funcFile << "\t\tnext_state = State_Ex;\n";

			*funcFile << "State_Finish:\n";
			*funcFile << "\tnext_state = State_Wait;\n";

			*funcFile << "default:\n\tnext_state = cur_state;\n";
			*funcFile << "endcase\n";
			*funcFile << "end\nend\n\n";
		}
		else if ( isCpyFunction(func) ) {
			*funcFile << "always @(*) begin\n";
			*funcFile << "next_state = cur_state;\n";
			*funcFile << "if ( memory_stall == 1 )\n";
			*funcFile << "\tnext_state = cur_state;\n";
			*funcFile << "else begin\n";
			*funcFile << "case(cur_state)\n";
			*funcFile << "State_Wait:\n";
			*funcFile << "\tif (start == 1'd1) begin\n";
			*funcFile << "\t\tif ( arg_0[" << memBlockAddressSize-1 << ":0] == ";
			*funcFile << "arg_1[" << memBlockAddressSize-1 << ":0] ) begin\n";
			*funcFile << "\t\t\tif ( arg_0[" << memBlockAddressSize-1 << ":0] == 0 ) begin\n";
			*funcFile << "\t\t\tif ( " << (memBitWidth/8) << " <= arg_2 )\n";
			*funcFile << "\t\t\t\tnext_state = State_ExT;\n";
			*funcFile << "\t\t\telse\n";
			*funcFile << "\t\t\t\tnext_state = State_OpT;\n";
			*funcFile << "\t\t\tend\n";
			*funcFile << "\t\t\telse\n";
			*funcFile << "\t\t\t\tnext_state = State_OpT;\n";
			*funcFile << "\t\tend\n";
			*funcFile << "\t\telse begin\n"; //not same offset
			*funcFile << "\t\t\t\tnext_state = State_OpT;\n";
			*funcFile << "\t\tend\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse\n";
			*funcFile << "\t\tnext_state = State_Wait;\n";

			*funcFile << "State_Op:\n";
			*funcFile << "\tif ( isFinish )\n";
			*funcFile << "\t\tnext_state = State_Finish;\n";
			*funcFile << "\telse if ( isSameOffset && isAligned && isEx )\n";
			*funcFile << "\t\tnext_state = State_ExT;\n";
			*funcFile << "\telse\n";
			*funcFile << "\t\tnext_state = State_Op;\n";

			*funcFile << "State_OpT:\n";
			*funcFile << "\t\tnext_state = State_Op;\n";

			*funcFile << "State_ExT:\n";
			*funcFile << "\t\tnext_state = State_Ex;\n";

			*funcFile << "State_Ex:\n";
			*funcFile << "\tif ( isFinish )\n";
			*funcFile << "\t\tnext_state = State_Finish;\n";
			*funcFile << "\telse if ( isOp )\n";
			*funcFile << "\t\tnext_state = State_OpT;\n";
			*funcFile << "\telse if ( isEx )\n";
			*funcFile << "\t\tnext_state = State_Ex;\n";

			*funcFile << "State_Finish:\n";
			*funcFile << "\tnext_state = State_Wait;\n";

			*funcFile << "default:\n\tnext_state = cur_state;\n";
			*funcFile << "endcase\n";
			*funcFile << "end\nend\n\n";
		}
	}


	void PrintVerilog::printFunctionOperationForMem(Function *func, raw_fd_ostream *funcFile){
		unsigned memBitWidth = memoryTable->getMemBitWidth();
		unsigned memBlockAddressSize = getRequiredBits(memBitWidth/8);
		unsigned addressBitSize 
			= memoryTable->getRAMBitSize() + memoryTable->getElementBitSize();

		unsigned ramBitSize = memoryTable->getRAMBitSize();
		unsigned elementBitSize = memoryTable->getElementBitSize();
		unsigned addressSize = ramBitSize + elementBitSize;
		unsigned bitWidth = memoryTable->getBitWidth();

		unsigned memAccessSize = getMemFunctionBitSize(func);

		*funcFile << "always @(posedge clk) begin\n";
		*funcFile << "\tstall_before <= memory_stall;\n";
		*funcFile << "end\n\n";

		*funcFile << "always @(posedge clk) begin\n";
		*funcFile << "\tif ( memory_stall ) begin\n";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_in_a_stall <= ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_a_input;\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_in_b_stall <= ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_b_input;\n";
		}
		*funcFile << "\tend\n";
		*funcFile << "end\n\n";

		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			*funcFile << "assign RAM" << ram->getRAMId() << "_data_in_a	= stall_before ? ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_a_stall : ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_a_input;\n";
			*funcFile << "assign RAM" << ram->getRAMId() << "_data_in_b	= stall_before ? ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_b_stall : ";
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_b_input;\n";
		}

		*funcFile << "assign data_in_a = ";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_a | ";
		*funcFile << "0;\n";

		*funcFile << "assign data_in_b = ";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_b | ";
		*funcFile << "0;\n";
		*funcFile << "\n";

		*funcFile << "assign data_in_a_ex = ";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_a_ex | ";
		*funcFile << "0;\n";

		*funcFile << "assign data_in_b_ex = ";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			*funcFile << "RAM" << ram->getRAMId() << "_data_in_b_ex | ";
		*funcFile << "0;\n";
		*funcFile << "\n";

		bool firstIf = true;
		//A
		*funcFile << "always @(*) begin\n";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			*funcFile << "\tRAM" << ram->getRAMId() << "_address_a_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_read_en_a_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_write_en_a_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_data_out_a_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_size_a_reg = 0;\n";
		}
		*funcFile << "\n";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			if ( firstIf ) {
				firstIf = false;
				*funcFile << "\t";
			}
			else
				*funcFile << "\telse ";
			*funcFile << "if ( address_a[" << addressSize-1 << ":" << addressSize-ramBitSize;
			*funcFile << "] == " << ramBitSize << "'d" << ram->getRAMId() << " ) begin\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_address_a_reg = address_a";
			*funcFile << "[" << ram->getElementBitSize()-1 << ":0];\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_read_en_a_reg = read_en_a;\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_write_en_a_reg = write_en_a;\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_out_a_reg = data_out_a";
			*funcFile << "[" << ram->getDataBitSize()-1 << ":0];\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_size_a_reg = size_a;\n";
			*funcFile << "\tend\n";
		}
		*funcFile << "end\n\n";

		firstIf = true;
		//B
		*funcFile << "always @(*) begin\n";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			*funcFile << "\tRAM" << ram->getRAMId() << "_address_b_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_read_en_b_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_write_en_b_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_data_out_b_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_size_b_reg = 0;\n";
		}
		*funcFile << "\n";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			if ( firstIf ) {
				firstIf = false;
				*funcFile << "\t";
			}
			else
				*funcFile << "\telse ";
			*funcFile << "if ( address_b[" << addressSize-1 << ":" << addressSize-ramBitSize;
			*funcFile << "] == " << ramBitSize << "'d" << ram->getRAMId() << " ) begin\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_address_b_reg = address_b";
			*funcFile << "[" << ram->getElementBitSize()-1 << ":0];\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_read_en_b_reg = read_en_b;\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_write_en_b_reg = write_en_b;\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_out_b_reg = data_out_b";
			*funcFile << "[" << ram->getDataBitSize()-1 << ":0];\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_size_b_reg = size_b;\n";
			*funcFile << "\tend\n";
		}
		*funcFile << "end\n\n";

		firstIf = true;
		//A EX
		*funcFile << "always @(*) begin\n";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			*funcFile << "\tRAM" << ram->getRAMId() << "_address_a_ex_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_select_en_a_ex_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_write_en_a_ex_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_data_out_a_ex_reg = 0;\n";
		}
		*funcFile << "\n";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			if ( firstIf ) {
				firstIf = false;
				*funcFile << "\t";
			}
			else
				*funcFile << "\telse ";
			*funcFile << "if ( address_a_ex[" << addressSize-1 << ":" << addressSize-ramBitSize;
			*funcFile << "] == " << ramBitSize << "'d" << ram->getRAMId() << " ) begin\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_address_a_ex_reg = address_a_ex";
			*funcFile << "[" << ram->getElementBitSize()-1 << ":0];\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_select_en_a_ex_reg = select_en_a_ex;\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_write_en_a_ex_reg = write_en_a_ex;\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_out_a_ex_reg = data_out_a_ex;\n";

			*funcFile << "\tend\n";
		}
		*funcFile << "end\n\n";

		firstIf = true;
		//B EX
		*funcFile << "always @(*) begin\n";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			*funcFile << "\tRAM" << ram->getRAMId() << "_address_b_ex_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_select_en_b_ex_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_write_en_b_ex_reg = 0;\n";
			*funcFile << "\tRAM" << ram->getRAMId() << "_data_out_b_ex_reg = 0;\n";
		}
		*funcFile << "\n";
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			if ( firstIf ) {
				firstIf = false;
				*funcFile << "\t";
			}
			else
				*funcFile << "\telse ";
			*funcFile << "if ( address_b_ex[" << addressSize-1 << ":" << addressSize-ramBitSize;
			*funcFile << "] == " << ramBitSize << "'d" << ram->getRAMId() << " ) begin\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_address_b_ex_reg = address_b_ex";
			*funcFile << "[" << ram->getElementBitSize()-1 << ":0];\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_select_en_b_ex_reg = select_en_b_ex;\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_write_en_b_ex_reg = write_en_b_ex;\n";
			*funcFile << "\t\tRAM" << ram->getRAMId() << "_data_out_b_ex_reg = data_out_b_ex;\n";

			*funcFile << "\tend\n";
		}
		*funcFile << "end\n\n";

		if ( isSetFunction(func) ) {
			*funcFile << "wire [" << memBitWidth-1 << ":0] set_value_ex;\n";
			*funcFile << "assign set_value_ex = {\n";
			for (unsigned i = 8; i<memBitWidth; i=i+8 )
				*funcFile << "set_value_reg,";
			*funcFile << "set_value_reg};\n\n";

			*funcFile << "always @(posedge clk) begin\n";
			*funcFile << "\tif (reset) begin\n";
			*funcFile << "\t\tdst_ptr_reg <= 0;\n";
			*funcFile << "\t\tset_value_reg <= 0;\n";
			*funcFile << "\t\tsize_reg <= 0;\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse if (!memory_stall) begin\n";
			*funcFile << "\t\tdst_ptr_reg <= dst_ptr_next;\n";
			*funcFile << "\t\tset_value_reg <= set_value_next;\n";
			*funcFile << "\t\tsize_reg <= size_next;\n";
			*funcFile << "\tend\nend\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\tif(cur_state == State_Wait && start == 1'd1) begin\n";
			*funcFile << "\t\tdst_ptr_next = arg_0;\n";
			*funcFile << "\t\tset_value_next = {\n";
			for (unsigned i = 8; i<memAccessSize; i=i+8)
				*funcFile << "arg_1,";
			*funcFile << "arg_1};\n";
			*funcFile << "\t\tsize_next = arg_2;\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse if(cur_state == State_Op ) begin\n";
			*funcFile << "\t\tsize_next = size_reg - " << memAccessSize/8<< ";\n";
			*funcFile << "\t\tdst_ptr_next = dst_ptr_reg + " << memAccessSize/8 << ";\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse if(cur_state == State_Ex ) begin\n";
			*funcFile << "\t\tsize_next = size_reg - " << memBitWidth/8 << ";\n";
			*funcFile << "\t\tdst_ptr_next = dst_ptr_reg + " << memBitWidth/8 << ";\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse if(cur_state == State_ExD ) begin\n";
			*funcFile << "\t\tsize_next = size_reg - " << (memBitWidth/8)*2 << ";\n";
			*funcFile << "\t\tdst_ptr_next = dst_ptr_reg + " << (memBitWidth/8)*2 << ";\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse begin\n";
			*funcFile << "\t\tdst_ptr_next = dst_ptr_reg;\n";
			*funcFile << "\t\tset_value_next = set_value_reg;\n";
			*funcFile << "\t\tsize_next = size_reg;\n";
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\taddress_a = 0;\n";
			*funcFile << "\tread_en_a = 0;\n";
			*funcFile << "\twrite_en_a = 0;\n";
			*funcFile << "\tdata_out_a = 0;\n";
			*funcFile << "\tsize_a = 0;\n";
			*funcFile << "\tif(cur_state == State_Op) begin\n";
			*funcFile << "\t\taddress_a = dst_ptr_reg;\n";
			*funcFile << "\t\tread_en_a = 1;\n";
			*funcFile << "\t\twrite_en_a = 1;\n";
			*funcFile << "\t\tdata_out_a = set_value_reg;\n";
			*funcFile << "\t\tsize_a = " << memAccessSize/8 << ";\n";
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\taddress_b = 0;\n";
			*funcFile << "\tread_en_b = 0;\n";
			*funcFile << "\twrite_en_b = 0;\n";
			*funcFile << "\tdata_out_b = 0;\n";
			*funcFile << "\tsize_b = 0;\n";
			*funcFile << "\tif(reset)begin\n";
			*funcFile << "\t\taddress_b = 0;\n";
			*funcFile << "\t\tread_en_b = 0;\n";
			*funcFile << "\t\twrite_en_b = 0;\n";
			*funcFile << "\t\tdata_out_b = 0;\n";
			*funcFile << "\t\tsize_b = 0;\n";
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\taddress_a_ex = 0;\n";
			*funcFile << "\tselect_en_a_ex = 0;\n";
			*funcFile << "\twrite_en_a_ex = 0;\n";
			*funcFile << "\tdata_out_a_ex = 0;\n";
			*funcFile << "\tif(cur_state == State_ExD) begin\n";
			*funcFile << "\t\taddress_a_ex = dst_ptr_reg;\n";
			*funcFile << "\t\tselect_en_a_ex = 1;\n";
			*funcFile << "\t\twrite_en_a_ex = 1;\n";
			*funcFile << "\t\tdata_out_a_ex = set_value_ex;\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse if(cur_state == State_Ex) begin\n";
			*funcFile << "\t\taddress_a_ex = dst_ptr_reg;\n";
			*funcFile << "\t\tselect_en_a_ex = 1;\n";
			*funcFile << "\t\twrite_en_a_ex = 1;\n";
			*funcFile << "\t\tdata_out_a_ex = set_value_ex;\n";
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\taddress_b_ex = 0;\n";
			*funcFile << "\tselect_en_b_ex = 0;\n";
			*funcFile << "\twrite_en_b_ex = 0;\n";
			*funcFile << "\tdata_out_b_ex = 0;\n";
			*funcFile << "\tif(cur_state == State_ExD) begin\n";
			*funcFile << "\t\taddress_b_ex = dst_ptr_reg + " << memBitWidth/8 << ";\n";
			*funcFile << "\t\tselect_en_b_ex = 1;\n";
			*funcFile << "\t\twrite_en_b_ex = 1;\n";
			*funcFile << "\t\tdata_out_b_ex = set_value_ex;\n";
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\tfinish = 0;\n";
			*funcFile << "\treturn_val = 0;\n";
			*funcFile << "\t\tif(cur_state == State_Finish) begin\n";
			*funcFile << "\t\t\tfinish = 1;\n";
			*funcFile << "\t\t\treturn_val = 0;\n";
			if ( verilogConfigInfo->getBluetoothUse() )
				*funcFile << "\t\t\t$display(\"Function " << func->getName() << " End\");\n";
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";
		}
		else if ( isCpyFunction(func) ) {
			*funcFile << "always @(posedge clk) begin\n";
			*funcFile << "\tif (reset) begin\n";
			*funcFile << "\t\tdst_ptr_reg <= 0;\n";
			*funcFile << "\t\tsrc_ptr_reg <= 0;\n";
			*funcFile << "\t\tsize_reg <= 0;\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse if (!memory_stall) begin\n";
			*funcFile << "\t\tdst_ptr_reg <= dst_ptr_next;\n";
			*funcFile << "\t\tsrc_ptr_reg <= src_ptr_next;\n";
			*funcFile << "\t\tsize_reg <= size_next;\n";
			*funcFile << "\tend\nend\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\tif(cur_state == State_Wait && start == 1'd1) begin\n";
			*funcFile << "\t\tdst_ptr_next = arg_0;\n";
			*funcFile << "\t\tsrc_ptr_next = arg_1;\n";
			*funcFile << "\t\tsize_next = arg_2;\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse if(cur_state == State_Op ) begin\n";
			*funcFile << "\t\tsize_next = size_reg - " << memAccessSize/8 << ";\n";
			*funcFile << "\t\tdst_ptr_next = dst_ptr_reg + " << memAccessSize/8 << ";\n";
			*funcFile << "\t\tsrc_ptr_next = src_ptr_reg + " << memAccessSize/8 << ";\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse if(cur_state == State_Ex ) begin\n";
			*funcFile << "\t\tsize_next = size_reg - " << memBitWidth/8 << ";\n";
			*funcFile << "\t\tdst_ptr_next = dst_ptr_reg + " << memBitWidth/8 << ";\n";
			*funcFile << "\t\tsrc_ptr_next = src_ptr_reg + " << memBitWidth/8 << ";\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse begin\n";
			*funcFile << "\t\tdst_ptr_next = dst_ptr_reg;\n";
			*funcFile << "\t\tsrc_ptr_next = src_ptr_reg;\n";
			*funcFile << "\t\tsize_next = size_reg;\n";
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\taddress_a = 0;\n";
			*funcFile << "\tread_en_a = 0;\n";
			*funcFile << "\twrite_en_a = 0;\n";
			*funcFile << "\tdata_out_a = 0;\n";
			*funcFile << "\tsize_a = 0;\n";
			*funcFile << "\tif(cur_state == State_Op) begin\n";
			*funcFile << "\t\taddress_a = dst_ptr_reg;\n";
			*funcFile << "\t\tread_en_a = 1;\n";
			*funcFile << "\t\twrite_en_a = 1;\n";
			*funcFile << "\t\tdata_out_a = data_in_b;\n";
			*funcFile << "\t\tsize_a = " << memAccessSize/8 << ";\n";
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\taddress_b = 0;\n";
			*funcFile << "\tread_en_b = 0;\n";
			*funcFile << "\twrite_en_b = 0;\n";
			*funcFile << "\tdata_out_b = 0;\n";
			*funcFile << "\tsize_b = 0;\n";
			*funcFile << "\tif(cur_state == State_Op)begin\n";
			*funcFile << "\t\taddress_b = src_ptr_next;\n";
			*funcFile << "\t\tread_en_b = 1;\n";
			*funcFile << "\t\twrite_en_b = 0;\n";
			*funcFile << "\t\tsize_b = " << memAccessSize/8 << ";\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse if(cur_state == State_OpT)begin\n";
			*funcFile << "\t\taddress_b = src_ptr_next;\n";
			*funcFile << "\t\tread_en_b = 1;\n";
			*funcFile << "\t\twrite_en_b = 0;\n";
			*funcFile << "\t\tsize_b = " << memAccessSize/8 << ";\n";
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\taddress_a_ex = 0;\n";
			*funcFile << "\tselect_en_a_ex = 0;\n";
			*funcFile << "\twrite_en_a_ex = 0;\n";
			*funcFile << "\tdata_out_a_ex = 0;\n";
			*funcFile << "\tif(cur_state == State_Ex) begin\n";
			*funcFile << "\t\taddress_a_ex = dst_ptr_reg;\n";
			*funcFile << "\t\tselect_en_a_ex = 1;\n";
			*funcFile << "\t\twrite_en_a_ex = 1;\n";
			*funcFile << "\t\tdata_out_a_ex = data_in_b_ex;\n";
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\taddress_b_ex = 0;\n";
			*funcFile << "\tselect_en_b_ex = 0;\n";
			*funcFile << "\twrite_en_b_ex = 0;\n";
			*funcFile << "\tdata_out_b_ex = 0;\n";
			*funcFile << "\tif(cur_state == State_Ex) begin\n";
			*funcFile << "\t\taddress_b_ex = src_ptr_next;\n";
			*funcFile << "\t\tselect_en_b_ex = 1;\n";
			*funcFile << "\t\twrite_en_b_ex = 0;\n";
			*funcFile << "\tend\n";
			*funcFile << "\telse if(cur_state == State_ExT) begin\n";
			*funcFile << "\t\taddress_b_ex = src_ptr_next;\n";
			*funcFile << "\t\tselect_en_b_ex = 1;\n";
			*funcFile << "\t\twrite_en_b_ex = 0;\n";
			*funcFile << "\tend\n";

			*funcFile << "end\n\n";

			*funcFile << "always @(*) begin\n";
			*funcFile << "\tfinish = 0;\n";
			*funcFile << "\treturn_val = 0;\n";
			*funcFile << "\t\tif(cur_state == State_Finish) begin\n";
			*funcFile << "\t\t\tfinish = 1;\n";
			*funcFile << "\t\t\treturn_val = 0;\n";
			if ( verilogConfigInfo->getBluetoothUse() )
				*funcFile << "\t\t\t$display(\"Function " << func->getName() << " End\");\n";
			*funcFile << "\tend\n";
			*funcFile << "end\n\n";
		}
	}

	//This function may additionally build memory table & RAM
	void PrintVerilog::ramDualPortSetting(void) {

		if ( verilogConfigInfo->getThreadUse() ) {
			//Make All BRAM dual port
			for ( auto ram : memoryTable->getAllRAMList() )
				memoryTable->addDualPortRAM(ram);
		}
		else if ( verilogConfigInfo->getRAMInit() ) { //all single ( sdc schedule based )

		}
		else {
			set<RAM_ *> dualPortRAMs;
			dualPortRAMs.clear();
			errs() << "Dual Port Test \n";

			for ( auto func : targetFuncSet )
			{
				//optimized mem function uses dual ports
				if ( isMemFunction(func) ) {
					errs() << "Function : " << func->getName() << "\n";

					for ( auto ramIter : memoryTable->getAccessRAMFromFunction(func) ) {
						dualPortRAMs.insert(ramIter);

						errs() << "RAM ID : " << ramIter->getRAMId() << "\n";
					}

					continue;
				}

				set<RAM_ *> accessRAMs = memoryTable->getAccessRAMFromFunction(func);
				for ( auto ramIter : accessRAMs )
				{
					//private insts
					for ( auto inst : memoryTable->getPrivateAccessInst(func, ramIter) )
					{
						State *state = fsm->getStateFromInst(inst);
						if ( !state->getLoadPortNum(inst) ) { // b port
							dualPortRAMs.insert(ramIter);

							errs() << "RAM ID : " << ramIter->getRAMId() << "\n";
						}
					}
				}

				//address access insts
				for ( auto inst : memoryTable->getAddressAccessInst(func) )
				{
					State *state = fsm->getStateFromInst(inst);
					if ( !state->getLoadPortNum(inst) )
						for ( auto ramIter : accessRAMs ) {
							dualPortRAMs.insert(ramIter);

							errs() << "RAM ID : " << ramIter->getRAMId() << "\n";
						}
				}
			}

			//build memory table
			for ( auto dualIter : dualPortRAMs )
			{
				errs() << "DUAL PORT RAM : " << dualIter->getRAMId() << "\n";
				memoryTable->addDualPortRAM(dualIter);
			}
		}
	}


	void PrintVerilog::printVerilog(void) {
		targetSetting();

		ramDualPortSetting();

		errs() << "\n@@@@@@@@@@ Generating Function Module @@@@@@@@@@\n";
		functionModuleGeneration();
		
		errs() << "\n@@@@@@@@@@ Generating Memory Module @@@@@@@@@@\n";
		memoryModuleGeneration();

		errs() << "\n@@@@@@@@@@ Generating Operator Module @@@@@@@@@@\n";
		operatorModuleGeneration();

		errs() << "\n@@@@@@@@@@ Generating Top Module @@@@@@@@@@\n";
		topModuleGeneration();
	}
}
