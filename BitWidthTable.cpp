#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/InstrTypes.h"

#include "BitWidthTable.h"

#include <iostream>
#include <fstream>

using namespace llvm;
using namespace std;

namespace corelab
{

static set<Instruction *> checkedInstSet;

static unsigned max(unsigned i, unsigned k) { return i < k ? k : i; }
static unsigned min(unsigned i, unsigned k) { return i < k ? i : k; }

	unsigned BitWidthTableBuilder::getWidthType(Type *ty) {
		if ( IntegerType *iTy = dyn_cast<IntegerType>(ty) )
			return iTy->getBitWidth();
		else if ( isa<PointerType>(ty) )
			return memoryTable->getPointerBitSize();
		else if ( ty->isFloatTy() )
			return 32;
		else if ( ty->isDoubleTy() )
			return 64;
		else if ( ty->isVoidTy() )
			return 0;
		else {
			assert(0);
		}
	}

	unsigned BitWidthTableBuilder::getWidthValue(Function *func, Value *v) {
		errs() << "BITWIDTH TEST : getWidthValue\n";
		v->dump();

		if ( ConstantInt *cInt =  dyn_cast<ConstantInt>(v) ) {
			if ( cInt->isNegative() )
				return getWidthType(v->getType()); //XXX
			else
				return bitWidthTable->getRequiredBitWidth(cInt);
		}
		else if ( Instruction *inst = dyn_cast<Instruction>(v) ) {
			if ( bitWidthTable->existWidth(inst) )
				return bitWidthTable->getWidth(inst);
			else {
				setInstructionBitWidth(func, inst);
				return bitWidthTable->getWidth(inst);
			}
				
		}
		else if ( isa<Argument>(v) ) {
			unsigned argNum = 0;
			bool find = false;
			for ( auto argIter = func->arg_begin(); argIter != func->arg_end(); ++argIter )
			{
				Value *arg = (Value *)(&*argIter);
				assert(arg);
				if ( arg == v ) {
					find = true;
					break;
				}
			}
			assert(find);

			return callTable->getFuncArgBit(func, argNum);
		}
		else if ( isa<PointerType>(v->getType()) ) {
			return memoryTable->getPointerBitSize();
		}
		else {
			v->dump();
			assert(0);
		}
	}


	Instruction *BitWidthTableBuilder::checkInductionVariable( Instruction *phi,
			Instruction *inst, BasicBlock *bb, BasicBlock *exit_bb) {
		errs() << "BITWIDTH TEST : checkInductionVariable\n";
		inst->dump();

		//already checked Instruction
		if ( checkedInstSet.find(inst) != checkedInstSet.end() )
			return NULL;
		else
			checkedInstSet.insert(inst);

		for ( auto ui = inst->user_begin(); ui != inst->user_end(); ++ui )
		{
			Instruction *user = dyn_cast<Instruction>(*ui);
			assert(user);

			if ( isa<PHINode>(user) ||  //prevent recursive
					isa<GetElementPtrInst>(user) || isa<LoadInst>(user) )
				//prevent memory access dependent condition
				continue;

			if ( BranchInst *bInst = dyn_cast<BranchInst>(user) ) {
				//inst is cmpinst
				for ( unsigned i = 0; i < bInst->getNumSuccessors(); ++i )
				{
					BasicBlock *succ = bInst->getSuccessor(i);
					if ( bb == succ ) {
						if ( !isa<CmpInst>(inst) )
							continue;

//						assert(isa<CmpInst>(inst));
//						if ( exit_bb == bInst->getParent() )
						return inst;
					}
				}
			}
			else {
				Instruction *returnInst = checkInductionVariable(phi, user, bb, exit_bb);
				if ( returnInst != NULL )
					return returnInst;
			}
		}
		return NULL;
	}

	Instruction *BitWidthTableBuilder::getCMPInst(PHINode *phi) {
		Function *func = phi->getFunction();
		LoopInfo *li = loopInfoOf[func];
		assert(li);

		BasicBlock *phiBB = phi->getParent();
		assert(phiBB);

		Loop *L = li->getLoopFor(phiBB);
		if ( L ) {
			errs() << L->getName() << "\n";

			LoopNodeAA *LN = paaa->getLoopNode(L);
			if ( LN ) {
				if ( LN->hasSimpleExitCond() ) {
					LN->getExitCondition()->dump();

					if ( LN->hasSimpleCanonical() )
						if ( LN->getInductionVariable() == phi ) {
							return LN->getExitCondition();
						}
				}
				else
					errs() << "Do not have simpel exit cond\n";
			}
			else
				errs() << "Can not find LoopNode\n";
		}
		else
			errs() << "Can not find Loop from BB\n";

		return NULL;
	}

	/*
	bool BitWidthTableBuilder::cyclicCheck(PHINode *phi, Value *op, Instruction *inst) {
		errs() << "BITWIDTH TEST : cyclicCheck\n";
		phi->dump();

		for ( auto ui = inst->user_begin(); ui != inst->user_end(); ++ui )
		{
			Instruction *user = dyn_cast<Instruction>(*ui);
			assert(user);

			if ( user == phi && op == inst )
				return true;
			else if ( isa<PHINode>(user) ||  //prevent recursive
					isa<GetElementPtrInst>(user) || isa<LoadInst>(user) )
				//prevent memory access dependent condition
				continue;
			else {
				bool tester = cyclicCheck(phi, user);
				if ( tester )
					return true;
			}
		}
		return false;
	}*/

	void BitWidthTableBuilder::handlePHINode(PHINode *phi) {
		errs() << "BITWIDTH TEST : handlePHINode\n";
		phi->dump();

		bool dontHaveDefinedOperands = false;
		unsigned bitWidthOfPHI = 0;
		for ( unsigned i = 0; i < phi->getNumIncomingValues(); i++ )
		{
			Value *v = phi->getIncomingValue(i);
			if ( ConstantInt *c = dyn_cast<ConstantInt>(v) ) {
				unsigned possibleWidth = bitWidthTable->getRequiredBitWidth(c);
				if ( bitWidthOfPHI < possibleWidth )
					bitWidthOfPHI = possibleWidth;
			}
			else if ( Instruction *opInst = dyn_cast<Instruction>(v) ) {
				if ( bitWidthTable->existWidth(opInst) ) {
					unsigned possibleWidth = bitWidthTable->getWidth(opInst);
					if ( bitWidthOfPHI < possibleWidth )
						bitWidthOfPHI = possibleWidth;
				}
				else if ( isa<LoadInst>(opInst) ) {
					unsigned possibleWidth = getWidthType(opInst->getType());
					if ( bitWidthOfPHI < possibleWidth )
						bitWidthOfPHI = possibleWidth;
				}
				else
					dontHaveDefinedOperands = true;
			}
			else
				dontHaveDefinedOperands = true;
		}

		errs() << "dontHaveDefinedOperands : " << dontHaveDefinedOperands << "\n";

		if ( !dontHaveDefinedOperands ) {
			assert(bitWidthOfPHI!=0);
			bitWidthTable->addWidth(phi,bitWidthOfPHI);
		}
		else {
			bool tester = false;////

			checkedInstSet.clear();
			Instruction *cmpInst = 
				checkInductionVariable(phi, phi, phi->getParent(), phi->getParent());
				//Heuristic way can not find
			if ( cmpInst == NULL ) {
				//LoopAA way
				cmpInst = getCMPInst(phi);
				if ( cmpInst != NULL ) {////
					tester = true;
				}
			}

				//Finally Fail
			if ( cmpInst == NULL ) {
				bitWidthTable->addWidth(phi, getWidthType(phi->getType()));
				errs() << "can not find cmp inst : \n";
			}
			else {
				// this is induction
				unsigned i0=0,i1=0;
				// comparison value const or not
				for ( unsigned i = 0; i < cmpInst->getNumOperands(); i++ )
				{
					Value *v = cmpInst->getOperand(i);
					if ( Constant *c = dyn_cast<Constant>(v) )
						i0 = bitWidthTable->getRequiredBitWidth(c);
					else if ( Instruction *opInst = dyn_cast<Instruction>(v) ) {
						if ( bitWidthTable->existWidth(opInst) )
							i0 = bitWidthTable->getWidth(opInst);
					}
				}

				// incoming value const or not
				for ( unsigned i = 0; i < phi->getNumIncomingValues(); i++ )
				{
					Value *v = phi->getIncomingValue(i);
					if ( Constant *c =  dyn_cast<Constant>(v) )
						i1 = bitWidthTable->getRequiredBitWidth(c);
					else if ( Instruction *opInst = dyn_cast<Instruction>(v) ) {
						if ( bitWidthTable->existWidth(opInst) )
							i1 = bitWidthTable->getWidth(opInst);
					}
				}

				if ( (i0==0 || i1==0) || (getWidthType(phi->getType()) < (max(i0,i1)+1)) )
					bitWidthTable->addWidth(phi, getWidthType(phi->getType()));//fail to find 
				else {
					errs() << "BitWidth PHI Succ : \n";
					phi->dump();
					errs() << " : " << max(i0,i1) << "+1\n\n";
					bitWidthTable->addWidth(phi, max(i0,i1) + 1);//changed because of ashr/sgt condition

					errs() << "CmpInst :\n";
					cmpInst->dump();

					if ( tester ) {
						errs() << "Loop Based Win : \n";
						errs() << "origin : " << getWidthType(phi->getType()) << "\n";
						errs() << "opt : " << max(i0,i1) << "+1\n\n";
					}

				}
			}
		}
	}


	BitWidthTableBuilder::BitWidthTableBuilder(
			Module *module_, MemoryTable *memoryTable_, CallTable *callTable_,
				DenseMap<const Function *, LoopInfo *> loopInfoOf_, 
				PADriver *pa_, bool bitWidthOff_, Function *accelFunction_) 
		: module(module_), memoryTable(memoryTable_), callTable(callTable_),
		loopInfoOf(loopInfoOf_), pa(pa_), bitWidthOff(bitWidthOff_), accelFunction(accelFunction_)
	{
		//Analysis Initialize
		paaa = new PABasedAA(module, pa, loopInfoOf);
		paaa->initLoopAA();

		//Table Allocation
		bitWidthTable = new BitWidthTable(memoryTable->getPointerBitSize());

		//Signed Checker
		setSignedInstruction();

		//Build
//		searchAllInst();
		setBitWidthFromFunction();


		//Print Operator Table Debug File
		std::error_code ec_print = std::make_error_code(std::errc::io_error);
		raw_fd_ostream widthFile("bitWidthTable.debug", ec_print, llvm::sys::fs::F_None);

		uint64_t totalBitWidth = 0;
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;
					unsigned bitWidth = bitWidthTable->getWidth(inst);

					widthFile << *inst << " : " << bitWidth << "\n\n";

					totalBitWidth += bitWidth;
				}
		widthFile << "total bit width : " << totalBitWidth << "\n\n\n";

		unsigned totalSigned = 0;
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;
					bool sign = bitWidthTable->getSigned(inst);

					widthFile << *inst << " : " << sign<< "\n\n";

					if ( sign )
						totalSigned++;
				}
		widthFile << "total signed : " << totalSigned << "\n\n\n";
	}

	void BitWidthTableBuilder::setInstructionBitWidth(Function *func, Instruction *inst) {
		errs() << "BITWIDTH TEST : setInstructionBitWidth\n";
		inst->dump();

		if ( isa<AllocaInst>(inst) ||
				isa<GetElementPtrInst>(inst) ||
				isa<BitCastInst>(inst) ||
				isa<SwitchInst>(inst) ) {
			bitWidthTable->addWidth(inst, memoryTable->getPointerBitSize());
		}
		else if ( CallInst *callInst = dyn_cast<CallInst>(inst) ) {
			Function *calledF = callInst->getCalledFunction();
			assert(calledF);
			//CallInstruction BitWidth Setting
			if ( func2done[calledF] ) {
				unsigned bitWidth = callTable->getFuncReturnBit(calledF);
				bitWidthTable->addWidth(inst, bitWidth);
			}
			else {
				Type *returnTy = calledF->getReturnType();
				bitWidthTable->addWidth(inst, getWidthType(returnTy));
			}

			//Callee Function Argument BitWidth Setting
			for ( unsigned i = 0; i < callInst->getNumArgOperands(); i++ )
			{
				Value *operand = callInst->getArgOperand(i);
				assert(operand);

				errs() << "call:" << i << "\n";
				unsigned bitWidth = getWidthValue(func, operand);
				callTable->setFuncArgBit(calledF, i, bitWidth);
				callTable->setMaxArgBitWidth(i, bitWidth);
			}
		}
		else if ( LoadInst *lInst = dyn_cast<LoadInst>(inst) ) {
			bitWidthTable->addWidth(inst, getWidthType(lInst->getType()));
		}
		else if ( isa<PtrToIntInst>(inst)) {
			bitWidthTable->addWidth(inst, getWidthType(inst->getType()));
		}
		else if ( isa<TruncInst>(inst) ) {
			unsigned i0 = getWidthValue(func, inst->getOperand(0));
			unsigned inst_width = getWidthType(inst->getType());

			bitWidthTable->addWidth(inst, min(i0,inst_width));
		}
		else if ( isa<SelectInst>(inst) ) {
			unsigned i0 = getWidthValue(func, inst->getOperand(1));
			unsigned i1 = getWidthValue(func, inst->getOperand(2));

			bitWidthTable->addWidth(inst, max(i0,i1));
		}
		else if ( isa<ZExtInst>(inst) ) {
			bitWidthTable->addWidth(inst, getWidthValue(func, inst->getOperand(0)));
		}
		else if ( isa<SExtInst>(inst)) {
			bitWidthTable->addWidth(inst, getWidthValue(func, inst->getOperand(0)));
//			bitWidthTable->addWidth(inst, getWidthType(inst->getType()));
		}
		else if ( PHINode *phi = dyn_cast<PHINode>(inst) ) {
			handlePHINode(phi);
		}
		else { //binary
			if ( inst->getOpcode() == Instruction::Add ) {
				//added
				if ( inst->hasNoUnsignedWrap() || inst->hasNoSignedWrap() ) 
					bitWidthTable->addWidth(inst, getWidthType(inst->getType()));
				else {
					unsigned i0 = getWidthValue(func, inst->getOperand(0));
					unsigned i1 = getWidthValue(func, inst->getOperand(1));
					unsigned max_value = max(i0,i1);

					bitWidthTable->addWidth(inst, min(max_value+1, getWidthType(inst->getType())));//changed
				}
			}
			else if ( inst->getOpcode() == Instruction::Sub ) {
				if ( inst->hasNoUnsignedWrap() || inst->hasNoSignedWrap() ) 
					bitWidthTable->addWidth(inst, getWidthType(inst->getType()));
				else {
					unsigned i0 = getWidthValue(func, inst->getOperand(0));
					unsigned i1 = getWidthValue(func, inst->getOperand(1));
					unsigned max_value = max(i0,i1);

					bitWidthTable->addWidth(inst, min(max_value+1, getWidthType(inst->getType())));//changed
				}
			}
			else if ( inst->getOpcode() == Instruction::Or ||
					inst->getOpcode() == Instruction::Xor ) {
				unsigned i0 = getWidthValue(func, inst->getOperand(0));
				unsigned i1 = getWidthValue(func, inst->getOperand(1));
				unsigned max_value = max(i0,i1);

				bitWidthTable->addWidth(inst, max_value);
			}
			else if ( inst->getOpcode() == Instruction::And ) {
				unsigned i0 = getWidthValue(func, inst->getOperand(0));
				unsigned i1 = getWidthValue(func, inst->getOperand(1));

				bool signedInst = bitWidthTable->getSigned(inst);

				if ( (isa<Constant>(inst->getOperand(0)) && i0 < i1) ||
						(isa<Constant>(inst->getOperand(1)) && i0 > i1) ) //XXX
					bitWidthTable->addWidth(inst, signedInst ? min(i0,i1)+1 : min(i0,i1) ); //signed? +1 : 0
//					bitWidthTable->addWidth(inst, min(i0,i1));
//					bitWidthTable->addWidth(inst, min( min(i0,i1)+1, getWidthType(inst->getType()) ) ); //signed? +1 : 0
				else
					bitWidthTable->addWidth(inst, signedInst ? max(i0,i1)+1 : max(i0,i1) ); //signed? +1 : 0
//					bitWidthTable->addWidth(inst, max(i0,i1));
//					bitWidthTable->addWidth(inst, min( max(i0,i1)+1, getWidthType(inst->getType()) ) );
			}
			else if ( inst->getOpcode() == Instruction::Shl ) {
				unsigned i0 = getWidthValue(func, inst->getOperand(0));

				if ( ConstantInt *cInt = dyn_cast<ConstantInt>(inst->getOperand(1)) ) {
					unsigned i1 = cInt->getZExtValue();

					bitWidthTable->addWidth(inst, min(i0+i1, getWidthType(inst->getType())));
				}
				else
					bitWidthTable->addWidth(inst, getWidthType(inst->getType()));
			}
			else if ( inst->getOpcode() == Instruction::Mul ) {
//				if ( inst->hasNoUnsignedWrap() || inst->hasNoSignedWrap() ) 
//					bitWidthTable->addWidth(inst, getWidthType(inst->getType()));
//				else {
					unsigned i0 = getWidthValue(func, inst->getOperand(0));
					unsigned i1 = getWidthValue(func, inst->getOperand(1));

					bitWidthTable->addWidth(inst, min(i0+i1, getWidthType(inst->getType())));
//				}
			}
			else if ( inst->getOpcode() == Instruction::LShr ) {
				unsigned i0 = getWidthValue(func, inst->getOperand(0));

				if ( ConstantInt *cInt = dyn_cast<ConstantInt>(inst->getOperand(1)) ) {
					unsigned i1 = cInt->getZExtValue();

					if ( i0 < i1 || i0 == i1 )
						bitWidthTable->addWidth(inst, 1);
					else
						bitWidthTable->addWidth(inst, i0-i1);
				}
				else
					bitWidthTable->addWidth(inst, i0);
			}
			else if (	inst->getOpcode() == Instruction::UDiv ||
					inst->getOpcode() == Instruction::URem ) {
				unsigned i0 = getWidthValue(func, inst->getOperand(0));

				bitWidthTable->addWidth(inst, i0);
			}
			else if ( inst->getOpcode() == Instruction::AShr ||
					inst->getOpcode() == Instruction::SDiv||
					inst->getOpcode() == Instruction::SRem ) {

				bitWidthTable->addWidth(inst, getWidthType(inst->getType()));
			}
			else if ( isa<ICmpInst>(inst) ) {
				bitWidthTable->addWidth(inst, 1);
			}
			else if ( isa<BranchInst>(inst) || isa<StoreInst>(inst) ) {
				bitWidthTable->addWidth(inst, 0);
			}
			else if ( ReturnInst *rInst = dyn_cast<ReturnInst>(inst) ) { 
				bitWidthTable->addWidth(inst, 0);
				Value *operand = rInst->getReturnValue();
				if ( operand ) {
				errs() << "2\n";
					unsigned bitWidth = getWidthValue(func, operand);	
					callTable->setFuncReturnBit(func, bitWidth);
					callTable->setMaxBitWidth(bitWidth);
				}
				else {//voidty return
					callTable->setFuncReturnBit(func, 1);
					callTable->setMaxBitWidth(1);
				}
			}
			else {
				inst->dump();
				assert(0);
			}

		}

	}

	/*
	void BitWidthTableBuilder::searchAllInst() {
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;


					if ( !bitWidthOff )
						setInstructionBitWidth(inst);
					else {
						if ( isa<AllocaInst>(inst) ||
								isa<GetElementPtrInst>(inst) ||
								isa<BitCastInst>(inst) ) {
							bitWidthTable->addWidth(inst, memoryTable->getPointerBitSize());
						}
						else if ( isa<ICmpInst>(inst) ) {
							bitWidthTable->addWidth(inst, 1);
						}
						else if ( isa<BranchInst>(inst) || isa<ReturnInst>(inst) || isa<StoreInst>(inst) ) {
							bitWidthTable->addWidth(inst, 0);
						}
						else {
							bitWidthTable->addWidth(inst, getWidthType(inst->getType()));
						}
					}
				}
	}*/

	void BitWidthTableBuilder::setFunctionArgBitWidth_Top(Function *func) {
		unsigned argNum = 0;
		for ( auto argIter = func->arg_begin(); argIter != func->arg_end(); ++argIter )
		{
			Value *arg = (Value *)(&*argIter);
			assert(arg);

			Type *argType = arg->getType();
			if ( IntegerType *intTy = dyn_cast<IntegerType>(argType) ) {
				callTable->setFuncArgBit(func, argNum, intTy->getBitWidth()); 
				callTable->setMaxArgBitWidth(argNum, intTy->getBitWidth());
			}
			else if ( isa<PointerType>(argType) ) {
				callTable->setFuncArgBit(func, argNum, memoryTable->getPointerBitSize()); 
				callTable->setMaxArgBitWidth(argNum, memoryTable->getPointerBitSize());
			}
			else if ( argType->isVoidTy() )
				callTable->setFuncArgBit(func, argNum, 1); 

			argNum++;
		}
	}

	void BitWidthTableBuilder::setBitWidthFunction(Function *func) {
		for ( auto bi = func->begin(); bi != func->end(); bi++ )
			for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
			{
				Instruction *inst = &*ii;

				if ( !bitWidthOff )
					setInstructionBitWidth(func, inst);
				else {
					if ( isa<AllocaInst>(inst) ||
							isa<GetElementPtrInst>(inst) ||
							isa<BitCastInst>(inst) ) {
						bitWidthTable->addWidth(inst, memoryTable->getPointerBitSize());
					}
					else if ( isa<ICmpInst>(inst) ) {
						bitWidthTable->addWidth(inst, 1);
					}
					else if ( isa<BranchInst>(inst) || isa<ReturnInst>(inst) || isa<StoreInst>(inst) ) {
						bitWidthTable->addWidth(inst, 0);
					}
					else {
						bitWidthTable->addWidth(inst, getWidthType(inst->getType()));
					}
				}
			}
	}

	void BitWidthTableBuilder::setBitWidthFromFunction() {
		func2done.clear();
		seed = 1;

		//target Setting
		targetFuncSet.clear();
		Function *topFunction;
		if ( accelFunction )
			topFunction = accelFunction;
		else
			topFunction = module->getFunction("main");
		targetFuncSetting(topFunction);

		setFunctionArgBitWidth_Top(topFunction);
		setBitWidthFunction(topFunction);
		func2done[topFunction] = true;

		set<Function *> nextFunctionSet = callTable->getCalleeFromFunction(topFunction);
		Function *nextFunction;
		if ( nextFunctionSet.size() == 0 )
			nextFunction = NULL;
		else
			nextFunction = *nextFunctionSet.begin();

		while(1) {
			if ( nextFunction == NULL )
				break;

			errs() << "Try to Set BitWidth Function : " << nextFunction->getName() << "\n";

			//check parents all done
			bool parentDone = true;
			for ( auto caller : callTable->getCallerFromFunction(nextFunction) )
				if ( !func2done[caller] )
					parentDone = false;

			//define bitwidth
			if ( parentDone ) {
				setBitWidthFunction(nextFunction);
				func2done[nextFunction] = true;
			}

			//find next
			nextFunction = getNextFunction();
		}

		//BitWidthOff ( need to define function arg/return )
		if ( bitWidthOff ) {
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
					if ( IntegerType *intType = dyn_cast<IntegerType>(argType) ) {
						callTable->setFuncArgBit(func, argNum, intType->getBitWidth());
						callTable->setMaxArgBitWidth(argNum, intType->getBitWidth());
					}
					else if ( isa<PointerType>(argType) ) {
						callTable->setFuncArgBit(func, argNum, memoryTable->getPointerBitSize()); 
						callTable->setMaxArgBitWidth(argNum, memoryTable->getPointerBitSize());
					}
					else if ( argType->isVoidTy() )
						callTable->setFuncArgBit(func, argNum, 1); 

					argNum++;
				}

				Type *returnType = func->getReturnType();
				if ( IntegerType *intType = dyn_cast<IntegerType>(returnType) ) {
					callTable->setFuncReturnBit(func, intType->getBitWidth());
					callTable->setMaxBitWidth(intType->getBitWidth());
				}
				else if ( isa<PointerType>(returnType) ) {
					callTable->setFuncReturnBit(func, memoryTable->getPointerBitSize());
					callTable->setMaxBitWidth(memoryTable->getPointerBitSize()); 
				}
				else if ( returnType->isVoidTy() )
					callTable->setFuncReturnBit(func, 1);
			}
		}

	}

	void BitWidthTableBuilder::searchSignedUser(Instruction *inst) {
		errs() << "searchSignedUser : " << *inst << "\n\n";
		for ( auto ui = inst->user_begin(); ui != inst->user_end(); ++ui )
		{
			Instruction *user = dyn_cast<Instruction>(*ui);
			assert(user);

			if ( bitWidthTable->getSigned(user) ) {

			}
			else if ( isa<PHINode>(user) ) {
				bitWidthTable->addSigned(user, true);
				searchSignedUser(user);
			}
			else if ( isa<SelectInst>(user) && (user->getOperand(0) != inst) ) {
				bitWidthTable->addSigned(user, true);
				searchSignedUser(user);
			}
			else if ( isa<ZExtInst>(user) ) { //last point
			}
			else if ( isa<SExtInst>(user) ) { //already done
			}
			else {//binary operations
				if ( user->getOpcode() == Instruction::Add ||
						user->getOpcode() == Instruction::Sub || //XXX
						user->getOpcode() == Instruction::Or ||
						user->getOpcode() == Instruction::And ||
						user->getOpcode() == Instruction::Shl ||
						user->getOpcode() == Instruction::Mul ||
						user->getOpcode() == Instruction::AShr ||
						user->getOpcode() == Instruction::SDiv ||
						user->getOpcode() == Instruction::SRem 
					 ) {
					//LShr ? UDiv? URem?
					bitWidthTable->addSigned(user, true);
					searchSignedUser(user);
				}
			}
		}
	}

	void BitWidthTableBuilder::setSignedInstruction() {
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;

					if ( isa<SExtInst>(inst) ) {
						bitWidthTable->addSigned(inst, true);

						searchSignedUser(inst);
					}
					//added
					/*
					else if ( inst->getOpcode() == Instruction::AShr ||
							inst->getOpcode() == Instruction::Sub ) {
//						bitWidthTable->addSigned(inst, true);

						searchSignedUser(inst);
					}
					else if ( inst->getOpcode() == Instruction::Add ) {
						Value *operand1 = inst->getOperand(1);
						if ( ConstantInt *cInt = dyn_cast<ConstantInt>(operand1) )
							if ( cInt->isNegative() ) {
//								bitWidthTable->addSigned(inst, true);

								searchSignedUser(inst);			
							}
					}*/

					/*
					else if ( inst->getOpcode() == Instruction::Sub ||
							inst->getOpcode() == Instruction::Mul ) {
						bitWidthTable->addSigned(inst, true);

						searchSignedUser(inst);
					}*/
					//ashr? SDiv? SRem?
				}
	}

}
