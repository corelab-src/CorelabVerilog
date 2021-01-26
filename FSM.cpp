#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"

#include "FSM.h"

#include <system_error>
#include <math.h>

#define DEBUG_FSM

using namespace llvm;

namespace corelab {

	static unsigned sNum;

	static void initStateNumber(void) { sNum = 0; }

	static unsigned genStateNumber(void)
	{
		sNum++;
		return sNum;
	}

	static unsigned nNum;

	static void initNoNameNumber(void) { nNum = 0; }

	static unsigned genNoNameInstNum(void)
	{
		nNum++;
		return nNum;
	}

	void FSM::printFSM(void)
	{
		DenseMap<Function *, unsigned> fnc2ClusterNum;

		std::error_code ec_print = std::make_error_code(std::errc::io_error);
		raw_fd_ostream fsmStructureFile("fsm_structure.dot", ec_print, llvm::sys::fs::F_None);

		fsmStructureFile << "digraph G {\n";

		fsmStructureFile << "graph [fontsize=10 fontname=\"Verdana\" compound=true];\n";
		fsmStructureFile << "node [shape=record fontname=\"Verdana\" fontsize=6];\n";
		fsmStructureFile << "ratio=\"fill\";\n";
		fsmStructureFile << "rankdir=LR;\n";
		fsmStructureFile << "ranksep=1;\n\n";

		fsmStructureFile << "\"Start\"[fontsize=10 fontname=\"Verdana\" color=blue shape=record];\n";
		fsmStructureFile << "\"End\"[fontsize=10 fontname=\"Verdana\" color=blue shape=record];\n";

		for ( auto fncIter : fnc2StartState )
		{
			Function *fncTmp = fncIter.first;
			fnc2ClusterNum[fncTmp] = genStateNumber();
			fsmStructureFile << "subgraph cluster_" << fnc2ClusterNum[fncTmp] << " {\n";
			fsmStructureFile << "label = \"" << fncTmp->getName() << "\";\n}\n";
		}
		fsmStructureFile << "\n";

		for ( auto stateIter : stateList )
		{
			unsigned num = stateIter->getStateNumber();
			fsmStructureFile << "subgraph cluster_" << fnc2ClusterNum[stateIter->getParentFunction()] 
				<< " {\n";
			fsmStructureFile << "subgraph cluster_" << num << " {\n";
			fsmStructureFile << "label=" << "\"" << num << "\";\n";
			fsmStructureFile << "\"" << num << "\";\n";								//for cluster edge
//				for ( auto instIter : stateIter->getInstList() )
//					fsmStructureFile << "\"" << *instIter << "\";\n";
			for ( auto schIter : stateIter->getScheduleMap() )
				for ( auto instIter : schIter.second )
					fsmStructureFile << "\"" << schIter.first << " : " << *instIter << "\";\n";
			fsmStructureFile << "}\t}\n";
		}

		for ( auto stateIter : stateList )
		{
			unsigned num = stateIter->getStateNumber();
			PrevNextState prevList = stateIter->getPrev();
			PrevNextState nextList = stateIter->getNext();
			for ( auto prevIter : prevList )
			{
				if ( prevIter->getExitInst() )
				{
					unsigned prevNum = prevIter->getStateNumber();
					fsmStructureFile << "\"" << prevNum << "\" -> \"" << num << "\" ["
						<< "ltail=cluster_" << prevNum  << ", "
						<< "lhead=cluster_" << num << ", "
						<< "label=\"" << *(prevIter->getExitInst()) << "\"];\n";
				}
			}
		/*	
			for ( auto nextIter : nextList )
			{
				if ( nextIter->getEnterInst() )
				{
					unsigned nextNum = nextIter->getStateNumber();
					fsmStructureFile << "\"" << num << "\" -> \"" << nextNum << "\" ["
						<< "ltail=cluster_" << num  << ", "
						<< "lhead=cluster_" << nextNum << ", "
						<< "label=\"" << *(nextIter->getEnterInst()) << "\"];\n";
					break;
				}
			}*/
		}
		errs() << "done\n";

//		fsmStructureFile << "\"Start\" -> \"" << startState->getStateNumber() << "\";\n";
//		fsmStructureFile << "\"" << endState->getStateNumber() << "\" -> \"End\";";
	
		fsmStructureFile << "}"; // digraph G end
	}

	void FSMBuilder::setFSM(FSM *fsm)
	{
		bool timeToStart = true;				//New State
		bool firstInstruction = true;		//New Function
		State *currentState;

//		Function *accelF = verilogConfigInfo->getAccelFunction();

//		for ( auto fi : callTable->getCalleeFromFunction(accelF) )
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
		{
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ ) {
				if ( verilogConfigInfo->getPipeline() )
					if ( lpa->isBBForPipeline(&*bi) ) //TODO: todo preheader case
						if ( lpa->getLoopAFromBB(&*bi)->getLoopType() != LoopAliasInfo::PREHEADER ) {
							Instruction *bInst = &*(--(&*bi)->end());

							//Check the existing Pipeline State
							PipelineState *pState = NULL;
							for ( auto pStateIter : fsm->getPipelineStateList() )
								if ( pStateIter->getLA() == lpa->getLoopAFromBB(&*bi) ) {
									pState = pStateIter;
									break;
								}

							//If exist, get the state & add basicblock
							// and if there is no exitInst, then find the exit ( conditional )
							if ( pState != NULL ) {
								pState->addBasicBlock(&*bi);
								if ( dyn_cast<BranchInst>(bInst)->isConditional() )
									pState->setExitInst(bInst);
							}
							else {
								//If not exist, create it
								PipelineState *state = new PipelineState(&*fi, genStateNumber(), 
										lpa->getLoopAFromBB(&*bi));
								//for basic FSM Style
								fsm->pushState2List(state);
								fsm->setFnc2StateList(&*fi, state);
								if ( dyn_cast<BranchInst>(bInst)->isConditional() )
									state->setExitInst(bInst);

								//for pipeline
								fsm->pushPipelineState2List(state);
								fsm->setState2Pipeline(dynamic_cast<State *>(state), state);
								state->setPipelineState(true);

								state->addBasicBlock(&*bi);
							}
							continue;
						}

				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;

					if ( timeToStart )
					{
						State *state = new State(&*fi, genStateNumber());
						currentState = state;
						fsm->pushState2List(currentState);
						fsm->setFnc2StateList(&*fi, currentState);
						timeToStart = false;
					}

					if ( firstInstruction )
					{
						fsm->setFnc2StartState(&*fi, currentState);
						if ( (&*fi) == module->getFunction("main") )
							fsm->setStartState(currentState);
						firstInstruction = false;
					}

					// These instructions are not in instList
					// Can not handle All Terminator Instructions Now
					if ( isa<ReturnInst>(inst) || isa<BranchInst>(inst) )
					{
						currentState->setExitInst(inst);
						if ( (&*fi) == module->getFunction("main") )
							fsm->setEndState(currentState); //XXX: we dont use it
						timeToStart = true;
					}
					else if ( isa<InvokeInst>(inst) )
					{
						Value *fcnPtr = dyn_cast<InvokeInst>(inst)->getCalledValue();
						assert(0 && "invoke inst has no ptr value\n");

						currentState->setExitInst(inst);
						timeToStart = true;
					}
					else if ( isa<CallInst>(inst) )
					{
						Function *fnc = dyn_cast<CallInst>(inst)->getCalledFunction();
						if ( fnc == NULL ) // indirectCall or declaration
							currentState->pushInst2List(inst);
						else if ( getMetadata(inst) == 1 ) {
							currentState->addParallelCallSite(dyn_cast<CallInst>(inst));
							currentState->pushInst2List(inst);

							errs() << "Parallel CallSite : ";
							inst->dump();
							errs() << "\n";
						}
						else
						{
							currentState->setExitInst(inst);
							if ( (&*fi) == module->getFunction("main") )
								fsm->setEndState(currentState); //XXX:we don't use it now
							timeToStart = true;
						}

					}
					else if ( isa<SwitchInst>(inst) )
					{
						currentState->setExitInst(inst);
						timeToStart = true;
					}
					else
						currentState->pushInst2List(inst);

					fsm->setInst2StateMap(inst, currentState);
				}
			}
			firstInstruction = true;
		}

		//Instruction Insertion to Pipeline State
		for ( auto pState : fsm->getPipelineStateList() )
			for ( auto bb : pState->getBB() )
				for ( auto ii = bb->begin(); ii != bb->end(); ++ii ) {
					Instruction *inst = &*ii;
					if ( !isa<BranchInst>(inst) )
						pState->pushInst2List(inst);
					fsm->setInst2StateMap(inst, pState);
				}
	

		//DEBUG
		for ( auto iter : fsm->getPipelineStateList() )
		{
			list<BasicBlock *> bbL = iter->getLA()->getBB();
			list<BasicBlock *> bbS = iter->getBB();

			errs() << " FSM : " << bbL.size() << "\n";
			errs() << " FSM : " << bbS.size() << "\n";
			if ( bbL.size() != bbS.size() )
				assert(0 && "FSM : pstate != LA");

			for ( auto bb : bbS )
			{
				bool find = false;
				for ( auto bbb : bbL )
					if ( bbb == bb ) {
						find = true;
						break;
					}

				if ( !find )
					assert(0 && "FSM : No bb");
			}
		}

	}//setFSM end

	void FSMBuilder::connectStates(FSM *fsm)
	{
		for ( auto stateIter : fsm->getStateList() )
		{
			Instruction *exitI = stateIter->getExitInst();
			if ( exitI == NULL )
				for ( auto instIter : stateIter->getInstList() )
					instIter->dump();
			
			assert(exitI);

			if ( isa<CallInst>(exitI) )
			{
				Function *fnc = dyn_cast<CallInst>(exitI)->getCalledFunction();
				assert(fnc && " Indirect Call ");
				if ( fnc->isDeclaration() )
					assert(0 && "declaration function in FSM");

				State *nextS = fsm->getStartStateFromFnc(fnc);
				assert(nextS && "can't get start State from Function");
				stateIter->addNext(nextS);
				nextS->addPrev(stateIter);
				nextS->setEnterInst(exitI);

				//return state setting
				Instruction *nextInstruction;
				for ( auto ii = (exitI->getParent())->begin();
						ii != (exitI->getParent())->end(); ii++ )
					if ( (&*ii) == exitI )
					{
						ii++;
						nextInstruction = (&*ii);
						break;
					}
				assert(nextInstruction);
				State *returnState = fsm->getStateFromInst(nextInstruction);
				stateIter->addNext(returnState);
				
				Instruction *enterInstOfReturnState;
				for ( auto callStates : fsm->getStateListOfFunction(fnc) )
					if ( Instruction *callStateExitI = callStates->getExitInst() )
						if ( ReturnInst *reInst = dyn_cast<ReturnInst>(callStateExitI) )
							enterInstOfReturnState = reInst;
				assert(enterInstOfReturnState);
				returnState->addPrev(fsm->getStateFromInst(enterInstOfReturnState));
				returnState->setEnterInst(enterInstOfReturnState);
				
			}//CallInst end
			else if ( isa<InvokeInst>(exitI) )
			{
				//return state setting
				Instruction *nextInstruction;
				for ( auto ii = (exitI->getParent())->begin();
						ii != (exitI->getParent())->end(); ii++ )
					if ( (&*ii) == exitI )
					{
						ii++;
						nextInstruction = (&*ii);
						break;
					}
				assert(nextInstruction);
				State *returnState = fsm->getStateFromInst(nextInstruction);
				stateIter->addNext(returnState);
			}
			else if ( isa<BranchInst>(exitI) )
			{
				unsigned numSucc = dyn_cast<BranchInst>(exitI)->getNumSuccessors();
				for ( unsigned i = 0; i < numSucc; i++ )
				{
					BasicBlock *bb = dyn_cast<BranchInst>(exitI)->getSuccessor(i);
					assert(bb);
					Instruction *firstI = &*(bb->begin());
					assert(firstI);

					//If this state is for pipeline state
					//this state may have more than two bb
					//back edge in this state will be managed through stages
					//connect to exit bb only

//					if ( stateIter->isPipelineState() &&
//							lpa->isBBForPipeline(bb) )
//						continue;

					State *nextS = fsm->getStateFromInst(firstI);
					assert(nextS && "can't get state from Instruction");
					stateIter->addNext(nextS);
					nextS->addPrev(stateIter);
					nextS->setEnterInst(exitI);
				}
			}//BranchInst end
			else if ( isa<SwitchInst>(exitI) )
			{
				//XXX : 
				//Condition Value of Switch Instruction is determined dynamically
				//So, We can not set some map container statically.
				//It will be printed out in PrintVerilog Class Method
			}
			else if ( isa<ReturnInst>(exitI) )
			{
				BasicBlock *bb = exitI->getParent();
				Function *fnc = bb->getParent();

				//TODO:Main Function Enter & Return

				// No Need to connect across function module
				/*
				for ( auto fi = module->begin(); fi != module->end(); fi++ )
					for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
						for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
							if ( isa<CallInst>(&*ii) )
							{
								//declaration becomes null ptr
								//indirectcall also becomes null ptr
								//if we want to find in-directcall then we should use <InvokeInst>
								Function *callF = dyn_cast<CallInst>(&*ii)->getCalledFunction();

								if ( callF == fnc )
								{
									auto ii2 = ii++;
									ii--;
									Instruction *nextI = &*ii2;
									assert(nextI);

									State *nextS = fsm->getStateFromInst(nextI);
									assert(nextS && "can't get state from Instruction after ret");
									stateIter->addNext(nextS);
									nextS->addPrev(stateIter);
									nextS->setEnterInst(exitI);
								}
							}
				*/
			}//ReturnInst end
			else
				assert(0);
		}
	}//connectFSM end

	void FSMBuilder::portAssignment(FSM *fsm, list<Instruction *> targetList, bool unresolved,
			Function *func) {
		DenseMap<unsigned, DenseMap<unsigned, list<Instruction *>>> timingMap;
		DenseMap<unsigned, unsigned> maxMap;
		timingMap.clear();
		maxMap.clear();

		//<StateNum, <ScheduleNum, pre-occupied Num>>
		DenseMap<unsigned, DenseMap<unsigned, unsigned>> occupiedMap;
		DenseMap<unsigned, DenseMap<unsigned, unsigned>> occupiedMap_ram;
		occupiedMap.clear();
		occupiedMap_ram.clear();

		for ( auto inst : targetList )
		{
			unsigned stateNumber = (fsm->getStateFromInst(inst))->getStateNumber();
			unsigned sch = (fsm->getStateFromInst(inst))->getSchFromInst(inst);
			(timingMap[stateNumber])[sch].push_back(inst);
			if ( maxMap[stateNumber] < sch )
				maxMap[stateNumber] = sch;
		}

		if ( unresolved ) {
			for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			{
				occupiedMap_ram.clear();
				if ( (memoryTable->getPrivateAccessInst(func, ram)).size() != 0 )
					for ( auto inst : memoryTable->getPrivateAccessInst(func, ram) )
					{
						unsigned stateNumber = (fsm->getStateFromInst(inst))->getStateNumber();
						unsigned sch = (fsm->getStateFromInst(inst))->getSchFromInst(inst);	
						if ( occupiedMap_ram.count(stateNumber) ) {
							if ( (occupiedMap_ram[stateNumber]).count(sch) ) {
								unsigned occupiedNum = (occupiedMap_ram[stateNumber])[sch];
								assert( occupiedNum < 2 );
								(occupiedMap_ram[stateNumber])[sch] = occupiedNum + 1;
							}
							else
								(occupiedMap_ram[stateNumber])[sch] = 1;
						}
						else
							(occupiedMap_ram[stateNumber])[sch] = 1;
					}

				//update on occupiedMap
				for ( auto occuIter : occupiedMap_ram )
				{
					unsigned stateNumber = occuIter.first;
					for ( auto schIter : occuIter.second )
					{
						unsigned schNumber = schIter.first;
						unsigned occuNumber = schIter.second;

						if ( occupiedMap.count(stateNumber) ) {
							if ( (occupiedMap[stateNumber]).count(schNumber) ) {
								if ( (occupiedMap[stateNumber])[schNumber] < occuNumber )
									(occupiedMap[stateNumber])[schNumber] = occuNumber;
							}
							else
								(occupiedMap[stateNumber])[schNumber] = occuNumber;
						}
						else
							(occupiedMap[stateNumber])[schNumber] = occuNumber;
					}
				}
			}
		}

		for ( auto mapIter : timingMap )
		{
			unsigned stateNumber = mapIter.first;
			for ( unsigned i = 0; i < maxMap[stateNumber] +1; ++i )
			{
				unsigned portN = 0;
				
				if ( unresolved ) {
					if ( occupiedMap.count(stateNumber) )
						if ( (occupiedMap[stateNumber]).count(i) )
							portN = (occupiedMap[stateNumber])[i];
				}

//				errs() << "Initial Port Number : " << portN << "\n";

				for ( auto inst : (mapIter.second)[i] )
				{
					if ( portN == 0 ) {
						(fsm->getStateFromInst(inst))->setLoadPortNum(inst, true); 
//						inst->dump();
//						errs() << "\t\tTiming : " << i << "\t\t : Mem Port A\n\n";
					}
					else {
						(fsm->getStateFromInst(inst))->setLoadPortNum(inst, false);
//						inst->dump();
//						errs() << "\t\tTiming : " << i << "\t\t : Mem Port B\n\n";
					}

//					if ( portN < 2 )
//						(fsm->getStateFromInst(inst))->setLoadPortNum(inst, portN);
//					else
					if ( portN > 2 )
						assert( 0 && "There are more than 2 mem inst");

					++portN;
				}
			}
		}
	}

	void FSMBuilder::setState(FSM *fsm)
	{
		initNoNameNumber();
		for ( auto stateIter : fsm->getStateList() )
		{
			ScheduleInfo *schedule = state2ScheduleInfo[stateIter];

			int max_s = 0;
			int max_end = 0;
			for ( auto instIter : stateIter->getInstList() )
			{
				double sch = schedule->getSSchFromInst(instIter);
				double end_sch = schedule->getESchFromInst(instIter);
				int intSch = ((int)(sch + 0.1));
				int endSch = ((int)(end_sch + 0.1));

				stateIter->setScheduleMap((unsigned)intSch, instIter);
				stateIter->setEScheduleMap((unsigned)endSch, instIter);

				if ( max_s < intSch )
					max_s = intSch;

				//if the last instruction that leads max_end schedule time is a store inst,
				// state transition time will compensate the store time.

				if ( isa<StoreInst>(instIter) ) {
					if ( checkEndPointStoreInst(dyn_cast<StoreInst>(instIter), stateIter, fsm) ) {
						if ( max_end < endSch )
							max_end = endSch;
						errs() << "!!!EndPointStoreInst!!!\n";
					}
					else {
						if ( max_end < endSch-1 )
							max_end = endSch -1;
					}
				}
				else {
					if ( max_end < endSch )
						max_end = endSch;
				}

				//noName Instruction Numbering
				StringRef noName("");
				if ( (instIter->getName()).str() == noName.str() )
					stateIter->setNoNameInstNum(instIter, genNoNameInstNum());
			}

			//state (having call instruction) latency checker
			/*
			bool oneMoreLatency = false;
			Instruction *exitInst = stateIter->getExitInst();
			for ( auto instIter : stateIter->getInstList() )
			{
				double end_sch = schedule->getESchFromInst(instIter);
				int endSch = ((int)(end_sch + 0.1));

				if ( (endSch == max_end) && isa<LoadInst>(instIter) )
					oneMoreLatency = true;
			}*/


			if ( stateIter->isPipelineState() ) {
				stateIter->setLatency(2);
				PipelineState *pState = fsm->getPipelineState(stateIter);
				pState->setMaxEnd((unsigned)max_end);
				//setExitPoint
				int max_s_exit = 0;
				for ( auto bb : pState->getBB() )
					if ( &*(--(bb->end())) == stateIter->getExitInst() )
						for ( auto ii = bb->begin(); ii != bb->end(); ++ii ) {
							int s_exit = (int)(schedule->getSSchFromInst(&*ii) + 0.1);
							if ( max_s_exit < s_exit )
								max_s_exit = s_exit;
						}
				pState->setExitPoint((unsigned)max_s_exit);
				errs() << "ExitPoint : " << max_s_exit << "\n";
			}
			else {
				if ( isa<CallInst>(stateIter->getExitInst()) ) {
//					if ( oneMoreLatency )
//						stateIter->setLatency((unsigned)max_end + 2);
//					else
						stateIter->setLatency((unsigned)max_end + 1);
				}
				else {
					stateIter->setLatency((unsigned)max_end + 1);

//XXX: max_end + 1 when any instruction uses the value created at max_end
//p.s. exit instruction executed at latency-1
//XXX: max_s +1 when any instruction do not uses the value created at max_end
				}
			}
			stateIter->setMaxStartV((unsigned)max_s);
		}


		for ( auto fi = module->begin(); fi != module->end(); ++fi )
		{
			Function *func = &*fi;
			for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			{
				//Private memories
				if ( (memoryTable->getPrivateAccessInst(func, ram)).size() != 0 )
					portAssignment( fsm, memoryTable->getPrivateAccessInst(func, ram), false, func );
			}

			//Address Memories
			if ( (memoryTable->getAddressAccessInst(func)).size() != 0 ) {
				portAssignment( fsm, memoryTable->getAddressAccessInst(func), true, func );
			}

//Unresolved Memories
// Unresolved Memory Instructions are included into address access inst set
//			if ( (memoryTable->getUnresolvedInst(func)).size() != 0 )
//				portAssignment( fsm, memoryTable->getUnresolvedInst(func) );
		}
		

		//TODO : Port Assignment
		/*
		// Memory Port limitation Handling
		//TODO : PRivate case
		//TODO : Memory Port fixed with only 2 , we need flexible port handling
		for ( auto stateIter : fsm->getStateList() )
		{
//			unsigned maxS = stateIter->getMaxStartV();
			unsigned maxS = stateIter->getLatency();
			for ( unsigned i = 0; i < maxS; i++ )
			{
				ScheduleMap schM = stateIter->getScheduleMap();

				unsigned portN = 0;
				for ( auto inst : schM[i] )
					if ( isa<LoadInst>(inst) || isa<StoreInst>(inst) )
					if ( !mm->getInstPrivateUse(inst) ) {
//						errs() << inst->getFunction()->getName() << "\n";
//						inst->dump();
//						errs() << i << " : " << portN << "\n\n";
						if ( portN < 2 )
							stateIter->setLoadPortNum(inst, portN);
						else
							assert(0&&"there are more than 2 load or store instruction");
						portN++;
					}

				//Private Parts
				for ( auto ramIter : mm->getPrivateList() ) {
					portN = 0;
					for ( auto inst : schM[i] )
						if ( isa<LoadInst>(inst) || isa<StoreInst>(inst) )
						if ( mm->getInstPrivateUse(inst) )
							if ( ramIter == mm->getPrivateRAMFromInst(inst) ) {
								if ( portN < 2 )
									stateIter->setLoadPortNum(inst, portN);
								else
									assert(0&&"there are more than 2 load or store instruction");
								portN++;		
							}
				}
			}
		}
		*/

	}

	void FSMBuilder::setScheduleForState(FSM *fsm)
	{
		std::error_code ec_print = std::make_error_code(std::errc::io_error);
		raw_fd_ostream scheduleFile("schedule.debug", ec_print, llvm::sys::fs::F_None);

		for ( auto stateIter : fsm->getStateList() )
		{
			if ( stateIter->isPipelineState() ) {
				PipelineState *pState = fsm->getPipelineState(stateIter);
				assert(pState);

				SDCSchedulingBuilder *scheduleB 
					= new SDCSchedulingBuilder(bbcdfg, verilogConfigInfo, stateIter, pState, 
														&scheduleFile, memoryTable, operatorTable, bitWidthTable);
				setState2ScheduleInfo (pState, scheduleB->getSDCSchedulingForPipeline());
			}
			else {
				SDCSchedulingBuilder *scheduleB 
					= new SDCSchedulingBuilder(bbcdfg, verilogConfigInfo, stateIter, NULL,
														&scheduleFile, memoryTable, operatorTable, bitWidthTable);
				setState2ScheduleInfo (stateIter, scheduleB->getSDCScheduling());
				//if null , there is no instructions only exitcond
			}
		}
	}

	void FSMBuilder::setFinalInstructions(FSM *fsm)
	{
		list<State *> stateList = fsm->getStateList();

		for ( auto stateIter : stateList )
		{
			list<Instruction *> instList = stateIter->getInstList();

			if ( stateIter->isPipelineState() ) {
				PipelineState *pState = fsm->getPipelineState(stateIter);
				for ( auto inst : instList ) {
					unsigned maxUserSch = 0;
					for ( auto ui = inst->user_begin(); ui != inst->user_end(); ++ui ) {
						Instruction *user = dyn_cast<Instruction>(*ui);
						assert(user);
						if ( stateIter->isInThisState(user) && isa<PHINode>(user) ) {
							//ESchFromInst == generation timing
							pState->setPHIUsedMap(stateIter->getESchFromInst(inst), inst);
							pState->setPHISubstitute(user, inst);
						}
						else if ( stateIter->isInThisState(user)  && !isa<BranchInst>(user) ) {
							unsigned userSch = stateIter->getSchFromInst(user);
							if ( maxUserSch < userSch )
								maxUserSch = userSch;
						}
					}
					for ( unsigned i = stateIter->getSchFromInst(inst); i <= maxUserSch; ++i )
						pState->setUsedMap(i, inst);
				}
			}

			for ( auto inst : instList )
			{
//				stateIter->setIsUsedAT(inst, true);
				if ( isa<PHINode>(inst) ) {
					stateIter->setIsUsedAT(inst, true);
					continue;
				}

				unsigned thisState = stateIter->getSchFromInst(inst);

				//is used another state
				bool isUsedAT = false;
				for ( auto ui = inst->user_begin(); ui != inst->user_end(); ++ui )
				{
					Instruction *user = dyn_cast<Instruction>(*ui);
					assert(user);

					if ( PHINode *phi = dyn_cast<PHINode>(user) ) {
						isUsedAT = true;
						break;
					}

					bool inTheState = false;
					for ( auto findIter : instList )
						if ( user == findIter ) {
							inTheState = true;
							break;
						}
					
					if (inTheState) {
						unsigned userState = stateIter->getSchFromInst(user);
						if ( userState != thisState ) {
							isUsedAT = true;
							break;
						}
					}
					else {
						isUsedAT = true;
						break;
					}
				}

				if ( isUsedAT )
					stateIter->setIsUsedAT(inst, true);
				else
					stateIter->setIsUsedAT(inst, false);
			}
		}
	}

	//return true : store latency should be 1
	bool FSMBuilder::checkEndPointStoreInst(StoreInst *sInst, State *state, FSM *fsm) {
		Instruction *exitI = state->getExitInst();
		if ( CallInst *callInst = dyn_cast<CallInst>(exitI) ) {
			Function *func = callInst->getCalledFunction();
			assert(func);
			return thisCallUseSameObject(sInst, func, fsm);
		}
		else
			return false;
	}

	//return true, if func uses the same objects ( as the store inst's ) in first cycle
	bool FSMBuilder::thisCallUseSameObject(StoreInst *sInst, Function *func, FSM *fsm) {
		//XXX: what if the func calls another function in the first cycle??

		State *sState = fsm->getStartStateFromFnc(func);
		ScheduleInfo *schedule = state2ScheduleInfo[sState];
		set<Instruction *> firstStateInstructions;
		firstStateInstructions.clear();
		for ( auto instIter : sState->getInstList() )
		{
			double sch = schedule->getSSchFromInst(instIter);
			int intSch = ((int)(sch + 0.1));
			
			if ( intSch == 0 )
				if ( isa<StoreInst>(instIter) || isa<LoadInst>(instIter) )
					firstStateInstructions.insert(instIter);
		}

		if ( firstStateInstructions.size() == 0 )
			return false;

		set<RAM_ *> storeInstRAMs;
		storeInstRAMs.clear();
		//AddressAccess of sInst
		Function *caller = sInst->getFunction();
		assert(caller);
		bool addressAccessStoreInst = false;
		for ( auto instIter : memoryTable->getAddressAccessInst(caller) )
			if ( instIter == sInst ) {
				addressAccessStoreInst = true;
				break;
			}
		if ( addressAccessStoreInst ) {
			for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(caller) )
				storeInstRAMs.insert(ram);
		}
		else { //PrivateAccess of sInst
			for ( auto ram : memoryTable->getAccessRAMFromFunction(caller) )
				for ( auto instIter : memoryTable->getPrivateAccessInst(caller, ram) )
					if ( instIter == sInst )
						storeInstRAMs.insert(ram);
		}
		assert(storeInstRAMs.size() != 0);

		set<RAM_ *> firstInstRAMs;
		firstInstRAMs.clear();
		for ( auto memInst : firstStateInstructions )
		{
			bool addressAccessInst = false;
			for ( auto instIter : memoryTable->getAddressAccessInst(func) )
				if ( instIter == memInst ) {
					addressAccessInst = true;
					break;
				}
			if ( addressAccessInst ) {
				for ( auto ram : memoryTable->getAddressAccessRAMFromFunction(func) )
					firstInstRAMs.insert(ram);
			}
			else { //PrivateAccess of sInst
				for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
					for ( auto instIter : memoryTable->getPrivateAccessInst(func, ram) )
						if ( instIter == memInst )
							firstInstRAMs.insert(ram);
			}
		}
		assert(firstInstRAMs.size() != 0);

		for ( auto sRAM : storeInstRAMs )
			for ( auto fRAM : firstInstRAMs )
				if ( sRAM == fRAM )
					return true;

		return false;
	}


	FSM *FSMBuilder::getFSM(void)
	{
		FSM *fsm = new FSM();

		initStateNumber();

		errs() << "FSM : initStateNumber Done\n";

//		setScheduleInfo();

		setFSM(fsm);
		errs() << "FSM : setFSM Done\n";

		connectStates(fsm);
		errs() << "FSM : connectFSM Done\n";
	
		setScheduleForState(fsm);
		errs() << "FSM : SDC Schedule Done\n";

		setState(fsm);
		errs() << "FSM : setState Done\n";

		setFinalInstructions(fsm);
		errs() << "FSM : setAT Done\n";
		
//		fsm->printFSM();

		return fsm;
	}

	// return 1 : parallel callsite // 0 : in-order callsite
	unsigned FSMBuilder::getMetadata(Instruction *inst) {
		MDNode *md = inst->getMetadata("parallel");
		if ( md ) {
			Metadata *m  = md->getOperand(0).get();
			ValueAsMetadata *VMD = dyn_cast<ValueAsMetadata>(m);
			assert(VMD);
			Value *v = VMD->getValue();
			assert(v);

			ConstantInt *cInt = dyn_cast<ConstantInt>(v);
			assert(cInt);

			return cInt->getZExtValue();
		}
		else
			return 0;
	}

}// end namespace
