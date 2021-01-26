#include "llvm/Support/FileSystem.h"
#include <system_error>

#include "SDCScheduling.h"
#include "FSM.h"
#include "LoopPatternAnal.h"

#include <lpsolve/lp_lib.h>

//#define DEBUG_SDCSCHEDULING

namespace corelab
{
	using namespace llvm;
	using namespace std;

	static lprec *lp;

	static void initLPSolver(Function *, VerilogConfigInfo *, ScheduleInfo *);
	static void multiCycleConstraints(ScheduleInfo *);
	static void addBasicBlockConstraints(Function *, ScheduleInfo *);
	static void addRegMemDepConstraints(Function *, BBCDFG *, OperatorTable *,
																			VerilogConfigInfo *, ScheduleInfo *,
																			BitWidthTable *);
	static void solveWithASAP(ScheduleInfo *);

	static void initLPSolver(State *state, 
			VerilogConfigInfo *verilogConfigInfo, ScheduleInfo *schedule){
		unsigned indexCount = 0;
		
		for ( auto inst : state->getInstList() )
		{
			int latency = verilogConfigInfo->getOperationInfo(inst).latency[0];
			assert(0 <= latency);
			schedule->setInst2Index(inst, indexCount, indexCount+(unsigned)latency);
			indexCount += 1 + latency;
		}

		lp = make_lp(0, indexCount);
		schedule->setNumVar( indexCount );
	}

	static void multiCycleConstraints(ScheduleInfo *schedule){
		for ( auto iter : schedule->getInst2Index() )
		{
//			int varIndex_[1] = { iter.second.first + 1 };
//			REAL cof_[1] = { 1 };
//			add_constraintex(lp, 1, cof_, varIndex_, GE, 0.0);
			for ( int i = iter.second.first ; i != iter.second.second; i++ )
			{
				int varIndex[2] = { i+1, i+2 };
				REAL cof[2] = { -1, 1 };
				add_constraintex(lp, 2, cof, varIndex, EQ, 1.0);
			}
		}
	}

	//This is for Pipeline State that contains more than two bb
	static void addBranchConstraints(PipelineState *pState, ScheduleInfo *schedule,
																VerilogConfigInfo *verilogConfigInfo) {
		list<BasicBlock *> bbList = pState->getBB();

		//Normal has only one basic block
		if ( pState->getType() != 1 ) {
			BasicBlock *endBB = pState->getExitInst()->getParent();

			//This is EDGE Case
			//ASSUME : The order of this list is right
			if ( pState->getType() == 2 ) {
				for ( auto bi = bbList.begin(); bi != bbList.end(); ) {
					BasicBlock *bb = *bi;
					if ( ++bi == bbList.end() )
						break;
					BasicBlock *nextBB = *bi;
					
					for ( auto ii = bb->begin(); ii != bb->end(); ii++ )
						for ( auto iiN = nextBB->begin(); iiN != nextBB->end(); iiN++ ) {
							Instruction *inst = &*ii;
							Instruction *nextInst = &*iiN;

							if ( !schedule->isInstInState(inst) || isa<PHINode>(inst) ||
									!schedule->isInstInState(nextInst) || isa<PHINode>(nextInst) )
								continue;

							int col[2];
							REAL val[2];

							col[0] = schedule->getIndexFromInst(inst).second+1; // end index
							val[0] = 1.0;
							col[1] = schedule->getIndexFromInst(nextInst).first+1; // start index
							val[1] = -1.0;
							//TODO : Automatic Chain vs Pipeline for efficient Max frequency
							if ( verilogConfigInfo->getFullChaining() )
								add_constraintex(lp, 2, val, col, LE, 0.0); // chaining
							else {
								add_constraintex(lp, 2, val, col, LE, -1.0);
							}
						}
				}
			}
			else if ( pState->getType() == 3 ) {
				assert(0 && "PREHEADER CASE can not come in \n" );
			}
		}
			
	}

	static void addRegMemDepConstraintsForPipeline(PipelineState *state, BBCDFG *bbcdfg,
																			OperatorTable *operatorTable,
																			VerilogConfigInfo *verilogConfigInfo, 
																			ScheduleInfo *schedule){
		BBCDFG_Map regUseMap = bbcdfg->getRegUseMap();
		BBCDFG_Map memUseMap = bbcdfg->getMemUseMap();

		//Iterate Instructions for a state
		for ( auto inst : state->getInstList() )
		{
			//Get all Use Chains in this Basic Block
			//TODO: should replace this memUseMap with advanced one
			InstructionSet useInsts = regUseMap[inst];
			InstructionSet memUseInsts = memUseMap[inst];

			//Register Constraints
			for ( auto iter : useInsts )
			{
				Instruction *useInst = iter;

				if ( !schedule->isInstInState(useInst) || isa<PHINode>(useInst) )
					continue;

				int col[2];
				REAL val[2];

				col[0] = schedule->getIndexFromInst(inst).second+1; // end index
				val[0] = 1.0;
				col[1] = schedule->getIndexFromInst(useInst).first+1; // start index
				val[1] = -1.0;

				//pipeline exit
				if ( state->getLA()->isForExitInst(inst) &&
						state->getLA()->isForExitInst(useInst) )
					add_constraintex(lp, 2, val, col, LE, 0.0);
				else {
					if ( operatorTable->isShareableUser(inst) ||
							operatorTable->isShareableUser(useInst) )
						add_constraintex(lp, 2, val, col, LE, -1.0);
					else if ( (isa<StoreInst>(useInst) || isa<LoadInst>(useInst)) && 
							!verilogConfigInfo->getFullChaining() )
						add_constraintex(lp, 2, val, col, LE, -1.0);
					else
						add_constraintex(lp, 2, val, col, LE, 0.0); // chaining
				}


#ifdef DEBUG_SDCSCHEDULING
				errs() << *inst << " -> " << *useInst << "\n";
#endif
			}

			//Memory Constraints
			for ( auto iter : memUseInsts )
			{
				Instruction *useInst = iter;

				if ( !schedule->isInstInState(useInst) )
					continue;

				int col[2];
				REAL val[2];

				col[0] = schedule->getIndexFromInst(inst).second+1; // end index
				val[0] = 1.0;
				col[1] = schedule->getIndexFromInst(useInst).first+1; // start index
				val[1] = -1.0;
				add_constraintex(lp, 2, val, col, LE, 0.0);

#ifdef DEBUG_SDCSCHEDULING
				errs() << *inst << " -> " << *useInst << "\n";
#endif
			}
		}
	}

	static bool inTheSet (set<list<Instruction *>> instSet, Instruction *inst) {
		for ( auto iter : instSet )
			for ( auto iter2 : iter )
				if ( iter2 == inst )
					return true;
		return false;
	}

	static void getChainInTheState(ScheduleInfo *schedule, set<list<Instruction *>> &chainSet,
										list<Instruction *> instList, Instruction *inst, BBCDFG_Map regUseMap) {
		//TODO: it takes too much time
		bool isEndPoint = true;

		if ( regUseMap.count(inst) ) {
			InstructionSet userInsts = regUseMap[inst];

			for ( auto userInst : userInsts )
			{
				//end points
				if ( !schedule->isInstInState(userInst) || isa<PHINode>(userInst) ||
						isa<StoreInst>(userInst) || isa<LoadInst>(userInst) )
					continue;

				errs() << " -> ";
				userInst->dump();

				//more chainable instructions exist
				list<Instruction *> newList(instList);
				newList.push_back(userInst);
				getChainInTheState(schedule, chainSet, newList, userInst, regUseMap);
				isEndPoint = false;
			}
		}
		errs() << "endpoint\n\n";

		if ( isEndPoint )
			chainSet.insert(instList);
	}

	static set<pair<Instruction *, Instruction *>> getBrokerPoints(State *state, 
																																ScheduleInfo *schedule, 
																					BBCDFG_Map regUseMap, BBCDFG_Map regDepMap){
		set<Instruction *> firstPoints;
		firstPoints.clear();

		set<list<Instruction *>> chainSet;
		chainSet.clear();

		set<pair<Instruction *, Instruction *>> brokerPoints;
		brokerPoints.clear();

		//collect first points
		for ( auto inst : state->getInstList() )
		{
			if ( isa<StoreInst>(inst) || isa<LoadInst>(inst) )
				continue;

			if ( !regDepMap.count(inst) ) {
				firstPoints.insert(inst);
				continue;
			}
				
			InstructionSet depInsts = regDepMap[inst];

			bool noDepInThisState = true;
			for ( auto depInst : depInsts )
			{
				if ( !schedule->isInstInState(depInst) ||
					 	isa<StoreInst>(depInst) || isa<LoadInst>(depInst) )
					continue;

				noDepInThisState = false;
			}

			if ( noDepInThisState )
				firstPoints.insert(inst);
		}

		//////////////////////////////////
		errs() << "\nFirst Point Instructions : \n";
//		for ( auto inst : firstPoints )
//			inst->dump();
//		errs() << "\n";

		//get chain set
		for ( auto inst : firstPoints )
		{
			inst->dump();
			list<Instruction *> newList;
			newList.clear();
			newList.push_back(inst);
			getChainInTheState(schedule, chainSet, newList, inst, regUseMap);
		}

		//////////////////////////////////
		errs() << "\nChained Instructions : \n";
		for ( auto chain : chainSet ) 
		{
			for ( auto inst : chain )
				inst->dump();
			errs() << "\n";
		}
		errs() << "\n";

		//count each chain
		for ( auto chain : chainSet )
		{
			int count = 0;
			Instruction *prev = NULL;
			for ( auto inst : chain )
			{
				if ( prev == NULL ) {
					prev = inst;
					count++;
				}
				else {
					//update count
					bool isThisBrokerPoint = false;
					for ( auto pair_p : brokerPoints )
					{
						if ( pair_p.first == prev && pair_p.second == inst )
							isThisBrokerPoint = true;
					}
					if ( isThisBrokerPoint )
						count = 1;
					else
						count++;

					//make broker
					if ( count == 4 ) {
						brokerPoints.insert(std::make_pair(prev,inst));
						count = 1;
					}

					//update prev
					prev = inst;
				}
			}
		}

		///////////////////////////
		errs() << "\nBroker Points : \n";
		for ( auto pair_p : brokerPoints )
		{
			(pair_p.first)->dump();
			(pair_p.second)->dump();
			errs() << "\n";
		}
		errs() << "\n";

		return brokerPoints;
	}


	static void addRegMemDepConstraints(State *state, BBCDFG *bbcdfg, OperatorTable *operatorTable,
																			VerilogConfigInfo *verilogConfigInfo, 
																			ScheduleInfo *schedule,
																			BitWidthTable *bitWidthTable){
		BBCDFG_Map regUseMap = bbcdfg->getRegUseMap();
		BBCDFG_Map regDepMap = bbcdfg->getRegDepMap();
		BBCDFG_Map memUseMap = bbcdfg->getMemUseMap();

		/*
		set<pair<Instruction *, Instruction *>> brokerPoints = 
															getBrokerPoints(state, schedule, regUseMap, regDepMap);*/

		//Iterate Instructions for a state
		for ( auto inst : state->getInstList() )
		{
			//Get all Use Chains in this Basic Block
			InstructionSet useInsts = regUseMap[inst];
			InstructionSet memUseInsts = memUseMap[inst];

			int userSize = 0;
			for ( auto iter : useInsts )
			{
				Instruction *useInst = iter;
				if ( !schedule->isInstInState(useInst) || isa<PHINode>(useInst) )
					continue;
				++userSize;
			}

			//Register Constraints
			for ( auto iter : useInsts )
			{
				Instruction *useInst = iter;

				if ( !schedule->isInstInState(useInst) || isa<PHINode>(useInst) )
					continue;

				int col[2];
				REAL val[2];

				col[0] = schedule->getIndexFromInst(inst).second+1; // end index
				val[0] = 1.0;
				col[1] = schedule->getIndexFromInst(useInst).first+1; // start index
				val[1] = -1.0;

				//XXX: Assume that shareable operator usaully takes long delay
				if ( operatorTable->isShareableUser(inst) ||
						operatorTable->isShareableUser(useInst) )
					add_constraintex(lp, 2, val, col, LE, -1.0);
				else if ( !verilogConfigInfo->getFullChaining() ) {
					if ( isa<StoreInst>(useInst) || isa<LoadInst>(useInst) ||
							isa<StoreInst>(inst) || isa<LoadInst>(inst) ||
							inst->getOpcode() == Instruction::URem ||
							inst->getOpcode() == Instruction::SRem )
						add_constraintex(lp, 2, val, col, LE, -1.0);
					else {
						/*
						bool broker = false;

						if ( brokerPoints.size() != 0 ) {
							for ( auto points : brokerPoints )
							{
								if ( points.first == inst && points.second == useInst ) {
									broker = true;

									errs() << "Get Broker !!\n";
									inst->dump();
									useInst->dump();
									errs() << "\n";
								}
							}
						}

						if ( broker )
							add_constraintex(lp, 2, val, col, LE, -1.0);
						else
							add_constraintex(lp, 2, val, col, LE, 0.0);
						*/
						//TODO: This is tmp implementation
						if ( 2 < userSize )
							add_constraintex(lp, 2, val, col, LE, -1.0);
						else
							add_constraintex(lp, 2, val, col, LE, 0.0);
					}
				}
				else {
					if ( inst->getOpcode() == Instruction::URem ||
							inst->getOpcode() == Instruction::SRem ||
							(inst->getOpcode() == Instruction::Mul && 31 < bitWidthTable->getWidth(inst)) ||
							isa<CallInst>(inst) )
						add_constraintex(lp, 2, val, col, LE, -1.0); 
					else
						add_constraintex(lp, 2, val, col, LE, 0.0); // chaining
				}
/*
				else if ( (isa<StoreInst>(useInst) || isa<LoadInst>(useInst)) && 
						!verilogConfigInfo->getFullChaining() )
					add_constraintex(lp, 2, val, col, LE, -1.0);
				else
*/

#ifdef DEBUG_SDCSCHEDULING
				errs() << *inst << " -> " << *useInst << "\n";
#endif
			}

			//Memory Constraints
			for ( auto iter : memUseInsts )
			{
				Instruction *useInst = iter;

				if ( !schedule->isInstInState(useInst) )
					continue;

				int col[2];
				REAL val[2];

				col[0] = schedule->getIndexFromInst(inst).second+1; // end index
				val[0] = 1.0;
				col[1] = schedule->getIndexFromInst(useInst).first+1; // start index
				val[1] = -1.0;
				add_constraintex(lp, 2, val, col, LE, 0.0);

#ifdef DEBUG_SDCSCHEDULING
				errs() << *inst << " -> " << *useInst << "\n";
#endif
			}

		}

	}


	static void solveWithASAP(ScheduleInfo *schedule){

		unsigned size = schedule->getNumVar();

		int i = 1;
		REAL cof[size+1];
		REAL var[size];

		for ( ; i < size + 1; i++)
			cof[i] = 1;

		assert ( size+1 == i );

		set_obj_fn(lp, cof);

		set_minim(lp);

		int result = solve(lp);

		if ( result != 0 )
		{
#ifdef DEBUG_SDCSCHEDULING
			errs() << "solve err : "<< result <<"\n";
#endif
		}
		else
		{
			get_variables(lp, var);

			for ( i = 0; i < size ; ++i )
				schedule->setIndex2Sch(i, var[i]);	
		}

		delete_lp(lp);
	}

	static void addResourceRestrictions(State *state, BBCDFG *bbcdfg,
																			VerilogConfigInfo *verilogConfigInfo, 
																			ScheduleInfo *schedule_old,
																			ScheduleInfo *schedule,
																			MemoryTable *memoryTable,
																			OperatorTable *operatorTable){
		Function *func = state->getParentFunction();

		unsigned allocatedMaxNum;
		unsigned timingCount;
		unsigned odd;
		unsigned maxSch;

		//Operator Resource Restriction
		for ( auto opType : operatorTable->getUsedOperatorByFunction(func) )
		{
			DenseMap<unsigned, list<Instruction *>> timingMap;
			DenseMap<unsigned, list<Instruction *>> timingMapWithRestriction;

			allocatedMaxNum = 0;
			timingCount = 0;
			odd = 0;
			maxSch = 0;

			//from old schedule
			for ( auto inst : operatorTable->getOperatorUserInst(func, opType) )
			{
				if ( !state->isInThisState(inst) )
					continue;

				unsigned sch = (int)( schedule_old->getSSchFromInst(inst) + 0.1 );
				if ( maxSch < sch )
					maxSch = sch;
				timingMap[sch].push_back(inst);
			}

			//timing map setting
			for ( unsigned i = 0; i < maxSch+1; ++i )
			{
				timingCount = 0;
				odd = 0;
				for ( auto inst : timingMap[i] )
				{
					timingMapWithRestriction[allocatedMaxNum + timingCount].push_back(inst);
					if ( odd % 2 == 1 )
						timingCount++;
					odd++;
				}
				if ( odd % 2 == 1 )
					allocatedMaxNum = allocatedMaxNum + timingCount + 1;
				else
					allocatedMaxNum = allocatedMaxNum + timingCount;
			}

			//add constraints
			for ( unsigned i = 0; i < allocatedMaxNum; i++ )
			{
				assert( timingMapWithRestriction[i].size() < 3 );
				for ( auto inst_before : timingMapWithRestriction[i] )
				{
					assert( timingMapWithRestriction[i+1].size() < 3 );
					for ( auto inst_after : timingMapWithRestriction[i+1] )
					{
						int col[2];
						REAL val[2];

						col[0] = schedule->getIndexFromInst(inst_before).second+1; // end index
						val[0] = 1.0;
						col[1] = schedule->getIndexFromInst(inst_after).first+1; // start index
						val[1] = -1.0;
						add_constraintex(lp, 2, val, col, LE, -1.0);//operation basically has latency 0
					}
				}
			}
		}

		//Private Access Memory Resource Restriction
		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
		{
			//if there is accessed private ram by this function
			if ( (memoryTable->getPrivateAccessInst(func, ram)).size() == 0 )
				continue;

			DenseMap<unsigned, list<Instruction *>> timingMap;
			DenseMap<unsigned, list<Instruction *>> timingMapWithRestriction;

			allocatedMaxNum = 0;
			timingCount = 0;
			odd = 0;
			maxSch = 0;

			//from old schedule - private ram
			for ( auto inst : memoryTable->getPrivateAccessInst(func, ram) )
			{
				if ( !state->isInThisState(inst) )
					continue;

				unsigned sch = (int)( schedule_old->getSSchFromInst(inst) + 0.1 );
				if ( maxSch < sch )
					maxSch = sch;
				timingMap[sch].push_back(inst);
			}

			//from old schedule - address ram 
			//conflict between address ram -> dynamic solved port and private port
			for ( auto inst : memoryTable->getAddressAccessInst(func) )
			{
				if ( !state->isInThisState(inst) )
					continue;

				unsigned sch = (int)( schedule_old->getSSchFromInst(inst) + 0.1 );
				if ( maxSch < sch )
					maxSch = sch;
				timingMap[sch].push_back(inst);
			}

			//timing map setting
			for ( unsigned i = 0; i < maxSch+1; ++i )
			{
				timingCount = 0;
				if ( verilogConfigInfo->getRAMInit() ) {
					for ( auto inst : timingMap[i] )
					{
						timingMapWithRestriction[allocatedMaxNum + timingCount].push_back(inst);
						timingCount++;
					}
					allocatedMaxNum = allocatedMaxNum + timingCount;
				}
				else {
					odd = 0;
					for ( auto inst : timingMap[i] )
					{
						timingMapWithRestriction[allocatedMaxNum + timingCount].push_back(inst);
						if ( odd % 2 == 1 )
							timingCount++;
						odd++;
					}
					if ( odd % 2 == 1 )
						allocatedMaxNum = allocatedMaxNum + timingCount + 1;
					else
						allocatedMaxNum = allocatedMaxNum + timingCount;
				}
			}

			//add constraints
			for ( unsigned i = 0; i < allocatedMaxNum; i++ )
			{
				assert( timingMapWithRestriction[i].size() < 3 );
				for ( auto inst_before : timingMapWithRestriction[i] )
				{
					assert( timingMapWithRestriction[i+1].size() < 3 );
					for ( auto inst_after : timingMapWithRestriction[i+1] )
					{
						int col[2];
						REAL val[2];

						col[0] = schedule->getIndexFromInst(inst_before).second+1; // end index
						val[0] = 1.0;
						col[1] = schedule->getIndexFromInst(inst_after).first+1; // start index
						val[1] = -1.0;
						add_constraintex(lp, 2, val, col, LE, 0.0);
					}
				}
			}
		}


		//Address Access Memory Resource Restriction
		DenseMap<unsigned, list<Instruction *>> timingMap;
		DenseMap<unsigned, list<Instruction *>> timingMapWithRestriction;

		allocatedMaxNum = 0;
		timingCount = 0;
		odd = 0;
		maxSch = 0;

		//from old schedule
		for ( auto inst : memoryTable->getAddressAccessInst(func) )
		{
			if ( !state->isInThisState(inst) )
				continue;

			unsigned sch = (int)( schedule_old->getSSchFromInst(inst) + 0.1 );
			if ( maxSch < sch )
				maxSch = sch;
			timingMap[sch].push_back(inst);
		}

		//timing map setting
		for ( unsigned i = 0; i < maxSch+1; ++i )
		{
			timingCount = 0;
			odd = 0;
			for ( auto inst : timingMap[i] )
			{
				timingMapWithRestriction[allocatedMaxNum + timingCount].push_back(inst);
				if ( odd % 2 == 1 )
					timingCount++;
				odd++;
			}
			if ( odd % 2 == 1 )
				allocatedMaxNum = allocatedMaxNum + timingCount + 1;
			else
				allocatedMaxNum = allocatedMaxNum + timingCount;
		}

		//add constraints
		for ( unsigned i = 0; i < allocatedMaxNum; i++ )
		{
			assert( timingMapWithRestriction[i].size() < 3 );
			for ( auto inst_before : timingMapWithRestriction[i] )
			{
				assert( timingMapWithRestriction[i+1].size() < 3 );
				for ( auto inst_after : timingMapWithRestriction[i+1] )
				{
					int col[2];
					REAL val[2];

					col[0] = schedule->getIndexFromInst(inst_before).second+1; // end index
					val[0] = 1.0;
					col[1] = schedule->getIndexFromInst(inst_after).first+1; // start index
					val[1] = -1.0;
					add_constraintex(lp, 2, val, col, LE, 0.0);
				}
			}
		}

		// Unresolved Access Memory Resource Restriction

		timingMap.clear();
		timingMapWithRestriction.clear();

		allocatedMaxNum = 0;
		timingCount = 0;
		odd = 0;
		maxSch = 0;

		//from old schedule
		for ( auto inst : memoryTable->getUnresolvedInst(func) )
		{
			if ( !state->isInThisState(inst) )
				continue;

			unsigned sch = (int)( schedule_old->getSSchFromInst(inst) + 0.1 );
			if ( maxSch < sch )
				maxSch = sch;
			timingMap[sch].push_back(inst);
		}

		//timing map setting
		for ( unsigned i = 0; i < maxSch+1; ++i )
		{
			timingCount = 0;
			odd = 0;
			for ( auto inst : timingMap[i] )
			{
				timingMapWithRestriction[allocatedMaxNum + timingCount].push_back(inst);
				if ( odd % 2 == 1 )
					timingCount++;
				odd++;
			}
			if ( odd % 2 == 1 )
				allocatedMaxNum = allocatedMaxNum + timingCount + 1;
			else
				allocatedMaxNum = allocatedMaxNum + timingCount;
		}

		//add constraints
		for ( unsigned i = 0; i < allocatedMaxNum; i++ )
		{
			assert( timingMapWithRestriction[i].size() < 3 );
			for ( auto inst_before : timingMapWithRestriction[i] )
			{
				assert( timingMapWithRestriction[i+1].size() < 3 );
				for ( auto inst_after : timingMapWithRestriction[i+1] )
				{
					int col[2];
					REAL val[2];

					col[0] = schedule->getIndexFromInst(inst_before).second+1; // end index
					val[0] = 1.0;
					col[1] = schedule->getIndexFromInst(inst_after).first+1; // start index
					val[1] = -1.0;
					add_constraintex(lp, 2, val, col, LE, 0.0);
				}
			}
		}

	}

	unsigned getABS(unsigned oldSch_mod, unsigned reschedule_mod) {
		int oldMod = (int)oldSch_mod;
		int reMod = (int)reschedule_mod;
		int newMod = reMod - oldMod;
		if ( newMod < 0 )
			newMod = -newMod;
		return (unsigned)newMod;
	}

	static void addPipelineConstraints(ScheduleInfo *schedule_old, ScheduleInfo *schedule,
																		DenseMap<unsigned, MemTable> &addTable, unsigned interval) {
		for ( auto iter : addTable ) {
			if ( (iter.second).empty() ) continue;
			for ( auto memIter : iter.second )
				for ( auto inst : memIter.second ) {
					unsigned oldSch = (int)( schedule_old->getSSchFromInst(inst) + 0.1 );
					unsigned reschedule_mod = iter.first;
					unsigned oldSch_mod = oldSch % interval;
					unsigned reschedule = oldSch + getABS(oldSch_mod, reschedule_mod);

					errs() << "ADD CON : " << oldSch << " : " << reschedule_mod << " : ";
					errs() << oldSch_mod << " : " << reschedule << "\n\n";

					int col[1];
					REAL val[1];
					col[0] = schedule->getIndexFromInst(inst).first+1;
					val[0] = 1.0;
					add_constraintex(lp, 1, val, col, EQ, (double)reschedule);
				}
		}
	}

	//Now this schedule2 is on LP
	static void copySchedule(PipelineState *state,
			ScheduleInfo *schedule_1, ScheduleInfo *schedule_2) {
		for ( auto inst : state->getInstList() )
		{
			double sch = schedule_1->getSSchFromInst(inst);
			
			int col[1];
			REAL val[1];

			col[0] = schedule_2->getIndexFromInst(inst).first+1;
			val[0] = 1.0;
			add_constraintex(lp, 1, val, col, EQ, sch );
		}
	}

	unsigned SDCSchedulingBuilder::getIIMem(PipelineState *state, ScheduleInfo *schedule) {
		unsigned intervalOffset = 0;

		unsigned maxSchOfState = 0;
		for ( auto inst : state->getInstList() ) {
			unsigned sch = (int)( schedule->getSSchFromInst(inst) + 0.1 );
			if ( maxSchOfState < sch )
				maxSchOfState = sch;
		}

		LoopAliasInfo *la = state->getLA();
		for ( auto inst : state->getInstList() )
			if ( StoreInst *sInst = dyn_cast<StoreInst>(inst) ) 
				if ( la->getUseAliasList(inst).size() != 0 ) {
					
					for ( auto iter : la->getUseAliasList(inst) ) {
						Instruction *userInst = iter.first;
						LoopAliasInfo::AliasInfo ai = iter.second;
						//Loop Carried Dependence Case
						if ( ai.type == LoopAliasInfo::Inter )
							if ( ai.distance != 0 ) {
								unsigned defSch = (int)( schedule->getSSchFromInst(sInst) + 0.1 );
								unsigned useSch = (int)( schedule->getSSchFromInst(userInst) + 0.1 );
								unsigned actualUseSch = useSch + ai.distance;

								errs() << "GET II MEM : " << ai.distance << "\n";
								errs() << "Actual Use Sch : " << actualUseSch << "\n";
								errs() << "Def Sch : " << defSch << "\n";

								while( 1 ) {
									if ( defSch < actualUseSch + intervalOffset )
										break;
									else
										++intervalOffset;
								}
								errs() << "Interval Offset : " << intervalOffset << "\n\n";
							}
					}
				}
		
		//TODO: XXX:
		//If Asynchronous Mem Read & store the value at the same cycle,
		//then you can use intervalOffset -1 instead of intervalOffset.
		return intervalOffset;
	}

	unsigned SDCSchedulingBuilder::getIIReg(PipelineState *state, ScheduleInfo *schedule) {
		unsigned intervalOffset = 0;

		unsigned maxSchOfState = 0;
		for ( auto inst : state->getInstList() ) {
			unsigned sch = (int)( schedule->getSSchFromInst(inst) + 0.1 );
			if ( maxSchOfState < sch )
				maxSchOfState = sch;
		}

		for ( auto inst : state->getInstList() )
		{
			//ALL internal Reg dependecies are because of PHI
			if ( PHINode *phiInst = dyn_cast<PHINode>(inst) ) {
				unsigned firstOfThisPhiUser = maxSchOfState;
				for ( auto ui = inst->user_begin(); ui != inst->user_end(); ++ui ) {
					if ( Instruction *phiUser = dyn_cast<Instruction>(*ui) )
						if ( state->isInThisState(phiUser) ) {
							unsigned sch = (int)( schedule->getSSchFromInst(phiUser) + 0.1 );
							if ( firstOfThisPhiUser > sch )
								firstOfThisPhiUser = sch;
						}
				}
				//Find Def of Operand of this phi
				unsigned schOfDef = 0;
				unsigned numOps = phiInst->getNumOperands();
				for ( unsigned i = 0; i < numOps; ++i ) {
					if ( Instruction *defInst = dyn_cast<Instruction>(phiInst->getOperand(i)) )
						if ( state->isInThisState(defInst) ) {
							unsigned sch = (int)( schedule->getSSchFromInst(defInst) + 0.1 );
							if ( schOfDef < sch )
								schOfDef = sch;
						}
				}
				//DEBUG
				errs() << "GET II REG : PHI : " << *phiInst << "\n";
				errs() << "firstUser : " << firstOfThisPhiUser << "\n";
				errs() << "PhiDef : " << schOfDef << "\n";

				firstOfThisPhiUser = firstOfThisPhiUser + 1;
				while ( 1 ) {
					if ( schOfDef < firstOfThisPhiUser + intervalOffset )
						break;
					else
						++intervalOffset;
				}
				errs() << "Interval Offset : " << intervalOffset << "\n\n";
//				if ( interval < schOfDef - firstOfThisPhiUser + 1 )
//					interval = schOfDef - firstOfThisPhiUser + 1;
			}
		}

		//TODO: XXX:
		//If the chaining operation between iteration operators,
		//You can use intervalOffset -1 instead of intervalOffset itself.
		return intervalOffset;
	}

	void SDCSchedulingBuilder::tableInit(ScheduleInfo *schedule, unsigned interval,
																			PipelineState *state, RAM_ *addressedRAM){
		modTable.clear();
		addTable.clear();

		Function *func = state->getParentFunction();

		for ( auto ram : memoryTable->getAccessRAMFromFunction(func) )
			for ( auto inst : memoryTable->getPrivateAccessInst(func, ram) )
			{
				if ( !state->isInThisState(inst) )
					continue;

				unsigned sch = (int)( schedule->getSSchFromInst(inst) + 0.1 );
				(modTable[sch % interval])[ram].push_back(inst);
			}

		for ( auto inst : memoryTable->getAddressAccessInst(func) )
		{
			if ( !state->isInThisState(inst) )
				continue;

			unsigned sch = (int)( schedule->getSSchFromInst(inst) + 0.1 );
			(modTable[sch % interval])[addressedRAM].push_back(inst);
		}

		for ( auto inst : memoryTable->getUnresolvedInst(func) )
		{
			if ( !state->isInThisState(inst) )
				continue;

			unsigned sch = (int)( schedule->getSSchFromInst(inst) + 0.1 );
			(modTable[sch % interval])[addressedRAM].push_back(inst);
		}
	}

	//TODO: now the targets that will be re-scheduled are selected heuristic-ly
	list<Instruction *> SDCSchedulingBuilder::getTarget(list<Instruction *> &userList) {
		list<Instruction *> targetList;
		targetList.clear();
		auto iter = userList.end();
		for ( unsigned i = 0; i < userList.size() -2; ++i ) {
			Instruction *inst = *(--iter);
			targetList.push_front(inst);
		}
		return targetList;
	}

	bool SDCSchedulingBuilder::checkII(PipelineState *state, unsigned interval) {
		for ( unsigned i = 0; i < interval; ++i )
			for ( auto memTable : modTable[i] ) {
				RAM_ *mem = memTable.first;
				list<Instruction *> userList = memTable.second;
				if ( 2 < userList.size() )
					for ( auto inst : getTarget(userList) ) { //find empty space for instruction
						bool findSpace = false; //not add interval
						//check empty space in other inetrval
						for ( unsigned j = 0; j < interval; ++j ) {
							if ( i == j ) continue;
							list<Instruction *> otherList = (modTable[j])[mem];
							list<Instruction *> addList = (addTable[j])[mem];
							if ( 2 <= otherList.size() + addList.size() )
								continue;
							else {
								(addTable[j])[mem].push_back(inst);
								findSpace = true;
								break;
							}
						}

						if ( !findSpace )
							return false;
					}
			}
		return true;
	}

	void SDCSchedulingBuilder::verifyII(PipelineState *state,
																ScheduleInfo *schedule, unsigned interval) {
		//Initialization of additional interval
		addInterval = 0;
		//This RAM represents globalMemory
//		RAM *globalMem = mm->getRAMList().front();
		RAM_ *addressedRAM;
		Function *func = state->getParentFunction();
		if ( (memoryTable->getAddressAccessRAMFromFunction(func)).size() == 0 )
			addressedRAM = NULL;
		else
			addressedRAM = *((memoryTable->getAddressAccessRAMFromFunction(func)).begin());

		while(1) {
			//Initialization of resource table
			tableInit(schedule, interval + addInterval, state, addressedRAM);

			//checkII is fine now
			if ( checkII(state, interval+addInterval) )
				break;
			else
				++addInterval;
		}
	}

	bool SDCSchedulingBuilder::addTableEmpty(void) {
		errs() << "Empty Table Check : \n";
		if ( addTable.empty() )
			return true;
		for ( auto iter : addTable ) {
			errs() << iter.first << " addTable Iteration\n";
			if ( (iter.second).empty() )
				continue;
			for ( auto memIter : iter.second ) {
				errs() << (memIter.first)->getRAMId() << " : ";
				if ( (memIter.second).size() != 0 ) {
					for ( auto inst : memIter.second )
						errs() << *inst << "\n";
					return false;
				}
			}
		}
		return true;
	}	

	ScheduleInfo *SDCSchedulingBuilder::getSDCSchedulingForPipeline(void){

		assert(pState);
		for ( auto bi : pState->getBB() )
			bi->dump();


		ScheduleInfo *schedule_1 = new ScheduleInfo();
		assert(schedule_1);

		initLPSolver(pState, verilogConfigInfo, schedule_1);
		assert(lp);
		errs() << "SCHEDULING PIPELING : " << schedule_1->getNumVar() << "\n";
		multiCycleConstraints(schedule_1);
		addBranchConstraints(pState, schedule_1, verilogConfigInfo);
		addRegMemDepConstraintsForPipeline(pState, bbcdfg, operatorTable,
																			verilogConfigInfo, schedule_1);
		solveWithASAP(schedule_1);

		ScheduleInfo *schedule_2 = new ScheduleInfo();
		initLPSolver(state, verilogConfigInfo, schedule_2);
		assert(lp);
		multiCycleConstraints(schedule_2);
		addBranchConstraints(pState, schedule_2, verilogConfigInfo);
		addRegMemDepConstraintsForPipeline(pState, bbcdfg, operatorTable,
																			verilogConfigInfo, schedule_2);
		//ADD Resource & Chaining Restrictions
		addResourceRestrictions(pState, bbcdfg, verilogConfigInfo, schedule_1, schedule_2,
				memoryTable, operatorTable);
		solveWithASAP(schedule_2);

		LoopAliasInfo *lp = pState->getLA();
		list<Instruction *> exitList = lp->getExitList(); //include Branch && PHI

		//Exit Instruction Check
		//assert; this should be determined before building fsm
		for ( auto inst : exitList ) {
			if ( isa<BranchInst>(inst) )
				continue;

			unsigned sch = (int)( schedule_2->getSSchFromInst(inst) + 0.1 );

			errs() << "Exit Check : " << *inst << " : " << sch << "\n";
			if ( sch != 0 )
				assert(0 && "Instructions related to ExitCond should have 0 sch\n");
		}

		free(schedule_1);
		schedule_1 = schedule_2;

		unsigned intervalOffset = 0;
		unsigned interval = 1;
		//II Calculation
		while (1) {
			//XXX: II is just for verilog print.
			//II means the interval between iterations
			//II check - Registers Dependence
			unsigned interval_reg = getIIReg(pState, schedule_1);

			//II check - Memory Dependence
			unsigned interval_mem = getIIMem(pState, schedule_1);

			if ( interval_reg < interval_mem ){
				if ( intervalOffset < interval_mem )
					intervalOffset = interval_mem;
			}
			else{
				if ( intervalOffset < interval_reg )
					intervalOffset = interval_reg;
			}
			interval = intervalOffset + 1; //basic pipeline interval is 1

			errs() << "Pipe Scheduling : interval : " << interval << "\n\n";

			//Verilfy II on the presence of memory resource limitation
			verifyII(pState, schedule_1, interval);
			if ( addTableEmpty() ) {
				pState->setII(interval + addInterval);
				errs() << "Pipe Scheduling Interval " << interval + addInterval << "\n";
				break;
			}
			errs() << "Pipe Scheduling Interval " << interval + addInterval << "\n";


			schedule_2 = new ScheduleInfo();
			initLPSolver(pState, verilogConfigInfo, schedule_2);
			assert(lp);
			multiCycleConstraints(schedule_2);
			addBranchConstraints(pState, schedule_2, verilogConfigInfo);
			addRegMemDepConstraintsForPipeline(pState, bbcdfg, operatorTable,
																				verilogConfigInfo, schedule_2);
			addResourceRestrictions(pState, bbcdfg, verilogConfigInfo, schedule_1, schedule_2,
					memoryTable, operatorTable);
			//Add constraints
			addPipelineConstraints(schedule_1, schedule_2, addTable, interval+addInterval);
			solveWithASAP(schedule_2);

			free(schedule_1);
			schedule_1 = schedule_2;
		}

		printSchedule(schedule_1);

		return schedule_1;
//		return NULL;
	}

	ScheduleInfo *SDCSchedulingBuilder::getSDCScheduling(void){

		if ( state->getInstList().size() == 0 )
			return NULL;
		
		ScheduleInfo *schedule_1 = new ScheduleInfo();

		assert(schedule_1);

		initLPSolver(state, verilogConfigInfo, schedule_1);
		assert(lp);
		multiCycleConstraints(schedule_1);
		addRegMemDepConstraints(state, bbcdfg, operatorTable, 
														verilogConfigInfo, schedule_1, bitWidthTable);	
		solveWithASAP(schedule_1);

		//Add Chaining Constraints

		ScheduleInfo *schedule_2 = new ScheduleInfo();
		initLPSolver(state, verilogConfigInfo, schedule_2);
		assert(lp);
		multiCycleConstraints(schedule_2);
		addRegMemDepConstraints(state, bbcdfg, operatorTable, 
														verilogConfigInfo, schedule_2, bitWidthTable);
		//ADD Resource & Chaining Restrictions
		addResourceRestrictions(state, bbcdfg, verilogConfigInfo, schedule_1, schedule_2,
				memoryTable, operatorTable);
		solveWithASAP(schedule_2);

		free(schedule_1);

		printSchedule(schedule_2);

		return schedule_2;
	}

	void SDCSchedulingBuilder::printSchedule(ScheduleInfo *schedule)
	{
		*scheduleFile << "Function : " << state->getParentFunction()->getName() << "\n";
		*scheduleFile << "State ID : " << state->getStateNumber() << "\n";
		*scheduleFile << "Exit Cond : " << *(state->getExitInst()) << "\n\n";

		for ( auto inst : state->getInstList() )
		{
			unsigned start = schedule->getIndexFromInst(inst).first;
			unsigned end = schedule->getIndexFromInst(inst).second;
			double v_s = schedule->getSchFromIndex(start);
			double v_e = schedule->getSchFromIndex(end);
			*scheduleFile << *inst 
				<< "\n\t start | end : " << start << " | " << end
				<< "\n\t var : " << v_s  << " | " << v_e << "\n\n";
		}
		*scheduleFile << "\n";
	}

} // end namespace

