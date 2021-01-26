#ifndef FSM_H
#define FSM_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"

#include "SDCScheduling.h"
//#include "ModuleStructure.h"
//#include "MemoryModule.h"
#include "MemoryTable.h"
#include "CallTable.h"
#include "OperatorTable.h"
#include "BitWidthTable.h"
#include "LoopPatternAnal.h"

#include <list>
#include <set>

namespace corelab
{
	using namespace llvm;
	using namespace std;

	class State;
	class FunctionSM;
	class FSM;

	typedef list<State *> PrevNextState;
	typedef DenseMap<unsigned, set<Instruction *>> ScheduleMap;

	class State
	{
		public:
			State (Function *fnc_, unsigned stateNumber_) : fnc(fnc_), stateNumber(stateNumber_) {
				pipeline = false;
				hasParallelCall = false;
			}
			virtual ~State() {}

			bool isPipelineState(void) { return pipeline; };
			unsigned getStateNumber(void) { return stateNumber; };

			void setScheduleMap (unsigned timing, Instruction *inst) 
			{ 
				scheduleMap[timing].insert(inst);
				inst2Sch[inst] = timing;
			};
			ScheduleMap getScheduleMap (void) { return scheduleMap; };
			unsigned getSchFromInst (Instruction *inst) 
			{ 
				if ( exitI == inst )
					return latency -1;
				return inst2Sch[inst]; 
			};
			void setEScheduleMap (unsigned timing, Instruction *inst) 
			{ 
				inst2ESch[inst] = timing;
			};
			unsigned getESchFromInst (Instruction *inst) 
			{ 
				if ( exitI == inst )
					return latency -1;
				return inst2ESch[inst]; 
			};

			bool isInThisState(Instruction *inst) {
				for ( auto iter : instList )
					if ( inst == iter )
						return true;
				return false;
			}

			bool isCallExitTime(unsigned timing) {
				return (latency-1==timing) && (isa<CallInst>(exitI)||isa<InvokeInst>(exitI));
			}

			void setLoadPortNum (Instruction *inst, bool isPortA) 
			{ loadPortNum[inst] = isPortA; };
			bool getLoadPortNum (Instruction *inst) { return loadPortNum[inst]; };

			void setLatency(unsigned lt) { latency = lt; };
			unsigned getLatency(void) { return latency; };
			void setMaxStartV(unsigned v) { maxStartV = v; };
			unsigned getMaxStartV(void) { return maxStartV; };
			void setMaxDelay(double d) { maxDelay = d; };
			double getMaxDelay(void) { return maxDelay; };

			void pushInst2List (Instruction *inst) { instList.push_back(inst); };
			list<Instruction *> getInstList(void) { return instList; };

			Function *getParentFunction (void) { return fnc; };

			bool isParallelCallSite(CallInst *callInst) {
				for ( auto callIter : parallelCallSiteSet )
					if ( callIter == callInst )
						return true;
				return false;
			}

			set<CallInst *> getParallelCallSite() { return parallelCallSiteSet; }
			set<CallInst *> getParallelCallSiteTime(unsigned i) { return time2parallelSet[i]; }

			///////////////////////////////////////////////////////

			void addPrev (State *state) { prev.push_back(state); };
			void addNext (State *state) { next.push_back(state); };
			PrevNextState getPrev (void) { return prev; };
			PrevNextState getNext (void) { return next; };

			void setEnterInst (Instruction *inst) { enterI = inst; };
			void setExitInst (Instruction *inst) { exitI = inst; };
			Instruction *getEnterInst (void) { return enterI; };
			Instruction *getExitInst (void) { return exitI; };

			void setIsUsedAT (Instruction *inst, bool reg) { isUsedAnotherTiming[inst] = reg; };
			bool isUsedAT (Instruction *inst) { return isUsedAnotherTiming[inst]; };

			void setNoNameInstNum (Instruction *inst, unsigned i) { noNameInstNum[inst] = i; };
			unsigned getNoNameInstNum (Instruction *inst) { return noNameInstNum[inst]; };

			//pipeline
			void setPipelineState(bool p) { pipeline = p; };

			void addParallelCallSite(CallInst *callInst) {
				parallelCallSiteSet.insert(callInst);
				hasParallelCall = true;
			}

			void setParallelCallSiteTime(unsigned i, set<CallInst *> &set) { 
				time2parallelSet[i] = set; 
			}

		private:
			unsigned stateNumber;
			unsigned latency;
			unsigned maxStartV;
			double maxDelay;

			bool pipeline;
			bool hasParallelCall;

			ScheduleMap scheduleMap;
			DenseMap<Instruction *, unsigned> inst2Sch;
			DenseMap<Instruction *, unsigned> inst2ESch;
			DenseMap<Instruction *, bool> isUsedAnotherTiming;
			DenseMap<Instruction *, unsigned> noNameInstNum;	// assign load ID
			DenseMap<Instruction *, bool> loadPortNum; // assign load Port Number
			list<Instruction *> instList;

			//Parallel CallSite Instructions
			set<CallInst *> parallelCallSiteSet;
			DenseMap<unsigned, set<CallInst *>> time2parallelSet;

			Function *fnc;

			PrevNextState prev;
			//if exit Instruction is callInst, there should be two states in next list.
			//front is called function, back is returned state.
			PrevNextState next;
			Instruction *enterI;
			Instruction *exitI;
	};

	class PipelineState : public State 
	{
		public:
			PipelineState(Function *fnc_, unsigned stateNumber_, LoopAliasInfo *la_) 
				: State(fnc_, stateNumber_), la(la_) {
					if ( la->getLoopType() == LoopAliasInfo::NORMAL )
						loopType = 1;
					else if ( la->getLoopType() == LoopAliasInfo::EDGE )
						loopType = 2;
					else if ( la->getLoopType() == LoopAliasInfo::PREHEADER )
						loopType = 3;
				}

			unsigned getII(void) { return interval; };
			LoopAliasInfo *getLA(void) { return la; };
			list<BasicBlock *> getBB(void) { return bbList; };
			bool isInThisPipeline(BasicBlock *bb) {
				for ( auto bi : bbList )
					if ( bb == bi )
						return true;
				return false;
			}

			unsigned getType(void) { return loopType; };
			unsigned getMaxEnd(void) { return maxEnd; };
			unsigned getExitPoint(void) { return exitPoint; };
			bool isUsedInThisStage(unsigned userSch, Instruction *inst) { 
				for ( auto ii : usedMap[userSch] )
					if ( ii == inst )
						return true;
				return false;
			}
			bool isUsedInPHINode(unsigned mySch, Instruction *inst) {
				for ( auto ii : phiUsedMap[mySch] )
					if ( ii == inst )
						return true;
				return false;
			}
			Instruction *getPHISubstitute(Instruction *phi) { return phi2Def[phi]; };

			// ------build up ------- //
			void addBasicBlock(BasicBlock *bb) { bbList.push_back(bb); };
			void setII(unsigned i) { interval = i; };
			void setMaxEnd(unsigned maxEnd_) { maxEnd = maxEnd_; };
			void setExitPoint(unsigned exit_) { exitPoint = exit_; };
			void setUsedMap(unsigned userSch, Instruction *inst) { 
				usedMap[userSch].push_back(inst); 
			}
			void setPHIUsedMap(unsigned mySch, Instruction *inst) {
				phiUsedMap[mySch].push_back(inst);
			}
			void setPHISubstitute(Instruction *phi, Instruction *sub) {
				phi2Def[phi] = sub;
			}

		private:
			LoopAliasInfo *la;
			list<BasicBlock *> bbList;

			DenseMap<unsigned, list<Instruction *>> usedMap;
			DenseMap<unsigned, list<Instruction *>> phiUsedMap;
			DenseMap<Instruction *, Instruction *> phi2Def;

			unsigned loopType;
			unsigned interval;
			unsigned maxEnd;
			unsigned exitPoint;
	};

	class FSM
	{
		public:
			void setInst2StateMap (Instruction *inst, State *state) { inst2State[inst] = state; };
			State *getStateFromInst (Instruction *inst) { return inst2State[inst]; };

			void setFnc2StartState (Function *fnc, State *state) { fnc2StartState[fnc] = state; };
			State *getStartStateFromFnc (Function *fnc) { return fnc2StartState[fnc]; };
			
			void pushState2List (State *state) { stateList.push_back(state); };
			list<State *> getStateList(void) { return stateList; };

			void setFnc2StateList (Function *fnc, State *state)
			{ fnc2StateList[fnc].push_back(state); };
			list<State *> getStateListOfFunction(Function *fnc) { return fnc2StateList[fnc]; };

			void setStartState (State *state) { startState = state; };
			void setEndState (State *state) { endState = state; };
			State *getStartState (void) { return startState; };
			State *getEndState (void) { return endState; };

			//For pipeline
			list<PipelineState *> getPipelineStateList(void) { return pStateList; };
			void pushPipelineState2List(PipelineState *state) { pStateList.push_back(state); };
			PipelineState *getPipelineState(State *state) { return state2Pipeline[state]; };
			void setState2Pipeline(State *state, PipelineState *pState) { 
				state2Pipeline[state] = pState; };

			void printFSM (void);

		private:
			Module *module;

			DenseMap<Function *, State *> fnc2StartState;

			DenseMap<Instruction *, State *> inst2State;
			list<State *> stateList;

			DenseMap<Function *, list<State *>> fnc2StateList;

			//For Pipeline
			list<PipelineState *> pStateList;
			DenseMap<State *, PipelineState *> state2Pipeline;
			//DenseMap<BasicBlock *, PipelineState *>
			//DenseMap<LoopAliasInfo *, PipelineState *>
			
			State *startState;
			State *endState;

			float clock_period;
	};

//cycle->instruction
//clock speed 조정
//eachFncSM -> unsigned number ( How to do labeling for FunctionSM )
//첫 instruction에 numbering을 하고 그 다음에 FncSM을 하나씩 넣어감??

	class FSMBuilder
	{
		public:
			FSMBuilder(BBCDFG *bbcdfg_, VerilogConfigInfo *verilogConfigInfo_,
					Module *module_, MemoryTable *memoryTable_, CallTable *callTable_,
					OperatorTable *operatorTable_, BitWidthTable *bitWidthTable_,
					LoopPatternAnal *lpa_)
				:	bbcdfg(bbcdfg_), verilogConfigInfo(verilogConfigInfo_), 
				module(module_), memoryTable(memoryTable_), callTable(callTable_),
				operatorTable(operatorTable_), bitWidthTable(bitWidthTable_), lpa(lpa_) {}
			~FSMBuilder() {}

			FSM *getFSM(void);

			void setFSM(FSM *);
			void connectStates(FSM *);

			void portAssignment(FSM *, list<Instruction *>, bool, Function *);
			void setScheduleForState(FSM *);
			void setState(FSM *);
			void setFinalInstructions(FSM *);

			void setState2ScheduleInfo(State *state, ScheduleInfo *scheduleInfo)
			{ state2ScheduleInfo[state] = scheduleInfo; };

			bool checkEndPointStoreInst(StoreInst *, State *, FSM *);
			bool thisCallUseSameObject(StoreInst *, Function *, FSM *);

			unsigned getMetadata(Instruction *);
			
		private:
			BBCDFG *bbcdfg;
			VerilogConfigInfo *verilogConfigInfo;
			Module *module;
//			MemoryModule *mm;
//			ModuleStructure *ms;
			MemoryTable *memoryTable;
			CallTable *callTable;
			OperatorTable *operatorTable;
			BitWidthTable *bitWidthTable;
			LoopPatternAnal *lpa;

			DenseMap<State *, ScheduleInfo *> state2ScheduleInfo;
	};

}//end namespace


#endif
