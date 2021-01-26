#ifndef SDCScheduling_H
#define SDCScheduling_H

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"

#include "BBCDFG.h"
#include "VerilogConfig.h"
//#include "MemoryModule.h"
#include "MemoryTable.h"
#include "OperatorTable.h"
#include "BitWidthTable.h"

#include <utility>

namespace corelab
{
using namespace llvm;
	
	typedef DenseMap<Instruction *, std::pair<unsigned, unsigned>> Inst2Index;
	typedef DenseMap<unsigned, double> Index2Sch;
	typedef DenseMap<RAM_ *, list<Instruction *>> MemTable;

	class State;
	class FSM;
	class PipelineState;

	class ScheduleInfo
	{
		public:
			void setNumVar(unsigned numVar_) { numVar = numVar_; };
			unsigned getNumVar(void) { return numVar; };

			void setInst2Index(Instruction *inst, unsigned start, unsigned end){
				inst2Index[inst] = std::make_pair(start, end);
			};

			void setIndex2Sch(unsigned index, double sch){
				index2Sch[index] = sch;
			};

			std::pair<unsigned, unsigned> getIndexFromInst(Instruction *inst){
				return inst2Index[inst]; 
			};

			bool isInstInState(Instruction *inst){
				if ( inst2Index.count(inst) )
					return true;
				else
					return false;
				/*
				if ( inst2Index.find(inst) == inst2Index.end() )
					return false;
				else
					return true;*/
			}

			double getSchFromIndex(unsigned index) { return index2Sch[index]; };

			double getSSchFromInst(Instruction *inst) { return index2Sch[ inst2Index[inst].first]; };
			double getESchFromInst(Instruction *inst) { return index2Sch[ inst2Index[inst].second]; };

			Inst2Index getInst2Index(void) { return inst2Index; };
			Index2Sch getIndex2Sche(void) { return index2Sch; };

		private:
			Inst2Index inst2Index;
			Index2Sch index2Sch;
			unsigned numVar;
	};

	class SDCSchedulingBuilder
	{
		public:
			SDCSchedulingBuilder(BBCDFG *bbcdfg_, VerilogConfigInfo *verilogConfigInfo_,
					State *state_, PipelineState *pState_, raw_fd_ostream *scheduleFile_,
					MemoryTable *memoryTable_, OperatorTable *operatorTable_,
					BitWidthTable *bitWidthTable_)
				: bbcdfg(bbcdfg_), verilogConfigInfo(verilogConfigInfo_), 
				state(state_), pState(pState_), scheduleFile(scheduleFile_),
				memoryTable(memoryTable_), operatorTable(operatorTable_),
				bitWidthTable(bitWidthTable_) {}
			~SDCSchedulingBuilder() {}

			ScheduleInfo *getSDCScheduling(void);
			ScheduleInfo *getSDCSchedulingForPipeline(void);

			unsigned getIIReg(PipelineState *, ScheduleInfo *);
			unsigned getIIMem(PipelineState *, ScheduleInfo *);

			void tableInit(ScheduleInfo *, unsigned, PipelineState *, RAM_ *);
			list<Instruction *> getTarget(list<Instruction *> &);
			bool checkII(PipelineState *, unsigned );
			void verifyII(PipelineState *, ScheduleInfo *, unsigned);
			bool addTableEmpty();

			void printSchedule(ScheduleInfo *);

		private:

			BBCDFG *bbcdfg;
			VerilogConfigInfo *verilogConfigInfo;
			State *state;
			PipelineState *pState;
			raw_fd_ostream *scheduleFile;
//			MemoryModule *mm;
			MemoryTable *memoryTable;
			OperatorTable *operatorTable;
			BitWidthTable *bitWidthTable;

			ScheduleInfo *schedule;

			DenseMap<unsigned, MemTable> modTable;
			DenseMap<unsigned, MemTable> addTable; //rescheduled value
			unsigned addInterval;
	};

}// end namespace


#endif
