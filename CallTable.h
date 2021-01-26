#ifndef CORELAB_CALL_TABLE_H
#define CORELAB_CALL_TABLE_H

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

	class CallTable
	{
		public:
			CallTable() {
				maxArgNum = 0;
				maxBitWidth = 0;
			}
			~CallTable() {}
			// --------- Method -------- //
			bool isCallBus(void) { return callBus; }
			unsigned getFuncId(Function *func) { return func2id[func]; }
			unsigned getFuncSize(void) { return funcSize; }
			unsigned getFuncBitSize(void) { return funcBitSize; }

			unsigned getMaxArgNum(void) { return maxArgNum; }
			unsigned getMaxReturnBitWidth(void) { return maxBitWidth; }
			unsigned getArgBitWidth(unsigned order) { return argOrder2bitwidth[order]; }

			set<Function *> getCalleeFromFunction(Function *func) { return func2callee[func]; }
			set<Function *> getCallerFromFunction(Function *func) { return func2caller[func]; }

			list<Instruction *> getCallInstFromFunction(Function *func, Function *callee)
			{ return (func2callee2inst[func])[callee]; }

			unsigned getCallId(Function *func, Instruction *inst) { return (call2id[func])[inst]; }

			unsigned getFuncArgBit(Function *func, unsigned arg) {
				if ( (func2arg2bit[func]).count(arg) )
					return (func2arg2bit[func])[arg];
				else {
					errs() << func->getName() << "\n";
					assert(0);
				}
			}

			unsigned getFuncReturnBit(Function *func) { 
				if ( func2returnBit.count(func) )
					return func2returnBit[func]; 
				else {
					errs() << func->getName() << "\n";
					assert(0);
				}
			}

			// --------- build -------- //
			void setCallBus(bool callBus_) { callBus = callBus_; }

			void setFuncId(Function *func, unsigned id) { func2id[func] = id; }
			void setFuncSize(unsigned n) { funcSize = n; }
			void setFuncBitSize(unsigned n) { funcBitSize = n; }

			void setMaxArgBitWidth(unsigned argOrder, unsigned bitwidth) {
				if ( argOrder2bitwidth.count(argOrder) ) {
					if ( argOrder2bitwidth[argOrder] < bitwidth )
						argOrder2bitwidth[argOrder] = bitwidth;
				}
				else
					argOrder2bitwidth[argOrder] = bitwidth;
			}

			void setMaxArgNum(unsigned n) {
				if ( maxArgNum < n )
					maxArgNum = n;
			}

			void setMaxBitWidth(unsigned n) {
				if ( maxBitWidth < n )
					maxBitWidth = n;
			}

			void addCalleeFunction(Function *caller, Function *callee)
			{ func2callee[caller].insert(callee); }

			void addCallerFunction(Function *caller, Function *callee)
			{ func2caller[callee].insert(caller); }

			void addCallInst(Function *caller, Function *callee, Instruction *inst)
			{ (func2callee2inst[caller])[callee].push_back(inst); }

			void setCallInstId(Function *caller, Instruction *callInst, unsigned callId)
			{ (call2id[caller])[callInst] = callId; }

			void setFuncArgBit(Function *func, unsigned arg, unsigned bit) {
				if ( !func2arg2bit.count(func) )
					(func2arg2bit[func])[arg] = bit; 
				else if ( !(func2arg2bit[func]).count(arg) )
					(func2arg2bit[func])[arg] = bit; 
				else {
					if ( (func2arg2bit[func])[arg] < bit )
						(func2arg2bit[func])[arg] = bit; 
				}
			}

			void setFuncReturnBit(Function *func, unsigned bit) {
				if ( !func2returnBit.count(func) )
					func2returnBit[func] = bit;
				else {
					if ( func2returnBit[func] < bit )
						func2returnBit[func] = bit;					
				}
			}

		private:
			unsigned funcSize;
			unsigned funcBitSize;
			DenseMap<Function *, unsigned> func2id;

			DenseMap<Function *, set<Function *>> func2callee;
			DenseMap<Function *, set<Function *>> func2caller;

			DenseMap<Function *, DenseMap<Function *, list<Instruction *>>> func2callee2inst;

			DenseMap<Function *, DenseMap<Instruction *, unsigned>> call2id;

			DenseMap<Function *, DenseMap<unsigned, unsigned>> func2arg2bit;
			DenseMap<Function *, unsigned> func2returnBit;

			bool callBus;

			//For Call Bus
			unsigned maxArgNum;
			unsigned maxBitWidth;
			DenseMap<unsigned, unsigned> argOrder2bitwidth;
	};

	class CallTableBuilder
	{
		public:
			CallTableBuilder(Module *module_, VerilogConfigInfo *verilogConfigInfo_);
			~CallTableBuilder() {}

			CallTable *getCallTable() { return callTable; }
			void setBitWidth(unsigned);

		private:
			CallTable *callTable;

			Module *module;
			VerilogConfigInfo *verilogConfigInfo;
	};
}

#endif
