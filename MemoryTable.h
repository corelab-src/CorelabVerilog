#ifndef CORELAB_MEMORY_TABLE_H
#define CORELAB_MEMORY_TABLE_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"

#include "VerilogConfig.h"
#include "GetMemOper.h"
#include "PABuilder.h"
#include "CallTable.h"

#include <list>
#include <set>

namespace corelab
{
	using namespace llvm;
	using namespace std;

	class RAM_
	{
		public:
			RAM_(Value *, DataLayout *, bool);
			~RAM_() {}

			Value *getValue(void) { return v; }
			unsigned getNumOfElements(void) { return numOfElements; }
			unsigned getElementBitSize(void) { return elementBitSize; } //address size(+byte addressed)
			unsigned getDataBitSize(void) { return dataBitSize; } //databitsize for function
			unsigned getTotalBitSize(void) { return totalBit; }
			bool isStruct(void) { return structure; }
			bool isReadOnly(void) { return readOnly; }

			void setRAMId(unsigned id) { ramId = id; }
			unsigned getRAMId(void) { return ramId; }

			void setInitId(unsigned id) { initId = id; }
			unsigned getInitId(void) { return initId; }

			//Only For NumElement = 1 && Private Reg
			void setInitData(uint64_t data) { initData = data; }
			uint64_t getInitData(void) { return initData; }

			Type *getETy(void) { return eTy; }
			Value *getV(void) { return v; }

			set<unsigned> getPossibleBitWidth(void) { return possibleBitWidth; }
			unsigned getMemBitWidth(void) { return memBitWidth; }
			unsigned getNumOfBlock(void) { return numOfBlock; }

			bool hasMultipleSize(void) { return ( possibleBitWidth.size() == 1 ) ? false : true; }
			bool hasExPort(void) { return exPortUse; } 

			//-------BUILD--------//
			void addPossibleBitWidth(unsigned bw) { possibleBitWidth.insert(bw); }
			void setMemBitWidth(unsigned bw) { memBitWidth = bw; }
			void setNumOfBlock(unsigned n) { numOfBlock = n; }
//			void setByteAddressedRAM(bool ba) { byteAddressedRAM = ba; }
//			void setValidSignal(bool vs) { validSignal = vs; }

			void setElementBitSize(unsigned ebs) { elementBitSize = ebs; }
			void setDataBitSize(unsigned dbw) { dataBitSize = dbw; }

			void setReadOnly(bool no) { readOnly = no; }

			void setExPortUse(bool use) { exPortUse = use; }

		private:
			void ramInitializer(DataLayout *);

			Value *v;

			Type *ty;
			Type *eTy;

			unsigned ramId;
			unsigned initId;
			//Only For NumElements = 1
			uint64_t initData;

			unsigned numOfElements;
			unsigned elementBitSize;
			unsigned dataBitSize; // bitwidth interface for FSM function
			unsigned totalBit;

			bool structure;
			bool readOnly;

			bool compactStruct;

			set<unsigned> possibleBitWidth;
			unsigned memBitWidth; //BRAM block chunk
			unsigned numOfBlock;

			bool multipleSize;
			bool exPortUse;
	};

	class MemoryTable
	{
		public:
			// ------- Method -------- //
			unsigned getRAMSize(void) { return ramSize; }
			unsigned getRAMBitSize(void) { return ramBitSize; }
//			unsigned getElementSize(void) { return elementSize; }
			unsigned getElementBitSize(void) { return elementBitSize; }
			unsigned getBitWidth(void) { return bitWidth; } // possible Max
			unsigned getMemBitWidth(void) { return memBitWidth; }

			unsigned getPointerBitSize(void) { return pointerBitSize; }
			bool isByteAddressedRAM(void) { return byteAddressedRAM; }

			RAM_ *getRAMFromValue(Value *v) { return value2RAM[v]; }
			list<RAM_ *> getAllRAMList(void) { return ramList; }
			set<RAM_ *> getAddressAccessRAMList(void) { return addressAccessRAMList; }

			set<RAM_ *> getAccessRAMFromFunction(Function *func) { return accessRAM[func]; }
			set<RAM_ *> getAddressAccessRAMFromFunction(Function *func)
			{ return addressAccessRAM[func]; }
			
			list<Instruction *> getPrivateAccessInst(Function *func, RAM_ *ram)
			{ return (privateAccessInst[func])[ram]; }
			list<Instruction *> getAddressAccessInst(Function *func)
			{ return addressAccessInst[func]; }
			list<Instruction *> getUnresolvedInst(Function *func)
			{ return unresolvedInst[func]; }

			set<Function *> getFunctionFromRAM(RAM_ *ram) { return ram2func[ram]; }

			bool isCompactStruct(Value *v) { 
				if ( memory2compact.count(v) )
					return memory2compact[v]; 
				else
					return false;
			}

			bool isDualPortRAM(RAM_ *ram) {
				if ( !dualPortRAMs.count(ram) )
					return false;
				else
					return dualPortRAMs[ram];
			}

			unsigned getGEPAddrSize(GetElementPtrInst *gep) {
				if ( gep2addrsize.count(gep) )
						return gep2addrsize[gep];
				else {
					gep->dump();
					assert(0);
				}
			}

			// ------- build ------- //
			void setValue2RAM(Value *v, RAM_ *ram) { value2RAM[v]=ram; }
			void setRAMList(list<RAM_ *> ramList_) { ramList = ramList_; }

			void addAccessRAM(Function *func, RAM_ *ram) { accessRAM[func].insert(ram); }
			void addAddressAccessRAM(Function *func, RAM_ *ram) { addressAccessRAM[func].insert(ram); }
			void addPrivateAccessInst(Function *func, RAM_ *ram, Instruction *inst)
			{ (privateAccessInst[func])[ram].push_back(inst); }
			void addAddressAccessInst(Function *func, Instruction *inst)
			{ addressAccessInst[func].push_back(inst); }
			void addUnresolvedInst(Function *func, Instruction *inst)
			{ unresolvedInst[func].push_back(inst); }

			void addAddressAccessRAMList(RAM_ *ram) { addressAccessRAMList.insert(ram); }

			void addRAMAccessedByFunction(RAM_ *ram, Function *func) { ram2func[ram].insert(func); }

			void setRAMSize(unsigned n) { ramSize = n; }
			void setRAMBitSize(unsigned n) { ramBitSize = n; }
			void setElementSize(unsigned n) { elementSize = n; }
			void setElementBitSize(unsigned n) { elementBitSize = n; }
			void setBitWidth(unsigned n) { bitWidth = n; }
			void setMemBitWidth(unsigned n) { memBitWidth = n; }

			void setPointerBitSize(unsigned n) { pointerBitSize = n; }
			void setByteAddressedRAM(bool ba) { byteAddressedRAM = ba; }

			bool hasStruct(Value *v) {
				if ( memory2compact.count(v) )
					return true;
				else
					return false;
			}
			void setCompactStruct(Value *v, bool compact) { memory2compact[v] = compact; }
			void setNoCompactStruct() { memory2compact.clear(); }

			void addDualPortRAM(RAM_ *ram) { dualPortRAMs[ram] = true; }

			void setGEPAddrSize(GetElementPtrInst *gep, unsigned addrSize)
			{ gep2addrsize[gep] = addrSize; }

		private:
			unsigned ramSize;
			unsigned ramBitSize;
			unsigned elementSize;
			unsigned elementBitSize;
			unsigned bitWidth;
			unsigned memBitWidth;

			unsigned pointerBitSize;

			bool byteAddressedRAM;

			list<RAM_ *> ramList;

			//LLVM Value -> RAM structure
			DenseMap<Value *, RAM_ *> value2RAM;

			DenseMap<Function *, set<RAM_ *>> accessRAM; // this is for connection
			DenseMap<Function *, set<RAM_ *>> addressAccessRAM; // this is for always block

			DenseMap<Function *, DenseMap<RAM_ *, list<Instruction *>>> privateAccessInst;
			DenseMap<Function *, list<Instruction *>> addressAccessInst;
			DenseMap<Function *, list<Instruction *>> unresolvedInst;

			DenseMap<Value *, bool> memory2compact;

			//This is for memory address space define
			set<RAM_ *> addressAccessRAMList;

			DenseMap<RAM_ *, set<Function *>> ram2func;

			DenseMap<RAM_ *, bool> dualPortRAMs;

			DenseMap<GetElementPtrInst *, unsigned> gep2addrsize;
	};

	class MemoryTableBuilder
	{
		public:
			MemoryTableBuilder(Module *module_, DataLayout *DL_, PADriver *pa_,
					VerilogConfigInfo *verilogConfigInfo_, CallTable *callTable_);
			~MemoryTableBuilder() {}
			
			MemoryTable *getMemoryTable() { return memoryTable; }

		private:
			void searchCompactStruct(MemoryTable *);
			bool hasStruct(Type *);

			void searchGV(MemoryTable *);
			void searchLocalAllocaVariable(MemoryTable *);

			void genMemoryTable(MemoryTable *);
			void genCentralizedMemoryTable(MemoryTable *);

			void setAddressSpace(MemoryTable *);

			void setGEPAddrSize(MemoryTable *);
			void setCentralizedGEPAddrSize(MemoryTable *);

			void targetFunctionSetting(Function *func) {
				targetFuncSet.insert(func);
				for ( auto callee : callTable->getCalleeFromFunction(func) )
					targetFunctionSetting(callee);
			}

			//should be used after genMemoryTable done
			bool isUsedInMemFunc(RAM_ *ram) {
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

			MemoryTable *memoryTable;
			list<RAM_ *> ramList;

			set<Function *> targetFuncSet;
			set<Function *> setFunctions;
			set<Function *> cpyFunctions;
			
			Module *module;
			DataLayout *DL;
			PADriver *pa;
			VerilogConfigInfo *verilogConfigInfo;
			CallTable *callTable;
	};
}

#endif
