#include "llvm/Support/FileSystem.h"

#include "MemoryTable.h"

#include <iostream>
#include <fstream>
#include <iomanip>

#define LAST(k,n) ((k) & ((1<<(n))-1))
#define MID(k,m,n) LAST((k)>>(m),((n)-(m)))

using namespace llvm;
using namespace std;

namespace corelab
{
	struct ElementInfo{
		unsigned dataWidth;
		unsigned numOfElements;
	};

	static unsigned ramId;
	static void initRAMId(void) { ramId = 1; }
	static unsigned getRAMId(void)
	{
		ramId++;
		return ramId - 1;
	}

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

	static ElementInfo getElementInfo(Type *eTy, DataLayout *DL)
	{
		if ( ArrayType *aTy = dyn_cast<ArrayType>(eTy) )
		{
			ElementInfo nowInfo;
			nowInfo.numOfElements = aTy->getNumElements();

			Type *nestType = aTy->getElementType();
			ElementInfo nestInfo = getElementInfo( nestType, DL );

			nowInfo.numOfElements *= nestInfo.numOfElements;
			nowInfo.dataWidth = nestInfo.dataWidth;

			return nowInfo;
		}
		else if (  IntegerType *iTy = dyn_cast<IntegerType>(eTy) )
		{
			ElementInfo nowInfo;
			nowInfo.numOfElements = 1;
			nowInfo.dataWidth = iTy->getBitWidth();
			return nowInfo;
		}
		else if ( eTy->isFloatTy() )
		{
			ElementInfo nowInfo;
			nowInfo.numOfElements = 1;
			nowInfo.dataWidth = 32;
			return nowInfo;
		}
		else if ( eTy->isDoubleTy() )
		{
			ElementInfo nowInfo;
			nowInfo.numOfElements = 1;
			nowInfo.dataWidth = 64;
			return nowInfo;		
		}
		else if ( isa<PointerType>(eTy) )
		{
			ElementInfo nowInfo;
			nowInfo.numOfElements = 1;
			//TODO : this should be deteremined by -like- :195-201 codes
			nowInfo.dataWidth = 32;
//This is for x86 system
//			nowInfo.dataWidth = DL->getPointerSizeInBits();
			return nowInfo;
		}
		else if ( StructType *sTy = dyn_cast<StructType>(eTy) )
		{
			ElementInfo nowInfo;
			nowInfo.dataWidth = 0;
			nowInfo.numOfElements = 0;
			for ( int i = 0; i < sTy->getNumElements(); i++ )
			{
				ElementInfo nestInfo = getElementInfo( sTy->getElementType(i), DL );
				if ( nowInfo.dataWidth < nestInfo.dataWidth )
					nowInfo.dataWidth = nestInfo.dataWidth;
				nowInfo.numOfElements += nestInfo.numOfElements;
			}
			return nowInfo;
		}
		else
		{
			eTy->dump();
			assert(0 &&"can not support this type of memory");
		}
	}

	void RAM_::ramInitializer(DataLayout *DL)
	{
		ElementInfo elementInfo = getElementInfo( eTy, DL );
		dataBitSize = elementInfo.dataWidth;
		numOfElements = elementInfo.numOfElements;
		elementBitSize = getRequiredBits(numOfElements);

		//TODO : Struct in an Array -> not detaily counted like below
		// just maxbitwidth * struct element num will be total bits

		totalBit = 0;
//		if ( StructType *sTy = dyn_cast<StructType>(eTy) ) {
		if ( isa<StructType>(eTy) && compactStruct ) {
		if ( StructType *sTy = dyn_cast<StructType>(eTy) ) {
			for ( int i = 0; i < sTy->getNumElements(); ++i ) {
				ElementInfo eInfo = getElementInfo( sTy->getElementType(i), DL );
				//DEBUG
//				errs() << "Debug for totalBit\n";
//				errs() << eInfo.dataWidth << " : " << eInfo.numOfElements << "\n";
				totalBit += eInfo.dataWidth * eInfo.numOfElements;
				//DEBUG
//				errs() << totalBit << "\n\n";
			}
		}
		}
		else
			totalBit = numOfElements * dataBitSize;

		if ( 1 < numOfElements )
			structure = true;
	}

	RAM_::RAM_(Value *v_, DataLayout *DL, bool compact) : v(v_)
	{
		ramId = 0;
		initId = 0;
		initData = 0;
		numOfElements = 0;
		structure = false;
		readOnly = true;
		compactStruct = compact;
		ty = v->getType();
		PointerType *pTy = dyn_cast<PointerType>(ty);
		assert(pTy && "Type of Memory Value should be Pointer Type");
		// the element which the pointer points
		eTy = pTy->getElementType();
		assert(eTy);
	
		ramInitializer(DL);		
	}

	void MemoryTableBuilder::searchGV(MemoryTable *memoryTable)
	{
		for (auto gvIter = module->global_begin(); gvIter != module->global_end(); gvIter++)
		{
			if ( (&*gvIter)->isDeclaration() ) continue;
			if ( (&*gvIter)->getName() == "llvm.used" ) continue;
//TODO:	There can be Various Global Values
//			*memFile << "Global Variable : \n" << *(&*gvIter) << "\n";

			bool compact = false;
			if ( memoryTable->hasStruct( &*gvIter ) )
				if ( memoryTable->isCompactStruct( &*gvIter ) )
					compact = true;

			RAM_ *ram = new RAM_(&*gvIter, DL, compact);
			memoryTable->setValue2RAM(&*gvIter, ram);
			ramList.push_back(ram);
		}
	}

	void MemoryTableBuilder::searchLocalAllocaVariable(MemoryTable *memoryTable)
	{
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
					if ( AllocaInst *alloca = dyn_cast<AllocaInst>(&*ii) )
					{
						bool compact = false;
						if ( memoryTable->hasStruct( alloca ) )
							if ( memoryTable->isCompactStruct( alloca ) )
								compact = true;

						RAM_ *ram = new RAM_(alloca, DL, compact);
						memoryTable->setValue2RAM(alloca, ram);
						ramList.push_back(ram);
					}
	}

	void MemoryTableBuilder::genCentralizedMemoryTable(MemoryTable *memoryTable)
	{
		memoryTable->setRAMList(ramList);

		//Address ( Pointer Register ) Size
		unsigned maxPSize = 0;
		for ( auto ram : memoryTable->getAllRAMList() )
			if ( ram->getTotalBitSize()/8 != 0 )
				if ( maxPSize <	getRequiredBits(ram->getTotalBitSize() /8) )
					maxPSize = getRequiredBits(ram->getTotalBitSize() /8);
		unsigned numOfRAM = (memoryTable->getAllRAMList()).size();
		unsigned bitSizeOfRAM = (numOfRAM == 0) ? 0 : getRequiredBits(numOfRAM+1);
		maxPSize += bitSizeOfRAM;

		if ( maxPSize <= 8 )
			memoryTable->setPointerBitSize( 8 );
		else if ( maxPSize <= 16 && 8 < maxPSize )
			memoryTable->setPointerBitSize( 16 );
		else if ( maxPSize <= 32 && 16 < maxPSize )
			memoryTable->setPointerBitSize( 32 );
		else if ( maxPSize <= 64 && 32 < maxPSize )
			memoryTable->setPointerBitSize( 64 );
		else {
			errs() << maxPSize << "\n";
			assert(0);
		}

		// First, Search Address RAM List
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;
					if ( isa<LoadInst>(inst) || isa<StoreInst>(inst) )
					{
						Value *pointerV = getMemOper(inst);
						assert(pointerV);

						Type *bitWidthType = NULL;
						if ( isa<LoadInst>(inst) )
							bitWidthType = inst->getType();
						else
							bitWidthType = inst->getOperand(0)->getType();
						assert(bitWidthType);

						unsigned bitWidth = 0;
						if ( IntegerType *intTy = dyn_cast<IntegerType>(bitWidthType) )
							bitWidth = intTy->getBitWidth();
						else if ( isa<PointerType>(bitWidthType) )
							bitWidth = memoryTable->getPointerBitSize();
						else if ( bitWidthType->isFloatTy() )
							bitWidth = 32;
						else if ( bitWidthType->isDoubleTy() )
							bitWidth = 64;
						else {
							bitWidth = 64;
							errs() << "unknown 64bitWidth : " << *inst << "\n";
						}

						set<Value *> memories = pa->getPointedMemory(pointerV);

						if ( memories.size() == 0 ) { // unresolved
//							memoryTable->addUnresolvedInst(&*fi, inst);
//							for ( auto ramIter : ramList )
//								memoryTable->addAddressAccessRAMList(ramIter);

							//XXX : Unify Unresolved to Address Access 
							memoryTable->addAddressAccessInst(&*fi, inst);
							for ( auto ramIter : ramList )
							{
								memoryTable->addAccessRAM(&*fi, ramIter);
								memoryTable->addAddressAccessRAM(&*fi, ramIter);
								memoryTable->addAddressAccessRAMList(ramIter);
								memoryTable->addRAMAccessedByFunction(ramIter, &*fi);

								if ( verilogConfigInfo->getNoMC() && hasStruct(ramIter->getETy()) )
										ramIter->addPossibleBitWidth(ramIter->getDataBitSize());
								else
									ramIter->addPossibleBitWidth(bitWidth);

								if ( isa<StoreInst>(inst) )
									ramIter->setReadOnly(false);
							}
						}
						else if ( memories.size() == 1 ) {

						}
						else {	// multi-points
							memoryTable->addAddressAccessInst(&*fi, inst);
							for ( auto memValue : memories )
							{
								RAM_ *ram = memoryTable->getRAMFromValue(memValue);
								if ( ram == NULL ) {
									memValue->dump();
									assert(ram);
								}

								memoryTable->addAccessRAM(&*fi, ram);
								memoryTable->addAddressAccessRAM(&*fi, ram);
								memoryTable->addAddressAccessRAMList(ram);
								memoryTable->addRAMAccessedByFunction(ram, &*fi);

								if ( verilogConfigInfo->getNoMC() && hasStruct(ram->getETy()) )
									ram->addPossibleBitWidth(ram->getDataBitSize());
								else
									ram->addPossibleBitWidth(bitWidth);

								if ( isa<StoreInst>(inst) )
									ram->setReadOnly(false);
							}
						}
					}
				}

		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;
					if ( isa<LoadInst>(inst) || isa<StoreInst>(inst) )
					{
						Value *pointerV = getMemOper(inst);
						assert(pointerV);

						Type *bitWidthType = NULL;
						if ( isa<LoadInst>(inst) )
							bitWidthType = inst->getType();
						else
							bitWidthType = inst->getOperand(0)->getType();
						assert(bitWidthType);

						unsigned bitWidth = 0;
						if ( IntegerType *intTy = dyn_cast<IntegerType>(bitWidthType) )
							bitWidth = intTy->getBitWidth();
						else if ( isa<PointerType>(bitWidthType) )
							bitWidth = memoryTable->getPointerBitSize();
						else if ( bitWidthType->isFloatTy() )
							bitWidth = 32;
						else if ( bitWidthType->isDoubleTy() )
							bitWidth = 64;
						else {
							bitWidth = 64;
							errs() << "unknown 64bitWidth : " << *inst << "\n";
						}

						set<Value *> memories = pa->getPointedMemory(pointerV);
						if ( memories.size() == 1 ) {
							RAM_ *ram = memoryTable->getRAMFromValue(*memories.begin());
							if ( ram == NULL ) {
								(*memories.begin())->dump();
								assert(ram);
							}
							
							bool addressAccessRAM = false;
							for ( auto ramIter : memoryTable->getAddressAccessRAMList() )
								if ( ramIter == ram ) {
									addressAccessRAM = true;
									break;
								}

							if ( addressAccessRAM ) {
								memoryTable->addAddressAccessInst(&*fi, inst);

								memoryTable->addAccessRAM(&*fi, ram);
								memoryTable->addAddressAccessRAM(&*fi, ram);
								memoryTable->addAddressAccessRAMList(ram);
								memoryTable->addRAMAccessedByFunction(ram, &*fi);

								if ( verilogConfigInfo->getNoMC() && hasStruct(ram->getETy()) )
									ram->addPossibleBitWidth(ram->getDataBitSize());
								else
									ram->addPossibleBitWidth(bitWidth);

								if ( isa<StoreInst>(inst) )
									ram->setReadOnly(false);
							}
							else {
								memoryTable->addPrivateAccessInst(&*fi, ram, inst);
								memoryTable->addAccessRAM(&*fi, ram);
								memoryTable->addRAMAccessedByFunction(ram, &*fi);

								if ( verilogConfigInfo->getNoMC() && hasStruct(ram->getETy()) )
									ram->addPossibleBitWidth(ram->getDataBitSize());
								else
									ram->addPossibleBitWidth(bitWidth);

								if ( isa<StoreInst>(inst) )
									ram->setReadOnly(false);
							}
						}

					}
				}
	}

	void MemoryTableBuilder::genMemoryTable(MemoryTable *memoryTable)
	{
		memoryTable->setRAMList(ramList);

		//Address ( Pointer Register ) Size
		unsigned maxPSize = 0;
		for ( auto ram : memoryTable->getAllRAMList() )
			if ( ram->getTotalBitSize()/8 != 0 )
				if ( maxPSize <	getRequiredBits(ram->getTotalBitSize() /8) )
					maxPSize = getRequiredBits(ram->getTotalBitSize() /8);
		unsigned numOfRAM = (memoryTable->getAllRAMList()).size();
		unsigned bitSizeOfRAM = (numOfRAM == 0) ? 0 : getRequiredBits(numOfRAM+1);
		maxPSize += bitSizeOfRAM;

		if ( maxPSize <= 8 )
			memoryTable->setPointerBitSize( 8 );
		else if ( maxPSize <= 16 && 8 < maxPSize )
			memoryTable->setPointerBitSize( 16 );
		else if ( maxPSize <= 32 && 16 < maxPSize )
			memoryTable->setPointerBitSize( 32 );
		else if ( maxPSize <= 64 && 32 < maxPSize )
			memoryTable->setPointerBitSize( 64 );
		else {
			errs() << maxPSize << "\n";
			assert(0);
		}

		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;
					if ( isa<LoadInst>(inst) || isa<StoreInst>(inst) )
					{
						Value *pointerV = getMemOper(inst);
						assert(pointerV);

						Type *bitWidthType = NULL;
						if ( isa<LoadInst>(inst) )
							bitWidthType = inst->getType();
						else
							bitWidthType = inst->getOperand(0)->getType();
						assert(bitWidthType);

						unsigned bitWidth = 0;
						if ( IntegerType *intTy = dyn_cast<IntegerType>(bitWidthType) )
							bitWidth = intTy->getBitWidth();
						else if ( isa<PointerType>(bitWidthType) )
							bitWidth = memoryTable->getPointerBitSize();
						else if ( bitWidthType->isFloatTy() )
							bitWidth = 32;
						else if ( bitWidthType->isDoubleTy() )
							bitWidth = 64;
						else {
							bitWidth = 64;
							errs() << "unknown 64bitWidth : " << *inst << "\n";
						}

						set<Value *> memories = pa->getPointedMemory(pointerV);
						if ( memories.size() == 0 ) { // unresolved
//							memoryTable->addUnresolvedInst(&*fi, inst);
//							for ( auto ramIter : ramList )
//								memoryTable->addAddressAccessRAMList(ramIter);

							//XXX : Unify Unresolved to Address Access 
							memoryTable->addAddressAccessInst(&*fi, inst);
							for ( auto ramIter : ramList )
							{
								memoryTable->addAccessRAM(&*fi, ramIter);
								memoryTable->addAddressAccessRAM(&*fi, ramIter);
								memoryTable->addAddressAccessRAMList(ramIter);
								memoryTable->addRAMAccessedByFunction(ramIter, &*fi);

								if ( verilogConfigInfo->getNoMC() && hasStruct(ramIter->getETy()) )
										ramIter->addPossibleBitWidth(ramIter->getDataBitSize());
								else
									ramIter->addPossibleBitWidth(bitWidth);

								if ( isa<StoreInst>(inst) )
									ramIter->setReadOnly(false);
							}
						}
						else if ( memories.size() == 1 ) { // private access pointer value
							RAM_ *ram = memoryTable->getRAMFromValue(*memories.begin());
							if ( ram == NULL ) {
								(*memories.begin())->dump();
								assert(ram);
							}

							memoryTable->addPrivateAccessInst(&*fi, ram, inst);
							memoryTable->addAccessRAM(&*fi, ram);
							memoryTable->addRAMAccessedByFunction(ram, &*fi);

							if ( verilogConfigInfo->getNoMC() && hasStruct(ram->getETy()) )
								ram->addPossibleBitWidth(ram->getDataBitSize());
							else
								ram->addPossibleBitWidth(bitWidth);

							if ( isa<StoreInst>(inst) )
								ram->setReadOnly(false);
						}
						else {	// multi-points
							memoryTable->addAddressAccessInst(&*fi, inst);
							for ( auto memValue : memories )
							{
								RAM_ *ram = memoryTable->getRAMFromValue(memValue);
								if ( ram == NULL ) {
									memValue->dump();
									assert(ram);
								}

								memoryTable->addAccessRAM(&*fi, ram);
								memoryTable->addAddressAccessRAM(&*fi, ram);
								memoryTable->addAddressAccessRAMList(ram);
								memoryTable->addRAMAccessedByFunction(ram, &*fi);

								if ( verilogConfigInfo->getNoMC() && hasStruct(ram->getETy()) )
									ram->addPossibleBitWidth(ram->getDataBitSize());
								else
									ram->addPossibleBitWidth(bitWidth);

								if ( isa<StoreInst>(inst) )
									ram->setReadOnly(false);
							}
						}
					}//load & store if end
				}

	}

	void MemoryTableBuilder::setCentralizedGEPAddrSize(MemoryTable *memoryTable) {
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;
					if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst) ) {
						unsigned global_addr_size 
							= memoryTable->getRAMBitSize() + memoryTable->getElementBitSize();
							
						memoryTable->setGEPAddrSize(gep, global_addr_size);
					} //gep if end

				}//instruction iter for end
	}


	void MemoryTableBuilder::setGEPAddrSize(MemoryTable *memoryTable) {
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;
					if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst) ) {
						Value *pointerV = gep->getPointerOperand();

						set<Value *> memories = pa->getPointedMemory(pointerV);
						if ( memories.size() == 1 ) { // private access pointer value
							RAM_ *ram = memoryTable->getRAMFromValue(*memories.begin());
							if ( ram == NULL ) {
								(*memories.begin())->dump();
								assert(ram);
							}

							unsigned addr_size = ram->getElementBitSize();
							unsigned global_addr_size 
								= memoryTable->getRAMBitSize() + memoryTable->getElementBitSize();

							bool passToMultiplePoints = false;
							for ( auto ui = inst->user_begin(); ui != inst->user_end(); ++ui )
							{
								Instruction *user = dyn_cast<Instruction>(*ui);
								assert(user);

								if ( CallInst *cInst = dyn_cast<CallInst>(user) ) {
									/*
									Function *callee = cInst->getCalledFunction();
									assert(callee);

									unsigned opN = 0, argN = 0;
									for ( ; opN < user->getNumOperands(); opN++ )
										if ( user->getOperand(opN) == inst )
											break;

									for ( auto arg = callee->arg_begin(); arg !=callee->arg_end(); arg++ )
									{
										if ( argN == opN ) {
											Value *argV = (Value *)(&*arg);
											set<Value *> c_memories = pa->getPointedMemory(argV);
											if ( c_memories.size() != 1 )
												passToMultiplePoints = true;
											break;
										}
										argN++;
									}*/
									passToMultiplePoints = true;
								}
								else if ( isa<BitCastInst>(user) ) {
									for ( auto ui2 = user->user_begin(); ui2 != user->user_end(); ++ui2 )
									{
										Instruction *user2 = dyn_cast<Instruction>(*ui2);
										assert(user2);
										if ( isa<CallInst>(user2) )
											passToMultiplePoints = true;
									}
								}
								//store pointer value
								else if ( StoreInst *sInst = dyn_cast<StoreInst>(user) ) {
									if ( inst == sInst->getValueOperand() )
										passToMultiplePoints = true;
								}
							}

							if ( passToMultiplePoints )
								memoryTable->setGEPAddrSize(gep, global_addr_size);
							else
								memoryTable->setGEPAddrSize(gep, addr_size);
						}
						else {	// multi-points
							unsigned global_addr_size 
								= memoryTable->getRAMBitSize() + memoryTable->getElementBitSize();
							
							memoryTable->setGEPAddrSize(gep, global_addr_size);
						}
					} //gep if end

				}//instruction iter for end
	}

	void MemoryTableBuilder::setAddressSpace(MemoryTable *memoryTable)
	{
		unsigned maxElements = 0;
//		unsigned numOfRAM = (memoryTable->getAddressAccessRAMList()).size();
		unsigned numOfRAM = (memoryTable->getAllRAMList()).size();
		unsigned bitSizeOfRAM = (numOfRAM == 0) ? 0 : getRequiredBits(numOfRAM+1);//XXX: null is 0

		bool byteAddressedRAM = false;
		for ( auto ram : memoryTable->getAllRAMList() )
		{
			unsigned memBitWidth = 0;
			unsigned possibleMax = 0;
			for ( auto possibleBitWidth : ram->getPossibleBitWidth() )
				if ( possibleMax < possibleBitWidth )
					possibleMax = possibleBitWidth;

			if ( verilogConfigInfo->getMemoryBitWidth() != 0 ) {
				memBitWidth = verilogConfigInfo->getMemoryBitWidth();
				byteAddressedRAM = true;
			}
			else
				memBitWidth = possibleMax;
			if ( memBitWidth == 0 ) {
				ram->getValue()->dump();
				assert(memBitWidth != 0);
			}
			ram->setMemBitWidth(memBitWidth);
			ram->setDataBitSize(possibleMax);

			//TODO: FIM Accel -> do not need Memory Controller
			// Real Automatic Accel -> need memory controller
			// Now Accelerator option can not distinguish those
			// this version is for FIM version.
			byteAddressedRAM = byteAddressedRAM |	((ram->getPossibleBitWidth()).size() != 1) |
				(memBitWidth > possibleMax);// | verilogConfigInfo->getAccel();
		}

		if ( verilogConfigInfo->getNoMC() )
			assert(byteAddressedRAM == false);

		memoryTable->setByteAddressedRAM( byteAddressedRAM );

		//setting all ram having the same memory bitwidth
		unsigned maxMemBitWidth = 0;

		for ( auto ram : memoryTable->getAllRAMList() )
			if ( maxMemBitWidth < ram->getMemBitWidth() )
				maxMemBitWidth = ram->getMemBitWidth();

		if ( byteAddressedRAM ) {
			for ( auto ram : memoryTable->getAllRAMList() )
				ram->setMemBitWidth(maxMemBitWidth); //re define membitwidth
		}

		memoryTable->setMemBitWidth(maxMemBitWidth);


		//setting numOfBlock and ElementBitSize ( related address size )
		for ( auto ram : memoryTable->getAllRAMList() )
		{
			ram->getValue()->dump();

			unsigned ramTotalBitSize = ram->getTotalBitSize();
			unsigned memBitWidth = ram->getMemBitWidth();
			unsigned possibleMax = ram->getDataBitSize();
			errs() << ramTotalBitSize << "\n";
			errs() << memBitWidth << "\n";
			errs() << possibleMax << "\n";
				
			if ( byteAddressedRAM ) {
				if ( ramTotalBitSize < memBitWidth )
					ram->setElementBitSize( getRequiredBits( memBitWidth / 8 ) );
				else
					ram->setElementBitSize( getRequiredBits( ramTotalBitSize /8 ) ); //re-define address

				if ( ramTotalBitSize < memBitWidth )
					ram->setNumOfBlock ( 1 );
				else if ( ramTotalBitSize % memBitWidth == 0 )
					ram->setNumOfBlock( (ramTotalBitSize / memBitWidth) );
				else
					ram->setNumOfBlock( (ramTotalBitSize / memBitWidth) + 1 );

				assert( memBitWidth >= possibleMax );
				assert( memBitWidth % possibleMax == 0 );
				assert( ramTotalBitSize <= (ram->getNumOfBlock() * memBitWidth) );
			}
			else
				ram->setNumOfBlock(0);

			errs() << "RAM" << ram->getRAMId() << "\n";
			for ( unsigned n : ram->getPossibleBitWidth() )
				errs() << n << "\t";
			errs() << "\n" << memBitWidth << " : " << possibleMax << " : ";
			errs() << ram->getElementBitSize() << " : " << ram->getDataBitSize() << "\n\n";
		}


		unsigned maxBitWidth = 0;
		unsigned bitSizeOfElement = 0;

		for ( auto ram : memoryTable->getAllRAMList() )
		{
			if ( bitSizeOfElement < ram->getElementBitSize() )
				bitSizeOfElement = ram->getElementBitSize();
			if ( maxBitWidth < ram->getDataBitSize() )
				maxBitWidth = ram->getDataBitSize();
		}

		memoryTable->setRAMSize(numOfRAM);
		memoryTable->setRAMBitSize(bitSizeOfRAM);
		memoryTable->setElementBitSize(bitSizeOfElement);
		memoryTable->setBitWidth(maxBitWidth);


		//exPortUse
		if ( memoryTable->isByteAddressedRAM() ) {
			for ( auto ram : memoryTable->getAllRAMList() )
			{
				if ( isUsedInMemFunc(ram) )
					ram->setExPortUse(true);
				else
					ram->setExPortUse(false);
			}
		}
		else {
			for ( auto ram : memoryTable->getAllRAMList() )
				ram->setExPortUse(false);
		}
	}

	bool MemoryTableBuilder::hasStruct(Type *ty) {
		if ( isa<StructType>(ty) )
			return true;
		else if ( SequentialType *sTy = dyn_cast<SequentialType>(ty) )
			return hasStruct( sTy->getElementType() );
		else
			return false;
	}

	void MemoryTableBuilder::searchCompactStruct(MemoryTable *memoryTable)
	{
		//search memory having struct -> struct / structNested ( array of struct )
		// struct - struct -> ok
		// struct - structNested -> make the struct no
		// TODO : Align Checker

		for (auto gvIter = module->global_begin(); gvIter != module->global_end(); gvIter++)
		{
			if ( (&*gvIter)->isDeclaration() ) continue;
			if ( (&*gvIter)->getName() == "llvm.used" ) continue;

			Value *mem = &*gvIter;

			Type *ty = mem->getType();
			PointerType *pTy = dyn_cast<PointerType>(ty);
			assert(pTy && "Type of Memory Value should be Pointer Type");
			Type *eTy = pTy->getElementType();

			if ( isa<StructType>(eTy) ) 
				memoryTable->setCompactStruct(mem, true);
			else if ( hasStruct(eTy) )
				memoryTable->setCompactStruct(mem, false);
		}

		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
					if ( AllocaInst *alloca = dyn_cast<AllocaInst>(&*ii) )
					{
						Value *mem = alloca;

						Type *ty = mem->getType();
						PointerType *pTy = dyn_cast<PointerType>(ty);
						assert(pTy && "Type of Memory Value should be Pointer Type");
						Type *eTy = pTy->getElementType();

						if ( isa<StructType>(eTy) ) 
							memoryTable->setCompactStruct(mem, true);
						else if ( hasStruct(eTy) )
							memoryTable->setCompactStruct(mem, false);
					}

		errs() << "Struct Compact Test\n";

		bool noCompact = false;
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;
					if ( isa<StoreInst>(inst) || isa<LoadInst>(inst) ) {
						Value *ptr = getMemOper(inst);

						//
						inst->dump();

						std::set<Value *> mset = pa->getPointedMemory(ptr);
						if ( mset.size() == 0 )
							noCompact = true;
						else {
							bool mixed = false;
							for ( auto candidate : mset )
								if ( memoryTable->hasStruct(candidate) ) {
									//
									candidate->dump();

									if ( !memoryTable->isCompactStruct(candidate) ) {
										mixed = true;
										//
										errs() << "(StructNested)\n";
									}
								}
							if ( mixed ) // with structNested
								for ( auto candidate : mset )
									if ( memoryTable->hasStruct(candidate) )
										memoryTable->setCompactStruct(candidate, false);
						}
						//
						errs() << "\n";
					}
				}

		if ( noCompact )
			memoryTable->setNoCompactStruct();
	}



	MemoryTableBuilder::MemoryTableBuilder(Module *module_, DataLayout *DL_, 
			PADriver *pa_, VerilogConfigInfo *verilogConfigInfo_, CallTable *callTable_)
		: module(module_), DL(DL_), pa(pa_), verilogConfigInfo(verilogConfigInfo_),
		callTable(callTable_)
	{
		targetFuncSet.clear();
		if ( verilogConfigInfo->getAccel() ) {
			Function *targetFunc = verilogConfigInfo->getAccelFunction();
			targetFunctionSetting(targetFunc);
		}
		else {
			targetFunctionSetting(module->getFunction("main"));
		}

		setFunctions.clear();
		cpyFunctions.clear();
		//optimized memory function setting 
		//these functions exploit ex ports
		if ( !verilogConfigInfo->getNoMF() ) {
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


		memoryTable = new MemoryTable();

		//Search compact struct candidates
		if ( !verilogConfigInfo->getNoMC() )
			searchCompactStruct(memoryTable);

		//Allocate RAMs
		searchGV(memoryTable);
		searchLocalAllocaVariable(memoryTable);

		//RAM ID Initialization
		initRAMId();
		for ( auto ramIter : ramList ) {
			ramIter->setRAMId(getRAMId());
			ramIter->setInitId(0);
		}

		if ( verilogConfigInfo->getCentralizedMemory() )
			genCentralizedMemoryTable(memoryTable);
		else
			genMemoryTable(memoryTable);

		//Address Space Define
		setAddressSpace(memoryTable);

		errs() << "AddressSpace Setting Done\n";

		if ( verilogConfigInfo->getCentralizedMemory() )
			setCentralizedGEPAddrSize(memoryTable);
		else
			setGEPAddrSize(memoryTable);


		unsigned global_addr_size 
			= memoryTable->getRAMBitSize() + memoryTable->getElementBitSize();
		memoryTable->setPointerBitSize(global_addr_size);

		errs() << "AddressSize Setting Done\n";

		//Assign RAM Initializer File from OPT pass
		unsigned maxId = 0;
		for ( auto ramIter : ramList )
		{
			// The case that this ram initialized into file in opt pass
			if ( AllocaInst *aInst = dyn_cast<AllocaInst>(ramIter->getValue()) )
				if ( MDNode *md = aInst->getMetadata("initGV") )
					if (Metadata *m = md->getOperand(0).get() )
						if ( Value *idV = dyn_cast<ValueAsMetadata>(m)->getValue() )
							if ( ConstantInt *cInt = dyn_cast<ConstantInt>(idV) ) {
								unsigned initId = cInt->getZExtValue();
								ramIter->setInitId(initId);
								if ( maxId < initId )
									maxId = initId;
							}
		}
		maxId++;

		for ( auto ramIter : ramList )
		{
			// This can give the positive maximum data bitwidth
			// But, when initializing somthing, original data bitwidth is needed.
			//unsigned dataBitWidth = ramIter->getDataBitSize();

			bool memBlockUse = ramIter->hasMultipleSize() || ramIter->hasExPort();

			ElementInfo elementInfo = getElementInfo( ramIter->getETy(), DL );
			unsigned dataBitWidth = elementInfo.dataWidth;
			unsigned memBitWidth = ramIter->getMemBitWidth();
			unsigned numBlock = ramIter->getNumOfBlock();
			unsigned totalBitSize = ramIter->getTotalBitSize();

			if ( GlobalVariable *gv = dyn_cast<GlobalVariable>(ramIter->getValue()) )
				if ( const Constant *init = gv->getInitializer() ) {
					if ( const ConstantDataSequential *array = dyn_cast<ConstantDataSequential>(init) ) {
						SmallString<256> nameBuf;
						nameBuf.clear();
						StringRef fileNameBuf = 
							Twine(Twine(maxId+(ramIter->getRAMId())) + Twine(".mem")).toStringRef(nameBuf);
						//TODO:file open
						ofstream output(fileNameBuf.str(), ios::out | ios::trunc);

						unsigned m = array->getNumElements();

//						if ( memoryTable->isByteAddressedRAM() ) {
						if ( memBlockUse ) {
							for ( unsigned k = 0; k < m; ++k )
							{
								for ( unsigned j = 0; j < dataBitWidth / 8; ++j )
								{
									auto pulled = MID(array->getElementAsInteger(k), j*8, (j+1)*8);
									output << setfill('0') << setw(2) << hex << pulled;
								}
								if ( ((k+1) * dataBitWidth) %  memBitWidth == 0 )
									output << endl;
							}

							if ( totalBitSize < numBlock * memBitWidth ) {
								unsigned additionalBits = (numBlock * memBitWidth) - totalBitSize;
								for ( unsigned i = 0; i < additionalBits / 8; ++i )
									output << setfill('0') << setw(2) << hex << 0;
							}
						}
						else {
							for ( unsigned i = 0; i < m; ++i )
							{
								uint64_t data = array->getElementAsInteger(i);
								output << hex << data << endl;
							}
						}


						output.close();

						ramIter->setInitId(maxId+(ramIter->getRAMId()));
					}
					else if ( const ConstantArray *constArray = dyn_cast<ConstantArray>(init) ) {
						SmallString<256> nameBuf;
						nameBuf.clear();
						StringRef fileNameBuf = 
							Twine(Twine(maxId+(ramIter->getRAMId())) + Twine(".mem")).toStringRef(nameBuf);
						//TODO:file open
						ofstream output(fileNameBuf.str(), ios::out | ios::trunc);

						unsigned n = init->getNumOperands();

//						if ( memoryTable->isByteAddressedRAM() ) {
						if ( memBlockUse ) {
							for ( unsigned i = 0; i < n; ++i )
							{
								const Value *opV = init->getOperand(i);
								if ( const ConstantDataSequential *nestArray 
										= dyn_cast<ConstantDataSequential>(opV) ) {
									unsigned m = nestArray->getNumElements(); 
									//XXX: Assume nest arrays have the same type

									for ( unsigned k = 0; k < m; ++k )
									{
										for ( unsigned j = 0; j < dataBitWidth /8; ++j )
										{
											auto pulled = MID( nestArray->getElementAsInteger(k), j*8, (j+1)*8 );
											output << setfill('0') << setw(2) << hex << pulled;
										}
										if ( (((k+1) + (i*m)) * dataBitWidth) % memBitWidth == 0 )
											output << endl;
									}
								}
							}

							if ( totalBitSize < numBlock * memBitWidth ) {
								unsigned additionalBits = (numBlock * memBitWidth) - totalBitSize;
								for ( unsigned i = 0; i < additionalBits / 8; ++i )
									output << setfill('0') << setw(2) << hex << 0;
							}
						}
						else {
							for ( unsigned i = 0; i < n; ++i )
							{
								const Value *opV = init->getOperand(i);
								if ( const ConstantDataSequential *nestArray 
										= dyn_cast<ConstantDataSequential>(opV) ) {
									unsigned m = nestArray->getNumElements();
									Type *ty = nestArray->getElementType();
									for ( unsigned j = 0; j < m ; ++j )
									{
										uint64_t data = nestArray->getElementAsInteger(j);
										output << hex << data << endl;
									}
								}
							}
						}

						output.close();

						ramIter->setInitId(maxId+(ramIter->getRAMId()));
					}
					else if ( const ConstantInt *cInt = dyn_cast<ConstantInt>(init) ) {
						SmallString<256> nameBuf;
						nameBuf.clear();
						StringRef fileNameBuf = 
							Twine(Twine(maxId+(ramIter->getRAMId())) + Twine(".mem")).toStringRef(nameBuf);
						ofstream output(fileNameBuf.str(), ios::out | ios::trunc);
						uint64_t data = cInt->getZExtValue();
						ramIter->setInitData(data);// This is for RegisterMemory

//						if ( memoryTable->isByteAddressedRAM() ) {
						if ( memBlockUse ) {
							for ( unsigned i = 0; i < dataBitWidth /8; ++i )
							{
								auto pulled = MID( data, i*8, (i+1)*8 );
								output << setfill('0') << setw(2) << hex << pulled;
							}

							if ( totalBitSize < numBlock * memBitWidth ) {
								unsigned additionalBits = (numBlock * memBitWidth) - totalBitSize;
								for ( unsigned i = 0; i < additionalBits / 8; ++i )
									output << setfill('0') << setw(2) << hex << 0;
							}
						}
						else 
							output << hex << data << endl;


						output.close();

						ramIter->setInitId(maxId+(ramIter->getRAMId()));
					}
				}
		}
		
		
		//Print Memory Debug File
		std::error_code ec_print = std::make_error_code(std::errc::io_error);
		raw_fd_ostream memFile("memoryTable.debug", ec_print, llvm::sys::fs::F_None);

		memFile << "@@@@@@@@@@@@@@ Address SPACE INFORMATION @@@@@@@@@@@\n\n";
		memFile << "is Byte Addressed Memory Space : " << memoryTable->isByteAddressedRAM() << "\n";
		memFile << "Byte Address - Pointer Bit Size : " << memoryTable->getPointerBitSize() << "\n";
		memFile << "RAM Bit Size : " << memoryTable->getRAMBitSize() << "\n";
		memFile << "Element Bit Size : " << memoryTable->getElementBitSize() << "\n";
		memFile << "Poss Max Bit Width : " << memoryTable->getBitWidth() << "\n";
		memFile << "Mem Max Bit Width : " << memoryTable->getMemBitWidth() << "\n\n";

		memFile << "@@@@@@@@@@@@@@ ALL MEMORY LIST @@@@@@@@@@@\n\n";
		for ( auto ramIter : memoryTable->getAllRAMList() )
		{
			memFile << "NAME : RAM" << ramIter->getRAMId() << "\n";
			memFile << "InitID : " << ramIter->getInitId() << "\n";
			memFile << "Value : " << (*(ramIter->getValue())) << "\n";
			memFile << "Num of Elements : " << ramIter->getNumOfElements() << "\n";
			memFile << "Total Bit Size: " << ramIter->getTotalBitSize() << "\n";
//			memFile << "is Byte Addressed RAM : " << ramIter->isByteAddressedRAM() << "\n";
			memFile << "Bit Size for Function(data bitwidth) : " << ramIter->getDataBitSize() << "\n";
			memFile << "Address Size (elem bitsize): " << ramIter->getElementBitSize() << "\n\n";
			memFile << "RAM Block Size : " << ramIter->getMemBitWidth() << "\n";
			memFile << "Num of Memory Blocks : " << ramIter->getNumOfBlock() << "\n";
			memFile << "is Read Only : " << ramIter->isReadOnly() << "\n";
			memFile << "has Multiple Size : " << ramIter->hasMultipleSize() << "\n";
			memFile << "has Ex Port : " << ramIter->hasExPort() << "\n\n\n";
		}	


		// NEED ADDRESS SPACE
		memFile << "\n@@@@@@@@@@@@@@ Address Access RAM LIST  @@@@@@@@@@@\n\n";
		for ( auto ramIter : memoryTable->getAddressAccessRAMList() )
			memFile << "NAME : RAM" << ramIter->getRAMId() << "\n";

		//
		memFile << "\n\n@@@@@@@@@@@@@@ Accessed RAMs by Functions @@@@@@@@@@@\n\n";
		for ( auto fi = module->begin(); fi != module->end(); fi++ ) 
		{
			if ( (&*fi)->isDeclaration() )
				continue;
			memFile << "Function : " << (&*fi)->getName() << "\n";
			for ( auto ramIter : memoryTable->getAccessRAMFromFunction(&*fi) )
				memFile << "\tNAME : RAM" << ramIter->getRAMId() << "\n";
			memFile << "\n";
		}

		//
		memFile << "\n\n@@@@@@@@@@@@@@ Address Accessed RAMs by Functions @@@@@@@@@@@\n\n";
		for ( auto fi = module->begin(); fi != module->end(); fi++ ) 
		{
			if ( (&*fi)->isDeclaration() )
				continue;
			memFile << "Function : " << (&*fi)->getName() << "\n";
			for ( auto ramIter : memoryTable->getAddressAccessRAMFromFunction(&*fi) )
				memFile << "\tNAME : RAM" << ramIter->getRAMId() << "\n";
			memFile << "\n";
		}


		//
		memFile << "\n\n@@@@@@@@@@@@@@ Private Access Insts of Functions @@@@@@@@@@@\n\n";
		for ( auto fi = module->begin(); fi != module->end(); fi++ ) 
		{
			if ( (&*fi)->isDeclaration() )
				continue;
			memFile << "Function : " << (&*fi)->getName() << "\n";
			for ( auto ramIter : memoryTable->getAccessRAMFromFunction(&*fi) ) {
				memFile << "\tNAME : RAM" << ramIter->getRAMId() << "\n";
				for ( auto instIter : memoryTable->getPrivateAccessInst(&*fi, ramIter) )
					memFile << "\t\t" << *instIter << "\n";
				memFile << "\n";
			}
			memFile << "\n";
		}

		memFile << "\n\n@@@@@@@@@@@@@@ Address Access Insts of Functions @@@@@@@@@@@\n\n";
		for ( auto fi = module->begin(); fi != module->end(); fi++ ) 
		{
			if ( (&*fi)->isDeclaration() )
				continue;
			memFile << "Function : " << (&*fi)->getName() << "\n";
			for ( auto instIter : memoryTable->getAddressAccessInst(&*fi) )
				memFile << "\t" << *instIter << "\n";
			memFile << "\n";
		}

		memFile << "\n\n@@@@@@@@@@@@@@ Unresolved Access Insts of Functions @@@@@@@@@@@\n\n";
		for ( auto fi = module->begin(); fi != module->end(); fi++ ) 
		{
			if ( (&*fi)->isDeclaration() )
				continue;
			memFile << "Function : " << (&*fi)->getName() << "\n";
			for ( auto instIter : memoryTable->getUnresolvedInst(&*fi) )
				memFile << "\t" << *instIter << "\n";
			memFile << "\n";
		}

	}
}
