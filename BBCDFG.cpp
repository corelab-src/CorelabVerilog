#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"

#include "BBCDFG.h"
#include "GetSize.h"
#include "GetMemOper.h"

#include <system_error>

#define DEBUG_BBCDFG

namespace corelab
{
	static RegisterPass<BBCDFG> X("BBCDFG", "basic block CDFG", false, false);

	void BBCDFG::genBBCDFG_Map(void)
	{
		for ( auto fi = module->begin(); fi != module->end(); ++fi)
		{
			Function *fcn = &*fi;
			if ( fcn->isDeclaration() )
				continue;

			aa = &getAnalysis<AAResultsWrapperPass>(*fcn).getAAResults();

			for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); ++bi)
				for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ++ii)
				{
					Instruction *inst = &*ii;


					genBBCDFG_Reg_Map_for_Inst( inst );

					for ( auto ii_before = (&*bi)->begin() ; ii_before != ii; ++ii_before)
					{
						Instruction *inst_before = &*ii_before;
						//RAW , WAR , or WAW
						if ((isa<LoadInst>(inst) && isa<StoreInst>(inst_before)) ||
								(isa<StoreInst>(inst) && isa<LoadInst>(inst_before)) ||
								(isa<StoreInst>(inst) && isa<StoreInst>(inst_before)) 
							 )
						{
							Value *ptr1 = getMemOper(inst);
							Value *ptr2 = getMemOper(inst_before);
							assert ( ptr1 && ptr2 );

							/*
							if ( !aa->isNoAlias(MemoryLocation::get(inst_before),
										MemoryLocation::get(inst)) )
								memDepMap[inst].insert(inst_before);*/

							
							
							if ( !aa->isNoAlias(MemoryLocation::get(inst_before),
										MemoryLocation::get(inst)) && 
									!paaa->isNoAlias(ptr1, paaa->getAccessSize(inst),
																	ptr2, paaa->getAccessSize(inst_before)) )
								memDepMap[inst].insert(inst_before);
							/*
							if ( !paaa->isNoAlias(ptr1, paaa->getAccessSize(inst),
																	ptr2, paaa->getAccessSize(inst_before)) )
								memDepMap[inst].insert(inst_before);*/
						}
					}

					for ( auto ii_after = ii; ii_after != (&*bi)->end(); ++ii_after)
					{
						if ( ii_after == ii )
							continue;

						Instruction *inst_after = &*ii_after;

						//RAW , WAR , or WAW
						if ((isa<LoadInst>(inst) && isa<StoreInst>(inst_after)) ||
								(isa<StoreInst>(inst) && isa<LoadInst>(inst_after)) ||
								(isa<StoreInst>(inst) && isa<StoreInst>(inst_after)))
						{
							Value *ptr1 = getMemOper(inst);
							Value *ptr2 = getMemOper(inst_after);
							assert ( ptr1 && ptr2 );
/*
							if ( !aa->isNoAlias(MemoryLocation::get(inst),
										MemoryLocation::get(inst_after)) )
								memUseMap[inst].insert(inst_after);*/

						
							if ( !aa->isNoAlias(MemoryLocation::get(inst),
										MemoryLocation::get(inst_after)) &&
									!paaa->isNoAlias(ptr1, paaa->getAccessSize(inst),
																	ptr2, paaa->getAccessSize(inst_after)) )
								memUseMap[inst].insert(inst_after);
						/*	
							if ( !paaa->isNoAlias(ptr1, paaa->getAccessSize(inst),
																	ptr2, paaa->getAccessSize(inst_after)) )
								memUseMap[inst].insert(inst_after);*/
						}
					}

				}
		}
	}


	void BBCDFG::genBBCDFG_Reg_Map_for_Inst(Instruction *inst)
	{
		BasicBlock *bb = inst->getParent();
		
		//regUseMap
		for ( auto ui = inst->user_begin(); ui != inst->user_end(); ++ui)
		{
			Instruction *user = dyn_cast<Instruction>(*ui);
			assert(user);
//			user->dump();
			if ( bb != user->getParent() )
				continue;

			regUseMap[inst].insert(user);
		}

		//regDepMap
		for ( auto oi = inst->op_begin(); oi != inst->op_end(); ++oi)
		{
			Value *v = dyn_cast<Value>(*oi);
			assert(v && "dyn cast value fail");
//			v->dump();
			// this Value can be BB, Constant, Global_Value, or Instruction
			Instruction *dep = dyn_cast<Instruction>(v);

			if ( !dep )
				continue;

			if ( bb != dep->getParent() )
				continue;

			regDepMap[inst].insert(dep);
		}

//		errs() << "def check done\n";
	}

	void BBCDFG::get_BBCDFG_File(void)
	{
		*regMapFile << " [ PRINT USE REG MAP DATA ] \n\n";
		/*
		for ( auto regI = regUseMap.begin(); regI != regUseMap.end(); ++regI )
		{
			*regMapFile << *((&*regI)->getFirst()) << "\n";
			for ( auto useI = (&*regI)->getSecond().begin(); 
					useI != (&*regI)->getSecond().end(); ++useI )
				*regMapFile << "->\t" << **useI << "\n";
			*regMapFile << "\n";
		}*/

		for ( auto regI = regDepMap.begin(); regI != regDepMap.end(); ++regI )
		{
			*regMapFile << *((&*regI)->getFirst()) << "\n";
			for ( auto useI = (&*regI)->getSecond().begin(); 
					useI != (&*regI)->getSecond().end(); ++useI )
				*regMapFile << "<-\t" << **useI << "\n";
			*regMapFile << "\n";
		}


		*memMapFile << " [ PRINT USE MEM MAP DATA ] \n\n";
		for ( auto memI = memUseMap.begin(); memI != memUseMap.end(); ++memI )
		{
			*memMapFile << *((&*memI)->getFirst()) << "\n";
			for ( auto useI = (&*memI)->getSecond().begin(); 
					useI != (&*memI)->getSecond().end(); ++useI )
				*memMapFile << "->\t" << **useI << "\n";
			*memMapFile << "\n";
		}
	}

	bool BBCDFG::runOnModule(Module &M)
	{
		module = &M;
		pa = getAnalysis< PADriver >().getPA();

		assert(loopInfoOf.empty() && "ERROR LOOP INFO TWICE CALLED\n\n");
		loopInfoOf.clear();
		for ( auto fi = module->begin(); fi != module->end(); fi++ )
		{
			Function &F = *fi;
			if ( (&*fi)->isDeclaration() )
				continue;

			LoopInfo *li = 
				new LoopInfo(std::move(getAnalysis< LoopInfoWrapperPass >(F).getLoopInfo()));
			loopInfoOf[&*fi] = li;
		}

		paaa = new PABasedAA(module, pa, loopInfoOf);

#ifdef DEBUG_BBCDFG
//		loopaa->print(errs());
		std::error_code ec_print = std::make_error_code(std::errc::io_error);
		raw_fd_ostream regMapFile_("regMap.debug", ec_print, llvm::sys::fs::F_None);
		raw_fd_ostream memMapFile_("memMap.debug", ec_print, llvm::sys::fs::F_None);
		regMapFile = &regMapFile_;
		memMapFile = &memMapFile_;
#endif
		errs() << "BB : Loop AA DONE??\n";

		genBBCDFG_Map();
		
		errs() << "BB : MAP DONE\n";

#ifdef DEBUG_BBCDFG
		get_BBCDFG_File();
#endif

		return false;
	}

	void BBCDFG::getAnalysisUsage(AnalysisUsage &AU) const
	{
		AU.addRequired<AAResultsWrapperPass>();
		AU.addRequired< LoopInfoWrapperPass >();
		AU.addRequired< PADriver >();
		AU.setPreservesAll();
	}

	char BBCDFG::ID = 0;
} //end namespace
