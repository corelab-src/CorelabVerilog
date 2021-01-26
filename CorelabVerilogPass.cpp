#include "CorelabVerilogPass.h"

#define DEBUG_VERILOG_PASS

using namespace llvm;

namespace corelab {

	void CorelabVerilogPass::initAnalysis(void){
#ifdef DEBUG_VERILOG_PASS
		errs() << "@@@@@@@@@@Analysis Initialization START@@@@@@@@@@\n";
#endif


		nlt = getAnalysis< NLT >().getNLT();
#ifdef DEBUG_VERILOG_PASS
		if ( !nlt ) assert(0 && "NLT FAIL\n");
		errs() << "NLT DONE\n";
#endif

		bbcdfg = getAnalysis< BBCDFG >().getBBCDFG();
#ifdef DEBUG_VERILOG_PASS
		if ( !bbcdfg ) assert(0 && "BBCDFG FAIL\n");
		errs() << "BBCDFG DONE\n";
#endif

		pa = getAnalysis<PADriver>().getPA();
#ifdef DEBUG_VERILOG_PASS
		if ( !pa ) assert(0 && "PA Build FAIL\n");
		errs() << "PA Build DONE\n";
#endif

		VerilogConfigBuilder *builder = new VerilogConfigBuilder("config.tcl");
#ifdef DEBUG_VERILOG_PASS
		if ( !builder ) assert(0 && "Configuration Builder FAIL\n");
		errs() << "BUILDER DONE\n";
#endif

		verilogConfigInfo = builder->getVerilogConfigInfo();
		free(builder);
#ifdef DEBUG_VERILOG_PASS
		if ( !verilogConfigInfo ) assert(0 && "Configuration TCL FAIL\n");
		errs() << "TCL DONE\n";
#endif

		if ( verilogConfigInfo->getAccel() ) {
			errs() << "ACCEL NAME : " << verilogConfigInfo->getAccelFunctionName() << "\n";
			for ( auto fi = module->begin(); fi != module->end(); ++fi ) {
				if ( (&*fi)->isDeclaration() ) continue;
				errs() << "TEST : " << (&*fi)->getName() << "\n";
				if ( (&*fi)->getName().str() == verilogConfigInfo->getAccelFunctionName() ) {
					errs() << "ACCEL FUNCTION FIND : " << (&*fi)->getName() << "\n";
					verilogConfigInfo->setAccelFunction(&*fi);
					break;
				}
			}
			assert(verilogConfigInfo->getAccelFunction() != NULL && "Wrong Accel FCN\n");
		}

//		LPA = getAnalysis< LoopPatternAnal >().getLPA();
		LPA = nullptr;
#ifdef DEBUG_VERILOG_PASS
//		if ( !LPA ) assert(0 && "Loop Pattern Analysis FAIL\n");
		errs() << "LPA DONE\n";
#endif	

#ifdef DEBUG_VERILOG_PASS
		errs() << "@@@@@@@@@@Analysis Initialization DONE@@@@@@@@@@\n";
#endif
	}

	bool CorelabVerilogPass::runOnModule(Module &M){
		
		module = &M;

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

		initAnalysis();

		DL = new DataLayout(&M);
		assert(DL);

#ifdef DEBUG_VERILOG_PASS
		errs() << "@@@@@@@@@@BackEnd Start@@@@@@@@@@\n";
#endif

		OperatorTableBuilder *operatorTableBuilder = new OperatorTableBuilder(&M, verilogConfigInfo);
		OperatorTable *operatorTable = operatorTableBuilder->getOperatorTable();

		errs() << "Operator Table Generation Done\n";

		CallTableBuilder *callTableBuilder = new CallTableBuilder(&M, verilogConfigInfo);
		CallTable *callTable = callTableBuilder->getCallTable();

		errs() << "Call Table Generation Done\n";

		MemoryTableBuilder *memoryTableBuilder = 
			new MemoryTableBuilder(&M, DL, pa, verilogConfigInfo, callTable);
		MemoryTable *memoryTable = memoryTableBuilder->getMemoryTable();

		errs() << "Memory Table Generation Done\n";

		BitWidthTableBuilder *bwtB = 
			new BitWidthTableBuilder(&M, memoryTable, callTable, 
					loopInfoOf, pa, verilogConfigInfo->getWifiUse(),
					verilogConfigInfo->getAccel()?verilogConfigInfo->getAccelFunction():NULL);
		BitWidthTable *bitWidthTable = bwtB->getBitWidthTable();


//		FSMBuilder *fsmBuilder = new FSMBuilder(bbcdfg, verilogConfigInfo, &M, mm, LPA, ms);
		FSMBuilder *fsmBuilder = 
			new FSMBuilder(bbcdfg, verilogConfigInfo, &M, memoryTable, callTable, 
																							operatorTable, bitWidthTable, LPA);

		fsm = fsmBuilder->getFSM();
		free(fsmBuilder);

#ifdef DEBUG_VERILOG_PASS
		errs() << "FSM BUILDER DONE\n";
#endif

//////////////NEW////////////////
		PrintVerilog *printVerilog = 
			new PrintVerilog(&M, verilogConfigInfo, fsm, LPA, 
					memoryTable, callTable, operatorTable, bitWidthTable, pa);

		printVerilog->printVerilog();



#ifdef DEBUG_VERILOG_PASS
		errs() << "@@@@@@@@@@Corelab Verilog BackEnd DONE@@@@@@@@@@\n";
#endif

		return false;
	}

	void CorelabVerilogPass::getAnalysisUsage(AnalysisUsage &AU) const{
				AU.addRequired< LoopInfoWrapperPass >();
				AU.addRequired<NLT>();
				AU.addRequired<AAResultsWrapperPass>();
				AU.addRequired<BBCDFG>();
				AU.addRequired<PADriver>();
//				AU.addRequired<LoopPatternAnal>();
        AU.setPreservesAll();
	}


}//end namespace

