//Written by csKim

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/BasicBlock.h"

#include "GetMemOper.h"
#include "GetSize.h"
#include "GetDataLayout.h"
#include "LoopPatternAnal.h"

#include <vector>
#include <algorithm>
#include <math.h>

using namespace llvm;
using namespace corelab;

static int lID;

char LoopPatternAnal::ID = 0;
static RegisterPass<LoopPatternAnal> X("LPA", 
		"Loop Pattern Analysis", false, false);

void LoopPatternAnal::getAnalysisUsage( AnalysisUsage &AU ) const
{
	AU.addRequired< LoopInfoWrapperPass >();
	AU.addRequired< PADriver >();
	AU.setPreservesAll();
}

//const static bool debug = false;
const static bool debug = true;

int gcd(int a, int b) {

	while (b != 0) {
		int temp = a % b;
		a = b;
		b = temp;
	}

	return abs(a);
}

// ------------------------------ //

bool LoopPatternAnal::runOnModule(Module& M) {
	if (debug)
		errs() << "\n@@@@@@@@@@ LoopPatternAnal Start @@@@@@@@@@@@@@\n";

	module = &M;
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

	pa = getAnalysis< PADriver >().getPA();

	//Find All Memory Objects
	searchGV();
	searchAlloca();
	searchAllocaCall();

	//Search All Loops
	for ( auto fi = module->begin() ; fi != module->end(); fi++ )
	{
		if ( (&*fi)->isDeclaration() )
			continue;

		if (debug) errs() << (&*fi)->getName().str() << "\n";

		LoopInfo &loopInfo = *loopInfoOf[&*fi];

		//TODO : the order of loops
		std::vector<Loop *> loops( loopInfo.begin(), loopInfo.end() );
//		std::vector<Loop *>::reverse_iterator rit = loops.rbegin();
//		for (; rit != loops.rend(); ++rit )
		for (Loop *rit : loops)
		{
				//TODO : Efficiency Check
			// if this loop is one of the target loops for optimization

			//TODO: It is very un-stable to use in verilog back-end
			// Find all conditions to do this analysis
			// before this analylsis
			//
//			analysisOnLoop( rit );

			//Loop Memory Alias Analysis
			/*
			LoopNode *LN = getLoopNodeFromLoop( rit );
			if ( LN->getStructureDetermined() )
			{
				const Loop *nestedLoop = LN->getLoopFromNest(LN->getMaxNestLevel());
				collectAliasInfo( nestedLoop );
			}
			*/

			//XXX: Loop Alias Analysis For Pipelining
			const Loop *innerMostLoop = getInnerMostLoop( rit );
			assert(innerMostLoop);
			list<BasicBlock *> innerBB = getOnlyOneBB(innerMostLoop);
			errs() << "Get BB List Done\n";
			for ( auto bi : innerBB )
				errs() << bi->getName() << "\n";

			PHINode *phiInst = getCanonicalInductionVariableAuxForAlias(innerMostLoop);
			errs() << "Get Phi Done\n";
			list<Instruction *> exitList;
			exitList.clear();
			if ( innerBB.size() != 0 )
				exitList = getExitList(innerBB);
			errs() << "Get ExitList Done\n";
			if ( innerBB.size() != 0 && phiInst != NULL && exitList.size() != 0 ) {
				errs() << "Collecting Alias Info ...\n";
				LoopAliasInfo *la = collectAliasInfo( innerMostLoop, innerBB , exitList );
				if ( la->getObjDetermined() )
					setBBForPipeline(innerBB);
			}
			else if ( phiInst == NULL )
				errs() << innerMostLoop->getName() << " can not find phi inst\n";
			else if ( exitList.size() == 0 )
				errs() << innerMostLoop->getName() << " can not find exit list\n";
			else
				errs() << innerMostLoop->getName() << " has multiple basic blocks\n";
		}
	}


//	if (debug)
//	{
		errs() << "@@@@@@@@@@ Print Information @@@@@@@@@@@@@@\n";

		std::error_code EC0;
		raw_fd_ostream memWriteFile("MemObject.info", EC0, llvm::sys::fs::OpenFlags::F_Text);
		printMemoryObject(memWriteFile);
		memWriteFile.close();

		errs() << "Print Mem Obj Done\n";

		//TODO : Not Yet on Verilog Back-End
		/*
		std::error_code EC1;
		raw_fd_ostream writeFile("LoopPattern.info", EC1, llvm::sys::fs::OpenFlags::F_Text);
		printAllInfo(writeFile);
			//TODO : Memory Oriented Information
		writeFile.close();

		errs() << "Print Mem Obj Done\n";
		*/

		std::error_code EC;
		raw_fd_ostream aWriteFile("LoopAlias.info", EC, llvm::sys::fs::OpenFlags::F_Text);
		printAliasInfo(aWriteFile);
		aWriteFile.close();

		errs() << "Print Mem Obj Done\n";

//	}

	errs() << "@@@@@@@@@@ Loop Pattern Anal END @@@@@@@@@@@@@@\n\n";

	return false;
}

// ------------------------------ //

PHINode *LoopPatternAnal::getCanonicalInductionVariableAux(const Loop *loop)
{
  BasicBlock *H = loop->getHeader();

  BasicBlock *Incoming = nullptr, *Backedge = nullptr;
  pred_iterator PI = pred_begin(H);
  assert(PI != pred_end(H) && "Loop must have at least one backedge!");
  Backedge = *PI++;
  if (PI == pred_end(H))
    return nullptr; // dead loop
  Incoming = *PI++;
  if (PI != pred_end(H))
    return nullptr; // multiple backedges?

  if (loop->contains(Incoming)) {
    if (loop->contains(Backedge))
      return nullptr;
    std::swap(Incoming, Backedge);
  } else if (!loop->contains(Backedge))
    return nullptr;

  // Loop over all of the PHI nodes, looking for a canonical indvar.
  for (BasicBlock::iterator I = H->begin(); isa<PHINode>(I); ++I) {
    PHINode *PN = cast<PHINode>(I);
    if (ConstantInt *CI =
            dyn_cast<ConstantInt>(PN->getIncomingValueForBlock(Incoming)))
		{
//      if (!CI->isZero())
        if (Instruction *Inc =
                dyn_cast<Instruction>(PN->getIncomingValueForBlock(Backedge)))
				{
          if (Inc->getOpcode() == Instruction::Add && Inc->getOperand(0) == PN) // only add
					{
            if (ConstantInt *CI = dyn_cast<ConstantInt>(Inc->getOperand(1))) // only 1 stride
						{
              if (CI->isOne())
                return PN;
							else
								if (debug) errs() << "IndV Stride is not One\n";
						}
					}
					else
						if (debug) errs() << "IndV incremental operation is not add\n";
				}
		}
 }
	if (debug) errs() << "Can not get Induction Variable :" << loop->getName() <<"\n";
  return nullptr;
}


PHINode *LoopPatternAnal::getCanonicalInductionVariableAuxForAlias(const Loop *loop)
{
  BasicBlock *H = loop->getHeader();

  BasicBlock *Incoming = nullptr, *Backedge = nullptr;
  pred_iterator PI = pred_begin(H);
  assert(PI != pred_end(H) && "Loop must have at least one backedge!");
  Backedge = *PI++;
  if (PI == pred_end(H))
    return nullptr; // dead loop
  Incoming = *PI++;
  if (PI != pred_end(H))
    return nullptr; // multiple backedges?

  if (loop->contains(Incoming)) {
    if (loop->contains(Backedge))
      return nullptr;
    std::swap(Incoming, Backedge);
  } else if (!loop->contains(Backedge))
    return nullptr;

  // Loop over all of the PHI nodes, looking for a canonical indvar.
  for (BasicBlock::iterator I = H->begin(); isa<PHINode>(I); ++I) {
    PHINode *PN = cast<PHINode>(I);
		//Initial Value of indv is constant
    if (ConstantInt *CI =
            dyn_cast<ConstantInt>(PN->getIncomingValueForBlock(Incoming)))
		{
//      if (!CI->isZero())
        if (Instruction *Inc =
                dyn_cast<Instruction>(PN->getIncomingValueForBlock(Backedge)))
				{
          if (Inc->getOpcode() == Instruction::Add && Inc->getOperand(0) == PN) // only add
					{
            if (ConstantInt *CI = dyn_cast<ConstantInt>(Inc->getOperand(1))) // only 1 stride
						{
//              if (CI->isOne())
                return PN;
//							else
//								if (debug) errs() << "IndV Stride is not One\n";
						}
					}
					else
						if (debug) errs() << "IndV incremental operation is not add\n";
				}
		}
  }
	if (debug) errs() << "Can not get Induction Variable :" << loop->getName() <<"\n";
  return nullptr;
}


// ------------------------------ //

bool LoopPatternAnal::simpleLoopCheck(const Loop *loop, unsigned *iterCount)
{
	if ( !loop->isLoopSimplifyForm() )
	{
		errs() << "Non Simplified Form :" << loop->getName() << "\n";
		return false;
	}
	if ( getCanonicalInductionVariableAux(loop) == NULL )
	{
//		errs() << "Can not get Induction Variable :" << loop->getName() <<"\n";
		return false;
	}
	if ( loop->getExitBlock() == NULL )
	{
		errs() << "There are more than Two Exit Block:" << loop->getName() <<"\n";
		return false;
	}
	*iterCount = getIterationCount(loop);
	if ( *iterCount == 0 )
	{
		errs() << "Can not find Iteratoin Count for Loop:" << loop->getName() <<"\n";
		return false;
	}

	return true;
}

// ------------------------------ //

bool LoopPatternAnal::setLoopStructure(LoopNode *LN)
{
	const Loop *loop = LN->getOutMostLoop();
	unsigned iterCount=0;
	unsigned nestLevel = 1;

	if ( !simpleLoopCheck(loop, &iterCount) )
		return false;
	
	LN->setLoopFromNest(nestLevel, loop);
//	LN->setIndV2Nest(loop->getCanonicalInductionVariable(), nestLevel);
	LN->setIndV2Nest(getCanonicalInductionVariableAux(loop), nestLevel);
	LN->setIterCount(nestLevel, iterCount);

	while (1)
	{
		if ( loop->getSubLoops().size() == 0 )
			break;

		Loop *subLoop;

		for ( auto subIter : loop->getSubLoops() )
		{
			nestLevel++;

			subLoop = &*subIter;

			if ( !simpleLoopCheck(subLoop, &iterCount) )
				return false;
		
			LN->setLoopFromNest(nestLevel, subLoop);
//			LN->setIndV2Nest(subLoop->getCanonicalInductionVariable(), nestLevel);
			LN->setIndV2Nest(getCanonicalInductionVariableAux(subLoop), nestLevel);
			LN->setIterCount(nestLevel, iterCount);
		}

		loop = subLoop;
	}

	LN->setMaxNestLevel(nestLevel);

	return true;
}

// ------------------------------ //

void initLoopID(void) { lID = 0; };

int getLoopID(void) {	return ++lID; };

void LoopPatternAnal::analysisOnLoop(const Loop *L) {
	
//	loopaa = getAnalysis< LoopAA >().getTopAA();
//	Loop *cL = const_cast<Loop *>(L);

//Initialize LoopNode
	LoopNode *LN = new LoopNode(L);
	if (setLoopStructure(LN))
	{
		LN->setStructureDetermined(true);
		LN->setObjDetermined(true);
		//collect Loop information into the LN
		for ( unsigned nestLevel = 1; nestLevel <= LN->getMaxNestLevel(); nestLevel++ )
		{
			const Loop *targetL = LN->getLoopFromNest(nestLevel);
			collectUsedObjects(targetL, nestLevel, LN);
		}

		collectLoopPattern(LN);

		setLNList(LN);
	}
	else
	{
		LN->setStructureDetermined(false);
		LN->setObjDetermined(true);

		for ( auto bi = L->block_begin(); bi != L->block_end(); bi++ )
			for ( auto ii = (*bi)->begin(); ii != (*bi)->end(); ii++ )
			{
				Instruction *inst = const_cast<Instruction *>(&*ii);

				if ( isa<StoreInst>(inst) || isa<LoadInst>(inst) )
				{
					Value *pointerV = const_cast<Value *>(getMemOper(inst));
					if ( MemObj *memObj = traceUsedObject(pointerV, 0) )
					{
						LN->insertMemObj(memObj);
						LN->setMemInst2MemObj(inst, memObj);
						LN->setObj2Inst(memObj, inst);
					}
					else
						LN->setObjDetermined(false);
				}
			}

		setLNList(LN);
	}

}

// ------------------------------ //

//XXX: return first meet operator result
std::pair<PHINode *, unsigned> LoopPatternAnal::getStride(Value *targetV)
{
	PHINode *indNULL = NULL;
	if (PHINode *ind = dyn_cast<PHINode>(targetV))
		return make_pair(ind,1);
	if (Instruction *inst = dyn_cast<Instruction>(targetV))
	{
		if (inst->getOpcode() == Instruction::Add ||
				inst->getOpcode() == Instruction::Sub)
		{
			if (PHINode *ind = dyn_cast<PHINode>(inst->getOperand(0)))
				return make_pair(ind,1);
			else if (PHINode *ind = dyn_cast<PHINode>(inst->getOperand(1)))
				return make_pair(ind,1);
		}
		else if (inst->getOpcode() == Instruction::Mul)
		{
			if (PHINode *ind = dyn_cast<PHINode>(inst->getOperand(0)))
			{
				if (ConstantInt *cInt = dyn_cast<ConstantInt>(inst->getOperand(1)))
					return make_pair(ind,cInt->getSExtValue());
			}
			else if (PHINode *ind = dyn_cast<PHINode>(inst->getOperand(1)))
			{
				if (ConstantInt *cInt = dyn_cast<ConstantInt>(inst->getOperand(0)))
				return make_pair(ind,cInt->getSExtValue());
			}
		}
		else if (inst->getOpcode() == Instruction::Shl)
		{
			if (PHINode *ind = dyn_cast<PHINode>(inst->getOperand(0)))
			{
				if (ConstantInt *cInt = dyn_cast<ConstantInt>(inst->getOperand(1)))
					return make_pair(ind,std::pow(2,cInt->getSExtValue()));
			}
			else if (PHINode *ind = dyn_cast<PHINode>(inst->getOperand(1)))
			{
				if (ConstantInt *cInt = dyn_cast<ConstantInt>(inst->getOperand(0)))
					return make_pair(ind,std::pow(2,cInt->getSExtValue()));
			}
		}
		//Complex Operation
		return make_pair(indNULL,0);
	}
	return make_pair(indNULL,0);
}

// ------------------------------ //

unsigned LoopPatternAnal::getStrideSizeFromArray(MemObj *memObj, unsigned dim)
{
	if (dim == 1)
		return 1;

	unsigned size = 1;
	for ( unsigned i = dim - 1; i != 0; i-- )
		size *= memObj->getDimSize(i);
	return size;
}

// ------------------------------ //
//False : RANDOM access
bool LoopPatternAnal::collectIndStride(DenseMap<PHINode *, unsigned> &ind2Stride, 
		GetElementPtrInst *GEPInst, MemObj *memObj)
{
	Value *ptrOperand = GEPInst->getOperand(0);
	Type *ptrType = ptrOperand->getType();
	if ( isa<ArrayType>(dyn_cast<PointerType>(ptrType)->getElementType()) )
	{
		unsigned numOps = GEPInst->getNumOperands();
		for ( unsigned i = 1; i < numOps; i++)
		{
			Value *operandI = GEPInst->getOperand(i);

			if ( ConstantInt *cInt = dyn_cast<ConstantInt>(operandI) )
				continue;

			pair<PHINode *, unsigned> stridePair = getStride(operandI);

			//RANDOM ACCESS CASE
			if ( stridePair.second == 0 )
				return false;

			unsigned dim = memObj->getMaxDimSize() - (i-2);

			ind2Stride[stridePair.first] 
				= stridePair.second * getStrideSizeFromArray(memObj, dim);
		}
	}
	else	
	{
		Value *operandI = GEPInst->getOperand(1);
		assert(operandI);

		pair<PHINode *, unsigned> stridePair = getStride(operandI);

		if ( stridePair.second == 0 )
			return false;

		ind2Stride[stridePair.first] = stridePair.second;

		if ( GetElementPtrInst *GEPOperand = dyn_cast<GetElementPtrInst>(ptrOperand) )
			if ( !collectIndStride(ind2Stride, GEPOperand, memObj) )
				return false;
	}

	return true;

}

// ------------------------------ //

void LoopPatternAnal::collectLoopPattern(LoopNode *LN)
{
//	errs() << "Loop Node : " << LN->getOutMostLoop()->getName() << "\n";

	for ( auto memObjIter : LN->getUsedMemObjList() )
	{
//		errs() << "MemObj : " << memObjIter->getName() << "\n";

		//Initialize AP
		AccessPattern *AP = new AccessPattern(LN->getOutMostLoop(), memObjIter);

		//AP Build : Instruction -> AccessPattern
		for ( auto instIter : LN->getInstSet(memObjIter) )
		{
			//load directly by ptr
			if (v2MemObj.count(getMemOper(instIter)))
			{
				if (debug)
					errs() << getMemOper(instIter)->getName() << " access in Loop\n";
				continue;
			}

			AP->insertAccessInstList(instIter);

			Value *GEPV = const_cast<Value *>(getMemOper(instIter));
			GetElementPtrInst *GEPInst = dyn_cast<GetElementPtrInst>(GEPV);
			assert(GEPInst);

			DenseMap<PHINode *, unsigned> ind2Stride;

			if ( !collectIndStride(ind2Stride, GEPInst, memObjIter) )
			{
				AP->setInst2SAP(instIter, AccessPattern::RANDOM);
				continue;
			}

			//insert inst-> (nest of indV, stride)
			for ( auto indIter : ind2Stride )
				AP->setInst2StridePair(instIter, LN->getNestFromIndV(indIter.first), indIter.second);
			
			const Loop *loopOfInst = LN->getLoopOfInst(instIter);
			unsigned nestOfInst = LN->getNestFromLoop(loopOfInst);
			assert(nestOfInst != 0 && "Can not find NestLevel from Loop\n" );
			AP->setInst2Nest(instIter, nestOfInst);
		}

		//AP Build : inst->simpleAP Setting
		for ( auto instIter : AP->getInstList() )
		{
			if (AccessPattern::UNKNOWN != AP->getSAPOfInst(instIter))
				continue;

			bool columnCandidate = false;
			bool rowCandidate = true;

			vector<pair<unsigned, unsigned>> strideVector = AP->getStridePair(instIter);
			std::sort(strideVector.begin(), strideVector.end());
			if ( strideVector.size() == 0 )
			{
				AP->setInst2SAP(instIter, AccessPattern::CONSTANT);
				continue;
			}
			if ( strideVector.size() == 1 )
			{
				AP->setInst2SAP(instIter, AccessPattern::ROW);
				continue;
			}

			for (int i = 0; i + 1 < strideVector.size(); i++)
			{
				if ( strideVector[i].first >= strideVector[i+1].first )
				{
					errs() << "Use Same IndV in different Loop Nest Level\n";
					AP->setInst2SAP(instIter, AccessPattern::RANDOM);
					break;
				}

//				errs() << strideVector[i].first << " : " << strideVector[i].second << " : ";
//				errs() << LN->getIterCount( strideVector[i].first ) << "\n";
//				errs() << strideVector[i+1].first << " : " << strideVector[i+1].second << " : ";
//				errs() << LN->getIterCount( strideVector[i+1].first ) << "\n";

				if ( i == 0 && strideVector[i].second == 1 )
					columnCandidate = true;
				else 
				{
					if ( strideVector[i].second >= strideVector[i+1].second * 
							LN->getIterCount( strideVector[i+1].first ) )
						rowCandidate = true && rowCandidate;
					else
						rowCandidate = false;
				}
			}

			if ( columnCandidate && rowCandidate )
				AP->setInst2SAP(instIter, AccessPattern::COLUMN);
			else if ( rowCandidate )
				AP->setInst2SAP(instIter, AccessPattern::ROW);
			else
				AP->setInst2SAP(instIter, AccessPattern::MIXED);
		}

		//AP Build : determine Simple AP
		AccessPattern::SimpleAP sAP;
		bool start = true;
		for ( auto instIter : AP->getInstList() )
		{
			if (start)
			{
				sAP = AP->getSAPOfInst(instIter);
				start = false;
			}
			else
			{
				if ( (sAP == AccessPattern::ROW && 
							AP->getSAPOfInst(instIter) == AccessPattern::COLUMN ) ||
						(sAP == AccessPattern::COLUMN && 
							AP->getSAPOfInst(instIter) == AccessPattern::ROW ) )
					sAP = AccessPattern::MIXED;
				else if ( sAP < AP->getSAPOfInst(instIter) )
				 sAP = AP->getSAPOfInst(instIter);
			}

//			errs() << "\tInst : ";
//			instIter->dump();
//			errs() << "\tAccessPattern : " << AP->getAPName(AP->getSAPOfInst(instIter)) << "\n";
		}
//		errs() << "\t\tSAP : " << AP->getAPName(sAP) << "\n";
		AP->setSimpleAP(sAP);

		LN->insertMemObjAP(memObjIter, AP);
//		errs() <<"\n";
	}

//	errs() <<"\n";
}

// ------------------------------ //

void LoopPatternAnal::collectUsedObjects(const Loop *targetL, unsigned nestLevel, LoopNode *LN)
{
	for ( auto bi = targetL->block_begin(); bi != targetL->block_end(); bi++ )
	{
		const BasicBlock *bb = *bi;
		if ( !isUniqueBB(targetL, bb) )
			continue;

		for ( auto ii = bb->begin(); ii != bb->end(); ii++ )
		{
			Instruction *inst = const_cast<Instruction *>(&*ii);

			if ( isa<StoreInst>(inst) || isa<LoadInst>(inst) )
			{
				LN->setInst2Loop(inst, targetL);

				Value *pointerV = const_cast<Value *>(getMemOper(inst));
				if ( MemObj *memObj = traceUsedObject(pointerV, 0) )
				{
					LN->setMemInst2MemObj(inst, memObj);
					LN->insertMemObj(memObj);
					LN->setNestMemObj(nestLevel, memObj);
					LN->setObj2Inst(memObj, inst);
				}
				else
					LN->setObjDetermined(false);
			}
		}
	}

}

// ------------------------------ //

list<Instruction *> LoopPatternAnal::getCallInstList(Function *fcn)
{
	list<Instruction *> callInstCandidates;
	callInstCandidates.clear();

	for ( auto fi = module->begin(); fi != module->end(); fi++ )
		for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
			for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
			{
				Function *callee = NULL;
				if ( CallInst *cInst = dyn_cast<CallInst>(&*ii) )
				{
					callee = cInst->getCalledFunction();
					if ( !callee )
					{
						errs() << "There is indirect call, so interprocedural search stopped\n";
						(&*ii)->dump();
						callInstCandidates.clear();
						return callInstCandidates;
					}
				}
				else if ( InvokeInst *iInst = dyn_cast<InvokeInst>(&*ii) ) 
				{
					callee = iInst->getCalledFunction();
					if ( !callee )
					{
						errs() << "There is indirect call, so interprocedural search stopped\n";
						(&*ii)->dump();
						callInstCandidates.clear();
						return callInstCandidates;
					}
				}

				if ( callee == fcn )
					callInstCandidates.push_back(&*ii);
			}

	return callInstCandidates;
}

// ------------------------------ //
// order argument is only for function argument tracing
MemObj *LoopPatternAnal::traceUsedObject(Value *targetV, unsigned order)
{
	std::set<int> mSet = pa->getPointsToSet(targetV);
	if ( mSet.size() != 1 )
		return NULL;

	for ( auto m : mSet ) {
		Value *memoryV = pa->int2mem[m];
		if ( v2MemObj.count(memoryV) )
			return v2MemObj[memoryV];
	}

	return NULL;
}

// ------------------------------ //


bool LoopPatternAnal::isUniqueBB ( const Loop *targetL, const BasicBlock *targetBB ) {
	
	for ( auto subIter : targetL->getSubLoops() )
	{
		Loop *innerL = &*subIter;

		for ( auto bi = innerL->block_begin(); bi != innerL->block_end(); bi++ )
		{
			const BasicBlock *bb = *bi;

			if ( targetBB == bb )
				return false;
		}
	}
	return true;
}

// ------------------------------ //

unsigned LoopPatternAnal::getIterationCount( const Loop *targetL ) {

	BasicBlock *exitBB = getOutsideExitBlock(targetL);
	if ( exitBB == NULL )
	{
		errs() << "There is no Unique Exit Block\n";
		return 0;
	}

//	PHINode *inductionV = targetL->getCanonicalInductionVariable();
	PHINode *inductionV = getCanonicalInductionVariableAux(targetL);
	if ( inductionV == NULL )
	{
		errs() << "Cannot find induction Variable\n";
		return 0;
	}

	if ( inductionV->getNumIncomingValues() != 2 )
	{
		errs() << "# of Incoming Values is not two\n";
		return 0;
	}

	Value *initV = inductionV->getIncomingValue(0);
	Value *nextV = inductionV->getIncomingValue(1);
	int initNumber;
	int exitNumber;
	int interval;

	if ( ConstantInt *cInt = dyn_cast<ConstantInt>(initV) )
	{
		initNumber = cInt->getSExtValue();
	}
	else if ( ConstantInt *ccInt = dyn_cast<ConstantInt>(nextV) )
	{
		initNumber = ccInt->getSExtValue();
		nextV = initV;
	}
	else
	{
		errs() << "Initial Value of InductionV is not Constant\n";
		return 0;
	}

	Instruction *exitBranch = &exitBB->back();
	if ( BranchInst *bInst = dyn_cast<BranchInst>(exitBranch) )
	{
//		bInst->dump();
		Value *condV = bInst->getCondition();
		if ( CmpInst *cmpInst = dyn_cast<CmpInst>(condV) )
		{
			Value *opV1 = cmpInst->getOperand(0); // ind V
			Value *opV2 = cmpInst->getOperand(1); // exit Number
			if ( ConstantInt *cInt = dyn_cast<ConstantInt>(opV2) )
			{
				exitNumber = cInt->getSExtValue();
			}
			else
			{
				errs() << "Exit condition is not Constant\n";
				return 0;
			}

			if ( opV1 == nextV )
			{
				if ( Instruction *addInst = dyn_cast<Instruction>(opV1) )
				{
					Value *intervalV = addInst->getOperand(1);
					if ( ConstantInt *cInt = dyn_cast<ConstantInt>(intervalV) )
						interval = cInt->getSExtValue();
					else
					{
						errs() << "Interval is not constant\n";
						return 0;
					}
				}
				else
				{
					errs() << "Interval is not made of add instruction\n";
					return 0;
				}
				return ( exitNumber - initNumber )/interval;
			}
			else
			{
				errs() << "Cmp operand != Phi induction \n";
				return 0;
			}
		}
		return 0;
	}
	else
		return 0;


}

// ------------------------------ //

BasicBlock *LoopPatternAnal::getOutsideExitBlock( const Loop *targetL ) {
	
	for ( auto bi = targetL->block_begin(); bi != targetL->block_end(); bi++ )
	{
		const BasicBlock *bb_c = *bi;
		BasicBlock *bb = const_cast<BasicBlock *>(bb_c);
		if ( !isUniqueBB(targetL, bb) )
			continue;

		Instruction *exitBranch = &bb->back();

		if ( BranchInst *bInst = dyn_cast<BranchInst>(exitBranch) )
		{
			for ( unsigned i = 0; i < bInst->getNumSuccessors(); i++ )
			{
				BasicBlock *succB = bInst->getSuccessor(i);

				if ( isUniqueBB(targetL, succB) )
					return bb;
			}
		}

	}

	return NULL;

}

// -------------------Alias Analysis Loop --------------//

// ------------------------------ //
// map< getInst, indInst -> list operations >
// true : use ind Varaible
// false : constant access
bool LoopPatternAnal::collectIndOperation(LoopAliasInfo *LA, Instruction *nowInst,
		list<Instruction *> &operList, GetElementPtrInst *GEP, PHINode *indInst) {

	BasicBlock *bb = nowInst->getParent();
	bool inBB = false;
	for ( auto bi : LA->getBB() )
		if ( bi == bb )
			inBB = true;

	if ( !inBB )
		return false;

	if ( nowInst == indInst )
		return true;

	if (nowInst->getOpcode() == Instruction::Add ||
			nowInst->getOpcode() == Instruction::Sub ||
			nowInst->getOpcode() == Instruction::Shl ||
			nowInst->getOpcode() == Instruction::Mul ||
			nowInst->getOpcode() == Instruction::Or) {
		for ( unsigned i = 0; i < nowInst->getNumOperands(); ++i )
			if ( Instruction *nextInst = dyn_cast<Instruction>(nowInst->getOperand(i)) ) {
				list<Instruction *> tmpList = operList;
				tmpList.push_front(nowInst);
				if ( collectIndOperation(LA, nextInst, tmpList, GEP, indInst) ) {
					operList = tmpList;
					return true;
				}
			}
	}

	if ( isa<ZExtInst>(nowInst) || isa<SExtInst>(nowInst) ) {
		for ( unsigned i = 0; i < nowInst->getNumOperands(); ++i )
			if ( Instruction *nextInst = dyn_cast<Instruction>(nowInst->getOperand(i)) ) {
				list<Instruction *> tmpList = operList;
				tmpList.push_front(nowInst);
				if ( collectIndOperation(LA, nextInst, tmpList, GEP, indInst) ) {
					operList = tmpList;
					return true;
				}
			}
	}

	if ( GetElementPtrInst *GEPInst = dyn_cast<GetElementPtrInst>(nowInst) )
	{
		Value *ptrOperand = GEPInst->getOperand(0);
		Type *ptrType = ptrOperand->getType();
		if ( isa<ArrayType>(dyn_cast<PointerType>(ptrType)->getElementType()) )
		{
			unsigned numOps = GEPInst->getNumOperands();
			for ( unsigned i = 1; i < numOps; i++)
			{
				Value *operandI = GEPInst->getOperand(i);

				if ( ConstantInt *cInt = dyn_cast<ConstantInt>(operandI) )
					continue;

				if ( Instruction *opInst = dyn_cast<Instruction>(operandI) )
				{
					operList.clear();
					if ( collectIndOperation(LA, opInst, operList, GEP, indInst) )
						return true;
				}
			}
			return false;
		}
		else	
		{
			Value *operandI = GEPInst->getOperand(1);
			assert(operandI);

			if ( Instruction *opInst = dyn_cast<Instruction>(operandI) )
			{
				operList.clear();
				if ( collectIndOperation(LA, opInst, operList, GEP, indInst) )
					return true;
				else
					return false;
			}

//TODO : Now only consider inner most loop
//			if ( GetElementPtrInst *GEPOperand = dyn_cast<GetElementPtrInst>(ptrOperand) )
//				if ( collectIndOperation(LA, GEPOperand, operList, GEP, indInst) )
//					return true;
		}
	}

	return false;
}

// ------------------------------ //
//Calculate index from operation list
int LoopPatternAnal::getRefIndex(list<Instruction *> &opList, int ref) {
	int startIndexValue;
	if ( ref == 0 )
		startIndexValue = 2;
	else
		startIndexValue = ref;
	int operand;
	for ( auto nowInst : opList )
	{
		bool findConst = false;
		for ( unsigned i = 0; i < nowInst->getNumOperands(); ++i )
		 if ( ConstantInt *cInt = dyn_cast<ConstantInt>(nowInst->getOperand(i)) ) {
			 operand = cInt->getSExtValue();
			 findConst = true;
			 break;
		 }
		
		if ( !findConst )
			operand = 3; //no meaning this is from instruction of outside of loop

		if (nowInst->getOpcode() == Instruction::Add) {
			startIndexValue += operand;
		}
		else if (nowInst->getOpcode() == Instruction::Sub) {
			startIndexValue -= operand;
		}
		else if (nowInst->getOpcode() == Instruction::Shl) {
			startIndexValue *= std::pow(2,operand);
		}
		else if (nowInst->getOpcode() == Instruction::Mul) {
			startIndexValue *= operand;
		}
		else if (nowInst->getOpcode() == Instruction::Or) {
			startIndexValue |= operand;
		}
	}
	return startIndexValue;
}

// ------------------------------ //

//TODO: Nested Loop is only one
const Loop *LoopPatternAnal::getInnerMostLoop(const Loop *L) {
	const Loop *subLoop = L;
	while (1)
	{
		if ( subLoop->getSubLoops().size() == 0 )
			break;

		Loop *subLoops;

		for ( auto subIter : subLoop->getSubLoops() ) {
			subLoops = &*subIter;
		}

		subLoop = subLoops;
	}
	return subLoop;
}

list<BasicBlock *> LoopPatternAnal::getOnlyOneBB(const Loop *L) {
	list<BasicBlock *> bbList;
	bbList.clear();

	errs() << "Only BB : " << L->getName() << "\n";

	if ( !L->isLoopSimplifyForm() )
		errs() << "Only BB : Not Simplified Form\n";

	if ( L->getExitBlock() == NULL )
		errs() << "Only BB : Do not have exit block\n";

	bool lastCond = false;
	unsigned i = 0;
	BasicBlock *bb;
	for ( auto bi = L->block_begin(); bi != L->block_end(); bi++ )
	{
		bb = const_cast<BasicBlock *>(*bi);
		if ( !lastCond )
			bbList.push_back(bb);

		for ( auto ii = bb->begin(); ii != bb->end(); ii++ )
		{
			Instruction *inst = const_cast<Instruction *>(&*ii);
			if ( isa<CallInst>(inst) || isa<InvokeInst>(inst) ) {
				bbList.clear();
				return bbList;
			}
		}
		i++;
	}

	unsigned iii = 0;
	if ( bbList.size() != 0 )
	for ( auto bi : bbList )
		for ( auto ii = bi->begin(); ii != bi->end(); ii++ )
			if ( BranchInst *bInst = dyn_cast<BranchInst>(&*ii) )
				if ( bInst->isConditional() )
					iii++;

//f ( i == bbList.size() )
	if ( iii == 1 )
		return bbList;
	else {
		bbList.clear();
		return bbList;
	}
}

bool LoopPatternAnal::collectCMPOperation(list<Instruction *> &operList, 
									list<BasicBlock *> &bb, Instruction *now) {
	errs() << "collect CMP : \n";
	now->dump();

	bool inBB = false;
	bool inList = false;
	/*
	for ( auto bi : bb )
		if ( now->getParent() == bi )
			inBB = true;
	if ( !inBB )
		return false;

	for ( auto ii : operList )
		if ( ii == now )
			inList = true;
	if ( inList )
		return true;
	*/
	if ( isa<PHINode>(now) ) {
		list<Instruction *> tmpList;
		for ( unsigned i = 0; i < now->getNumOperands(); ++i )
			if ( Instruction *next = dyn_cast<Instruction>(now->getOperand(i)) )
			{
				errs() << " Operand : \n";
				next->dump();
				//In BB Check
				inBB = false;
				for ( auto bi : bb )
					if ( next->getParent() == bi )
						inBB = true;
				//In List Check
				inList = false;
				for ( auto ii : operList )
					if ( ii == next )
						inList = true;
				if ( inList )
					return true;

				if ( inBB ) {
					operList.push_front(next);
					if ( !collectCMPOperation( operList, bb, next ) )
						return false;
				}

//				tmpList = operList;
//				tmpList.push_front(now);
//				if ( collectCMPOperation( tmpList, bb, next ) ) {
//					operList = tmpList;
//					return true;
//				}
//				else
//					return false;
			}
		return true;
	}
	else if (now->getOpcode() == Instruction::Add ||
					now->getOpcode() == Instruction::Sub ||
					now->getOpcode() == Instruction::Shl ||
					now->getOpcode() == Instruction::Mul ||
					isa<BranchInst>(now) ||
					isa<CmpInst>(now)) {
		list<Instruction *> tmpList;
		for ( unsigned i = 0; i < now->getNumOperands(); ++i )
			if ( Instruction *next = dyn_cast<Instruction>(now->getOperand(i)) ) {
				errs() << " Operand : \n";
				next->dump();
				//In BB Check
				inBB = false;
				for ( auto bi : bb )
					if ( next->getParent() == bi )
						inBB = true;
				//In List Check
				inList = false;
				for ( auto ii : operList )
					if ( ii == next )
						inList = true;
				if ( inList )
					return true;

				if ( inBB ) {
					operList.push_front(next);
					if ( !collectCMPOperation( operList, bb, next ) )
						return false;
				}


//				tmpList = operList;
//				tmpList.push_front(now);
//				if ( collectCMPOperation( tmpList, bb, next ) ) {
//					operList = tmpList;
//					return true;
//				}
//				else
//					return false;
			}
		return true;
	}

	return false;
}

list<Instruction *> LoopPatternAnal::getExitList(list<BasicBlock *> &innerBB) {

	Instruction *exitBranch = NULL;
	for ( auto bi : innerBB )
		for ( auto ii = bi->begin(); ii != bi->end(); ++ii )
			if ( BranchInst *bInst = dyn_cast<BranchInst>(&*ii) )
				if ( bInst->isConditional() )
					exitBranch = &*ii;

	assert(exitBranch && "Can not Find Exit branch\n");

	list<Instruction *> exitList;

	exitList.clear();
	if (collectCMPOperation( exitList, innerBB, exitBranch ))
		return exitList;
	else {
		exitList.clear();
		return exitList;
	}
}


//Inner Most Loop
LoopAliasInfo *LoopPatternAnal::collectAliasInfo(const Loop *L, list<BasicBlock *> &bb,
																		list<Instruction *> &exitList) {
	PHINode *phiInst = getCanonicalInductionVariableAuxForAlias(L);
	assert(phiInst && "collectAliasInfo : no indV\n");

	LoopAliasInfo *LA = new LoopAliasInfo(L, bb, exitList);

	//determine loop type
	if ( bb.size() == 1 )
		LA->setLoopType(LoopAliasInfo::NORMAL);
	else {
		for ( auto bi : bb )
		{
			errs() << "TEST : " << bi->getName() << "\n";
			bool isFromEntry = false;
			for ( auto PI = pred_begin(bi); PI != pred_end(bi); ++PI ) {
				errs() << "\tTEST : " << (*PI)->getName() << "\n";
				bool exist = false;
				for ( auto bii : bb )
					if ( bii == *PI )
						exist = true;

				if ( !exist ) {
					isFromEntry = true;
					break;
				}
			}

			if ( isFromEntry ) {
				BranchInst *bInst = dyn_cast<BranchInst>(&*(--(bi->end())));
				bInst->dump();
				if ( bInst->isConditional() ) {
					LA->setLoopType(LoopAliasInfo::EDGE);
					errs() << "EDGE type\n";
				}
				else {
					LA->setLoopType(LoopAliasInfo::PREHEADER);
					errs() << "Can not handle this PREHEADER type yet\n";
				}
			}
			errs() << "Test done\n";
		}
	}

	errs() << "ExitList : \n";
	for ( auto exitInst : exitList )
		exitInst->dump();
	errs() << "\n";

	setLAList(LA);
	LA->setIndPHINode(phiInst);
	LA->setObjDetermined(true);

	unsigned defOrder = 1;
	unsigned useOrder = 1;

//	if (debug) errs() << L->getName().str() << "\n";

	//Register
	for ( auto bi =  L->block_begin(); bi != L->block_end(); bi++ )
	{
		BasicBlock *bb = const_cast<BasicBlock *>(*bi);
		for ( auto ii = bb->begin(); ii != bb->end(); ii++ )
		{
			Instruction *inst = const_cast<Instruction *>(&*ii);

			useOrder = 1;
			for ( auto bi_ =  L->block_begin(); bi_ != L->block_end(); bi_++ )
			{
				BasicBlock *bb_ = const_cast<BasicBlock *>(*bi);
				for ( auto ii_ = bb_->begin(); ii_ != bb_->end(); ii_++ )
				{
					Instruction *inst_ = const_cast<Instruction *>(&*ii_);

					//
					if ( PHINode *pn = dyn_cast<PHINode>(inst_) )
					{
						unsigned numOps = inst_->getNumOperands();
						for ( unsigned i = 0; i < numOps; i++)
						{
							Value *operandI = inst_->getOperand(i);
							if ( Instruction *opInst = dyn_cast<Instruction>(operandI) )
								if ( opInst == inst )
								{
									if ( defOrder == useOrder )
										assert (0 &&"This can not be possible in SSA form\n");
									//Only Inter, Intra use def chain will be carried by BBCDFG
									if ( defOrder > useOrder )
									{
//										inst->dump();
//										inst_->dump();
										LA->setInterDefList(inst_, inst);
										LA->setInterUseList(inst_, inst);
									}
								}
						}
					}
					useOrder++;
				}
			}//nested loop for
			defOrder++;
		}
	}//loop for


	list<Instruction *> opList;
	//Collect Memory Access 
	for ( auto bi =  L->block_begin(); bi != L->block_end(); bi++ )
	{
		BasicBlock *bb = const_cast<BasicBlock *>(*bi);
		for ( auto ii = bb->begin(); ii != bb->end(); ii++ )
		{
			Instruction *inst = const_cast<Instruction *>(&*ii);

			if ( isa<StoreInst>(inst) )
			{
					Value *pointerV = const_cast<Value *>(getMemOper(inst));
					if ( MemObj *memObj = traceUsedObject(pointerV, 0) )
					{
						Value *memV = memObj->getValue();
						//array
						if (GetElementPtrInst *GEPInst = dyn_cast<GetElementPtrInst>(pointerV))
						{
							opList.clear();
							if ( collectIndOperation(LA, GEPInst, opList, GEPInst, phiInst) )
							{
								errs() << "Collect Ind Test :\n";
								for ( auto test : opList )
									test->dump();
//								errs() << "Store\n";
								LA->setInst2AInfo(inst, LA->genAInfo(memV, true, true, false, opList)); 
//								for ( auto iii : opList)
//									iii->dump();
								// EMPTY means Standard Interation
							}
							else
							{
//								errs() << "Store const\n";
								opList.clear();
								LA->setInst2AInfo(inst, LA->genAInfo(memV, true, true, true, opList));
							}
						}
						else
						{
							opList.clear();
							LA->setInst2AInfo(inst, LA->genAInfo(memV, true, false, true, opList));
						}
					}
					else {
//					else
//						LA->setObjDetermined(false);
						Value *memV = NULL;
						//XXX: This Null Ptr means Global Memory ( untracable )
						if (GetElementPtrInst *GEPInst = dyn_cast<GetElementPtrInst>(pointerV))
						{
							opList.clear();
							if ( collectIndOperation(LA, GEPInst, opList, GEPInst, phiInst) )
							{
								errs() << "Collect Ind Test :\n";
								for ( auto test : opList )
									test->dump();

								LA->setInst2AInfo(inst, LA->genAInfo(memV, true, true, false, opList)); 
							}
							else
							{
								opList.clear();
								LA->setInst2AInfo(inst, LA->genAInfo(memV, true, true, true, opList));
							}
						}
						else
						{
							opList.clear();
							LA->setInst2AInfo(inst, LA->genAInfo(memV, true, false, true, opList));
						}
					}
			}
			if ( isa<LoadInst>(inst) )
			{
					Value *pointerV = const_cast<Value *>(getMemOper(inst));
					if ( MemObj *memObj = traceUsedObject(pointerV, 0) )
					{
						Value *memV = memObj->getValue();
						//array
						if (GetElementPtrInst *GEPInst = dyn_cast<GetElementPtrInst>(pointerV))
						{
							opList.clear();
							if ( collectIndOperation(LA, GEPInst, opList, GEPInst, phiInst) )
							{
								errs() << "Collect Ind Test :\n";
								for ( auto test : opList )
									test->dump();

//								errs() << "Load\n";
								LA->setInst2AInfo(inst, LA->genAInfo(memV, false, true, false, opList));
//								for ( auto iii : opList)
//									iii->dump();
								// EMPTY means Standard Interation
							}
							else
							{
//								errs() << "Load const\n";
								opList.clear();
								LA->setInst2AInfo(inst, LA->genAInfo(memV, false, true, true, opList));
							}
						}
						else
						{
							opList.clear();
							LA->setInst2AInfo(inst, LA->genAInfo(memV, false, false, true, opList));
						}
					}
					else {
//					else
//						LA->setObjDetermined(false);
						Value *memV = NULL;
						//XXX: This Null Ptr means Global Memory ( untracable )
						if (GetElementPtrInst *GEPInst = dyn_cast<GetElementPtrInst>(pointerV))
						{
							opList.clear();
							if ( collectIndOperation(LA, GEPInst, opList, GEPInst, phiInst) )
							{
								errs() << "Collect Ind Test :\n";
								for ( auto test : opList )
									test->dump();

								LA->setInst2AInfo(inst, LA->genAInfo(memV, false, true, false, opList));
							}
							else
							{
								opList.clear();
								LA->setInst2AInfo(inst, LA->genAInfo(memV, false, true, true, opList));
							}
						}
						else
						{
							opList.clear();
							LA->setInst2AInfo(inst, LA->genAInfo(memV, false, false, true, opList));
						}
					}
			}
		}
	}//loop for


	if ( LA->getObjDetermined() )
	{
		defOrder = 1; // acInfo0
		useOrder = 1; // acInfo1

		unsigned defIndex = 1;
		unsigned useIndex = 1;
		//Memory Alias Information
		for ( auto bi =  L->block_begin(); bi != L->block_end(); bi++ )
		{
			BasicBlock *bb = const_cast<BasicBlock *>(*bi);
			for ( auto ii = bb->begin(); ii != bb->end(); ii++ )
			{
				Instruction *inst = const_cast<Instruction *>(&*ii);

				if ( isa<StoreInst>(inst) || isa<LoadInst>(inst) )
				{
					useOrder = 1;
					for ( auto bi_ =  L->block_begin(); bi_ != L->block_end(); bi_++ )
					{
						BasicBlock *bb_ = const_cast<BasicBlock *>(*bi);
						for ( auto ii_ = bb_->begin(); ii_ != bb_->end(); ii_++ )
						{
							Instruction *inst_ = const_cast<Instruction *>(&*ii_);

							if ( isa<StoreInst>(inst_) || isa<LoadInst>(inst_) )
							{
								LoopAliasInfo::AccessInfo acInfo0 = LA->getAccessInfo(inst);
								LoopAliasInfo::AccessInfo acInfo1 = LA->getAccessInfo(inst_);

								// NULL is Global == conservative unit
								if ( acInfo0.value == acInfo1.value ||
										acInfo0.value == NULL || acInfo1.value == NULL )	{
									if ( !acInfo0.array ) {// array is the characteristic of the value
										if ( (acInfo0.store && acInfo1.store ) ||
												(acInfo0.store && !acInfo1.store ) ) {
											if ( defOrder < useOrder ) { //Intra
												LA->setUseAliasInfo(inst_, inst, LoopAliasInfo::Intra, 0);
												LA->setDefAliasInfo(inst, inst_, LoopAliasInfo::Intra, 0);
											}
											else if ( defOrder > useOrder ) { //Inter
												LA->setUseAliasInfo(inst_, inst, LoopAliasInfo::Inter, 0);
												LA->setDefAliasInfo(inst, inst_, LoopAliasInfo::Inter, 0);
											}
										}
									}
									else { //if the object has array type
										if ( (acInfo0.store && acInfo1.store ) ||
												(acInfo0.store && !acInfo1.store ) ) {
											//TODO : Constant index access to array : not handle this yet
											if ( acInfo0.constant || acInfo1.constant ) {
												errs() << "Constant index access to array : not handle this yet\n";
												LA->setObjDetermined(false);
												//assert(0 && "TODO : Constant index access");
												if ( defOrder < useOrder ) { //Intra
													LA->setUseAliasInfo(inst_, inst, LoopAliasInfo::Intra, 0);
													LA->setDefAliasInfo(inst, inst_, LoopAliasInfo::Intra, 0);
												}
												else if ( defOrder > useOrder ) {
													LA->setUseAliasInfo(inst_, inst, LoopAliasInfo::Inter, 0);
													LA->setDefAliasInfo(inst, inst_, LoopAliasInfo::Inter, 0);
												}
											}
											else {
												int ref, gcd1, distance1, gcd2, distance2;
												if ( ConstantInt *CI = dyn_cast<ConstantInt>(phiInst->getOperand(0)) )
													ref = CI->getSExtValue();
												else
													ref = 0;

												//TODO : This Does not consider outer dimension access location from
												//       outer loop
												if ( (defOrder < useOrder) &&
														(defIndex == useIndex)) {	//Intra
													LA->setUseAliasInfo(inst_, inst, LoopAliasInfo::Intra, 0);
													LA->setDefAliasInfo(inst, inst_, LoopAliasInfo::Intra, 0);
												}
												else if (defOrder > useOrder) { //Inter candidates
													// Inter , distance 0 == WAR or WAW
	
													defIndex = getRefIndex(acInfo0.operationList, ref);
													useIndex = getRefIndex(acInfo1.operationList, ref);
//													gcd1 = gcd(defIndex, useIndex);
//													distance1 = useIndex/gcd1 - defIndex/gcd1;
													distance1 = useIndex - defIndex;

													defIndex = getRefIndex(acInfo0.operationList, ref +1);
													useIndex = getRefIndex(acInfo1.operationList, ref +1);
//													gcd2 = gcd(defIndex, useIndex);
//													distance2 = useIndex/gcd2 - defIndex/gcd2;
													distance2 = useIndex - defIndex;

													if ( distance1 != distance2 )
														LA->setVariousDist();

													LA->setUseAliasInfo(inst_, inst, LoopAliasInfo::Inter, distance1);
													LA->setDefAliasInfo(inst, inst_, LoopAliasInfo::Inter, -distance1);
												}
											}
										}
									}//array end
								}
							}
							useOrder++;
						}}//nested loop for

				}
				defOrder++;
			}}//loop for
	}

	return LA;
}

// ------------------------------ //


// -------------------Print Information --------------//

void LoopPatternAnal::printRegDependence(raw_fd_ostream &writeFile, LoopAliasInfo *LA) {
	if ( LA->getObjDetermined() ) {
		writeFile << "\t-Reg Inter Dependence Info ( not contain normal dep )\n\n";
		const Loop *L = LA->getLoop();
		for ( auto bi =  L->block_begin(); bi != L->block_end(); bi++ )
		{
			BasicBlock *bb = const_cast<BasicBlock *>(*bi);
			for ( auto ii = bb->begin(); ii != bb->end(); ii++ )
			{
				Instruction *inst = const_cast<Instruction *>(&*ii);

				if ( LA->getInterUseList(inst).size() != 0 )
				{
					writeFile << "\t\tPrint Use Reg Map :\n";
					for ( auto usedIter : LA->getInterUseList(inst) )
					{
						writeFile << "\t\t\t" << *inst << "\n\t\t\tis used by\n";
						writeFile << "\t\t\t" << *usedIter << "\n\n";
					}
		}}}
	}
}

void LoopPatternAnal::printMemDependence(raw_fd_ostream &writeFile, LoopAliasInfo *LA) {
	if ( LA->getObjDetermined() ) {
		writeFile << "\t-Memory Dependence Info\n\n";
		const Loop *L = LA->getLoop();
		for ( auto bi =  L->block_begin(); bi != L->block_end(); bi++ )
		{
			BasicBlock *bb = const_cast<BasicBlock *>(*bi);
			for ( auto ii = bb->begin(); ii != bb->end(); ii++ )
			{
				Instruction *inst = const_cast<Instruction *>(&*ii);
				if ( isa<StoreInst>(inst) || isa<LoadInst>(inst) ) {
					if ( isa<StoreInst>(inst) )
						writeFile << "\tStore ";
					else
						writeFile << "\tLoad ";
					writeFile << "Mem instruction : " << *inst << "\n";
					if ( LA->getUseAliasList(inst).size() != 0 )
					{
						writeFile << "\t\tPrint Use Mem Map :\n";
						for ( auto usedIter : LA->getUseAliasList(inst) )
						{
							writeFile << "\t\t\t" << *inst << "\n\t\t\tis used by\n";
							writeFile << "\t\t\t" << *(usedIter.first) << "\n";
							LoopAliasInfo::AliasInfo ai = usedIter.second;
							if ( LA->getAccessInfo(inst).value == NULL )
								writeFile << "\t\t\t  null = global ";
							else
								writeFile << "\t\t\t " << LA->getAccessInfo(inst).value->getName().str();
							if ( ai.type == LoopAliasInfo::Intra )
								writeFile << " : Intra : 0\n\n";
							else
								writeFile << " : Inter : " << ai.distance << "\n\n";
						}
					}
					if ( LA->getDefAliasList(inst).size() != 0 )
					{
						writeFile << "\t\tPrint Def Mem Map :\n";
						for ( auto usedIter : LA->getDefAliasList(inst) )
						{
							writeFile << "\t\t\t" << *inst << "\n\t\t\t's def is\n";
							writeFile << "\t\t\t" << *(usedIter.first) << "\n";
							LoopAliasInfo::AliasInfo ai = usedIter.second;
							if ( LA->getAccessInfo(inst).value == NULL )
								writeFile << "\t\t\t null = global ";
							else
								writeFile << "\t\t\t " << LA->getAccessInfo(inst).value->getName().str();
							if ( ai.type == LoopAliasInfo::Intra )
								writeFile << " : Intra : 0\n\n";
							else
								writeFile << " : Inter : " << ai.distance << "\n\n";
						}
					}
					writeFile << "\n";
				}// store || load
			}// bb for end
		}// L for end
	}
	else {
		writeFile << "\tObjects this loop uses can not be determined\n";
		writeFile << "Now... this is because of constant index access\n\n";
	}
}

void LoopPatternAnal::printAliasInfo(raw_fd_ostream &writeFile) {
	writeFile << "/*** Loop Alias Information ... by csKim ***/\n\n";
	writeFile << "/*** The order of instructions is same as the origin ***/\n\n";

	writeFile << "/** Loop Memory Alias Print Example **/\n";
	writeFile << "/** for () **/\n";
	writeFile << "/** inst0 : load A[i] **/\n";
	writeFile << "/** inst1 : store A[i+1] **/\n\n";
	writeFile << "/** ... is used by ... : Obj value : Intra, Inter, distance  **/\n";
	writeFile << "/** inst1 is used by inst0 : A : Inter : 1 **/\n";
	writeFile << "/** inst0's def is inst1 : Inter : -1 **/\n\n";

	for ( auto fi = module->begin() ; fi != module->end(); fi++ )
	{
		Function *fcn = &*fi;
		if ( fcn->isDeclaration() )
			continue;

		writeFile << "Function : " << fcn->getName().str() <<"\n\n";

		LoopInfo &loopInfo = *loopInfoOf[&*fi];
		std::vector<Loop *> loops( loopInfo.rbegin(), loopInfo.rend() );
		for (Loop *rit : loops)
		{
			//			LoopNode *LN = getLoopNodeFromLoop(rit);
			writeFile << "Outmost Loop : " << rit->getName().str() << "\n";
			const Loop *nestedLoop = getInnerMostLoop( rit );
			writeFile << "Inner Most Loop : " << nestedLoop->getName().str() << "\n\n";
			LoopAliasInfo *LA = getLoopAFromLoop(nestedLoop);
			if ( LA ) {
				if ( LA->isVariousDist() )
					writeFile << "\tHas Various Distance in Loop Mem dep\n\n";
				else {
					writeFile << "INDV : " << *LA->getIndPHINode() << "\n\n";
					printRegDependence(writeFile, LA);
					printMemDependence(writeFile, LA);
				}
			}
			else
				writeFile << "\tHas Multiple Basic Block or Not Simple Loop\n\n";
		}

	}

	writeFile << "\n";
	
}

// ------------------------------ //

void LoopPatternAnal::printMemoryObject(raw_fd_ostream &writeFile) {
	writeFile << "/*** Memory Object Information ... by csKim ***/\n\n";
	writeFile << "/*** Memory Array Dimension Example : Array[3][2][1] ***/\n\n";

	for ( auto memIter : memObjList )
	{
		writeFile << "Memory Object\n";
		writeFile << "\tName : " << memIter->getName().str() << "\n";
		writeFile << "\tby AllocaCall : " << memIter->isCallInstBased() << "\n";
		writeFile << "\tis External : " << memIter->isExternalValue() << "\n";
		if ( !memIter->isExternalValue() )
		{
			writeFile << "\tBitWidth : " << memIter->getDataBitSize() << "\n";
			writeFile << "\tNumOfElements : " << memIter->getNumOfElements() << "\n";
			writeFile << "\tMaxDimension : " << memIter->getMaxDimSize() << "\n";
			for ( unsigned i = memIter->getMaxDimSize(); i != 0; i-- )
			{
				writeFile << "\t\t" << i << " level dimension size : ";
				writeFile << memIter->getDimSize(i) << "\n";
			}
		}
		writeFile << "\n";
	}

}

// ------------------------------ //

void LoopPatternAnal::printAllInfo(raw_fd_ostream &writeFile) {
	writeFile << "/*** Memory Object Information ... by csKim ***/\n\n";
	writeFile << "/*** The Order of Loops in a Function is Correct ***/\n";
	writeFile << "/*** But The Order of Functions is Not Correct ***/\n\n";

	writeFile << "/*** Loop Nest Level Example : ***/\n";
	writeFile << "/*** Loop {\t\t\t: 1 Level***/\n";
	writeFile << "/*** \tLoop { \t\t: 2 Level***/\n";
	writeFile << "/*** \t\tLoop { \t: 3 Level***/\n";
	writeFile << "/*** }}}\t\t\t\t\t\t***/\n";

	writeFile << "/** Memory Access Patterns in a target Loop **/\n\n";
	
	for ( auto fi = module->begin() ; fi != module->end(); fi++ )
	{
		Function *fcn = &*fi;
		if ( fcn->isDeclaration() )
			continue;

		writeFile << "Function : " << fcn->getName().str() <<"\n\n";
		
		LoopInfo &loopInfo = *loopInfoOf[&*fi];
		std::vector<Loop *> loops( loopInfo.rbegin(), loopInfo.rend() );
		for (Loop *rit : loops)
			printLoopPattern(rit, writeFile);

	}


	writeFile << "\n/** Loops which uses a target Memory Object **/\n\n";
}

// ------------------------------ //

void LoopPatternAnal::printLoopPattern(const Loop *L, raw_fd_ostream &writeFile) {

//	errs() << L->getHeader()->getName() << "\n";
	LoopNode *LN = getLoopNodeFromLoop(L);
	writeFile << "Loop : " << L->getName().str() << "\n";
	if ( !LN->getStructureDetermined() )
		writeFile << "\tThis Loop is not simplified form\n";
	else
	{
		if ( LN != NULL )
		{
//			writeFile << "LN : " << LN->getName().str() << "\n";
			writeFile << "\tLoop Max Nest Level : " << LN->getMaxNestLevel() << "\n";
			for ( int i = 1; i <= LN->getMaxNestLevel(); ++i )
				writeFile << "\tLoop Nest " << i << " Itercount : " << LN->getIterCount(i) << "\n";
			writeFile << "\n";

			writeFile << "Loop : " << L->getName().str() << "\n";
			writeFile << "Obj Determined : ";
			if ( LN->getObjDetermined() )
				writeFile << "true";
			else
				writeFile << "false";

			writeFile << "\n\nFailed Instruction : \n";
				for ( auto fIter : LN->getFailList() )
					writeFile << fIter->getName().str() << "\n";
			writeFile << "\n\n";
			//errs() << "1\n";
			for ( auto memObjIter : LN->getUsedMemObjList() )
			{
				if (!memObjIter->isExternalValue())
				{
					//errs() << "2\n";
					writeFile << "\tUsed Memory Object : " << memObjIter->getName().str() << "\n";
					AccessPattern *AP = LN->getAccessPattern(memObjIter);
					writeFile << "\t\tAccessPattern : ";
					writeFile << (AP->getAPName(AP->getSimpleAP())).str() << "\n\n";
					//errs() << "3\n";
					for ( auto instIter : AP->getInstList() )
					{
						//errs() << "4\n";
						writeFile << "\t\tAccess Instruction : ";
						writeFile << instIter->getName().str() << "\n";
						writeFile << "\t\t\tType : ";
						if ( isa<StoreInst>(instIter) )
							writeFile << "Store\n";
						else
							writeFile << "Load\n";
						writeFile << "\t\t\tAccessPattern : ";
						writeFile << (AP->getAPName(AP->getSAPOfInst(instIter))).str() << "\n";
						writeFile << 
							"\t\t\tAccess LoopNestLevel of Induction Variable | Stride | IterCount :\n";
						vector<pair<unsigned, unsigned>> strideVector = AP->getStridePair(instIter);
						std::sort(strideVector.begin(), strideVector.end());
						//errs() << "5\n";
						for ( int i = 0; i < strideVector.size(); i++ )
						{
							//errs() << "6\n";
							writeFile << "\t\t\t\t" << strideVector[i].first << " | ";
							writeFile << strideVector[i].second << " | ";
							writeFile << LN->getIterCount( strideVector[i].first ) << "\n";
							//errs() << "7\n";
						}
						writeFile << "\n";
					}
				}
				writeFile << "\n\n";
			}
			writeFile << "\n";
		}
	}
}

// ------------------------------ //

void LoopPatternAnal::searchGV(void){

	const DataLayout &DL = module->getDataLayout();
	for (auto gvIter = module->global_begin(); gvIter != module->global_end(); gvIter++)
	{
		if ( (&*gvIter)->isDeclaration() ) // external such as stderr
		{
			MemObj *memObj = new MemObj(&*gvIter, false, true, DL);
			memObjList.push_back(memObj);
			v2MemObj[&*gvIter] = memObj;
			continue;
		}
		if ( (&*gvIter)->getName() == "llvm.used" ) continue;

		MemObj *memObj = new MemObj(&*gvIter, false, false, DL);

		memObjList.push_back(memObj);
		v2MemObj[&*gvIter] = memObj;
	}
}

// ------------------------------ //

void LoopPatternAnal::searchAlloca(void){

	const DataLayout &DL = module->getDataLayout();
	for ( auto fi = module->begin(); fi != module->end(); fi++ )
		for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
			for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				if ( AllocaInst *alloca = dyn_cast<AllocaInst>(&*ii) )
				{
					MemObj *memObj = new MemObj(alloca, false, false, DL);

					memObjList.push_back(memObj);
					v2MemObj[alloca] = memObj;
				}

}

// ------------------------------ //

void LoopPatternAnal::searchAllocaCall(void){

	const DataLayout &DL = module->getDataLayout();
	for ( auto fi = module->begin(); fi != module->end(); fi++ )
		for ( auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
			for ( auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				if ( CallInst *callInst = dyn_cast<CallInst>(&*ii) )
				{
					Function *callee = callInst->getCalledFunction();
					if ( callee->getName() == "malloc" )
					{
						MemObj *memObj = new MemObj(&*ii, true, false, DL);

						memObjList.push_back(memObj);
						v2MemObj[&*ii] = memObj;
					}
				}
}

// ---------------------Memory Object Class Methods -------------------------//

ElementInfo MemObj::getElementInfo(Type *eTy, const DataLayout &DL)
{
	if ( ArrayType *aTy = dyn_cast<ArrayType>(eTy) )
	{
		ElementInfo nowInfo;
		nowInfo.numOfElements = aTy->getNumElements();
		nestArrayStructure.push_back(aTy->getNumElements());

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
		nowInfo.dataWidth = DL.getPointerSizeInBits();
		return nowInfo;
	}
	else if ( StructType *sTy = dyn_cast<StructType>(eTy) )
	{
		//TODO: Dealing With Pattern Anal...?
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

// ------------------------------ //

MemObj::MemObj(Value *v_, bool isCallInst_, bool isExternal_, const DataLayout &DL) 
	: v(v_), isCallInst(isCallInst_), isExternal(isExternal_)
{
	if (!isExternal)
	{
		if (isCallInst)
		{
			CallInst *cInst = dyn_cast<CallInst>(v);
			assert(cInst && "isCallInst :Mem Obj Value is not Call Instruction\n");

			Value *op0 = cInst->getArgOperand(0);
			ConstantInt *cInt = dyn_cast<ConstantInt>(op0);
			assert(cInt && "Operand of Alloca Call is not constant value\n");
			unsigned sext = cInt->getZExtValue();

			Type *cTy = op0->getType();
			IntegerType *iTy = dyn_cast<IntegerType>(cTy);

			dataBitSize = iTy->getBitWidth();
			unsigned dataByteSize = dataBitSize / 8;
			numOfElements = (sext / dataByteSize);
			maxDim = 1;
			dim2Size[maxDim] = numOfElements;
		}
		else
		{
			ty = v->getType();
			PointerType *pTy = dyn_cast<PointerType>(ty);
			assert(pTy && "Type of Memory Value should be Pointer Type\n");
			eTy = pTy->getElementType();
			assert(eTy);

			ElementInfo elementInfo = getElementInfo( eTy, DL );
			dataBitSize = elementInfo.dataWidth;
			numOfElements = elementInfo.numOfElements;

			unsigned iterCount = 0;
			for ( auto iter : nestArrayStructure )
				iterCount++;

			maxDim = iterCount;

			for ( auto iter : nestArrayStructure )
			{
				dim2Size[iterCount] = iter;
				iterCount--;
			}
		}
	}
}

// ------------------------------ //

unsigned MemObj::getDimSize(unsigned dim)
{
	if ( dim > maxDim )
		assert(0 && "MemObj Max Dim < input \n");
	else
		return dim2Size[dim];
}

// ---------------------Loop Node Class Methods -------------------------//


// ------------------------------ //

