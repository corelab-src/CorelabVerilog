#include "NLT.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Constants.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;
using namespace corelab;


#define DEBUG_NLT

static RegisterPass<NLT> X("NLT", "Custom FPGA Lib Transform", false, false);
void NLT::getAnalysisUsage(AnalysisUsage &AU) const
{
	AU.setPreservesAll();
}
char NLT::ID = 0;

void NLT::networkFcnTransform(void) {

	LLVMContext &Context = module->getContext();

	bool localChange = true;
	for (auto fi = module->begin(); fi != module->end(); fi++ )
		for (auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
		{
			localChange = true;

			while (localChange) {
				localChange = false;

				for (auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;

					if ( CallInst *cInst = dyn_cast<CallInst>(inst) )
					{
						Function *fcn = cInst->getCalledFunction();
						if (fcn)
						{
							if ( fcn->getName() == "fpga_tcp_singleConnectionOpen" )
							{
								Metadata *name = (Metadata*)MDString::get(Context, "SingleOpenCall");
								Metadata *apJoin = (Metadata*)MDString::get(Context, "Corelab-2.4G");

								Metadata *valuesArray[] = {name, apJoin};
								ArrayRef<Metadata *> values(valuesArray, 2);
								MDNode *mdNode = MDNode::get(Context, values);

								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "sc", cInst);
								nop->setMetadata("network", mdNode);

								setNetworkType(nop, SingleOpenCall);

								//ReplaceInstWithInst(cInst, nop);
								cInst->replaceAllUsesWith(nop);
								cInst->eraseFromParent();
								
								localChange = true;

								setNetworkMode(ClientMode);
								break;
							}
							else if ( fcn->getName() == "fpga_tcp_serverCreate" )
							{
								Metadata *name = (Metadata*)MDString::get(Context, "ServerCreateCall");
								Metadata *ipAddress = (Metadata*)MDString::get(Context, "192.168.4.1");
								Metadata *socket = (Metadata*)MDString::get(Context, "333");
								Metadata *valuesArray[] = {name, ipAddress, socket};
								ArrayRef<Metadata *> values(valuesArray, 3);
								MDNode *mdNode = MDNode::get(Context, values);

								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "server", cInst);
								nop->setMetadata("network", mdNode);

								setNetworkType(nop, ServerCreateCall);

								cInst->eraseFromParent();
								
								localChange = true;

								setNetworkMode(ServerMode);
								break;
							}
							else if ( fcn->getName() == "fpga_tcp_accept" )
							{
								Metadata *name = (Metadata*)MDString::get(Context, "AcceptCall");
								Metadata *valuesArray[] = {name};
								ArrayRef<Metadata *> values(valuesArray, 1);
								MDNode *mdNode = MDNode::get(Context, values);

								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "accept", cInst);
								nop->setMetadata("network", mdNode);

								setNetworkType(nop, AcceptCall);

								cInst->replaceAllUsesWith(nop);
								cInst->eraseFromParent();
								
								localChange = true;
								break;
							}
							else if ( fcn->getName() == "fpga_tcp_read" )
							{
								Metadata *name = (Metadata*)MDString::get(Context, "ReadCall");

								Metadata *valuesArray[] = {name};
								ArrayRef<Metadata *> values(valuesArray, 1);
								MDNode *mdNode = MDNode::get(Context, values);
								
								//Insert NOP instruction
								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "read", cInst);
								nop->setMetadata("network", mdNode);

								//set this nop instruction is about network instruction
								setNetworkType(nop, ReadCall);

								//TODO:socket number
								//operand 0
								Value *op0 = cInst->getArgOperand(0);

								//data location
								//operand 1
								Value *op1 = cInst->getArgOperand(1);
								//read(int , char *, int);
								/*
								Instruction *op1Inst = dyn_cast<Instruction>(op1);
								BitCastInst *bitCastInst = dyn_cast<BitCastInst>(op1Inst);
								assert(bitCastInst && "It may be bitcast Instruction");
								Value *ptrV = bitCastInst->getOperand(0);
								assert(ptrV && "no pointer??");
								*/

								//size
								//operand 2
								Value *op2 = cInst->getArgOperand(2);

								//set Read Info Values
								//setRWValues(nop, nullptr, ptrV, op2);
								setRWValues(nop, op0, op1, op2);


								//Remove CallInstruction
								cInst->eraseFromParent();

								setFcnUseNetworkRead(&*fi);

								localChange = true;
								break;
							}
							else if ( fcn->getName() == "fpga_tcp_non_blocking_read" )
							{
								Metadata *name = (Metadata*)MDString::get(Context, "NonBlockingReadCall");

								Metadata *valuesArray[] = {name};
								ArrayRef<Metadata *> values(valuesArray, 1);
								MDNode *mdNode = MDNode::get(Context, values);
								
								//Insert NOP instruction
								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "read", cInst);
								nop->setMetadata("network", mdNode);

								//set this nop instruction is about network instruction
								setNetworkType(nop, NonBlockingReadCall);

								//socket number
								//operand 0
								Value *op0 = cInst->getArgOperand(0);

								//data location
								//operand 1
								Value *op1 = cInst->getArgOperand(1);

								//size
								//operand 2
								Value *op2 = cInst->getArgOperand(2);

								//set Read Info Values
								//setRWValues(nop, nullptr, ptrV, op2);
								setRWValues(nop, op0, op1, op2);

								cInst->replaceAllUsesWith(nop);
								//Remove CallInstruction
								cInst->eraseFromParent();

								setFcnUseNetworkRead(&*fi);

								localChange = true;
								break;
							}
							else if ( fcn->getName() == "fpga_tcp_write" )
							{
								Metadata *name = (Metadata*)MDString::get(Context, "WriteCall");

								Metadata *valuesArray[] = {name};
								ArrayRef<Metadata *> values(valuesArray, 1);
								MDNode *mdNode = MDNode::get(Context, values);
								
								//Insert NOP instruction
								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "write", cInst);
								nop->setMetadata("network", mdNode);

								//set this nop instruction is about network instruction
								setNetworkType(nop, WriteCall);

								//TODO:socket number
								//operand 0
								Value *op0 = cInst->getArgOperand(0);

								//data location
								//operand 1
								Value *op1 = cInst->getArgOperand(1);

								//size
								//operand 2
								Value *op2 = cInst->getArgOperand(2);

								//set Read Info Values
								setRWValues(nop, op0, op1, op2);

								//Remove CallInstruction
								cInst->eraseFromParent();

								setFcnUseNetworkWrite(&*fi);

								localChange = true;
								break;
							}
						}
					}
				}//for end
			}
		}

	//Remove Function Definitions

	localChange = true;

	while (localChange) {
		localChange = false;
		
		for (auto fi = module->begin(); fi != module->end(); fi++ )
		{
			Function *fcn = &*fi;

			if ( fcn->getName() == "fpga_tcp_singleConnectionOpen" ||
					fcn->getName() == "fpga_tcp_read" ||
					fcn->getName() == "fpga_tcp_write" ||
					fcn->getName() == "fpga_tcp_accept" ||
					fcn->getName() == "fpga_tcp_nonBlockingRead" ||
					fcn->getName() == "fpga_tcp_serverCreate" )
			{
				fcn->eraseFromParent();	
				localChange = true;
				break;
			}	
		}
	}



}

void NLT::threadFcnTransform(void) {

	LLVMContext &Context = module->getContext();

	bool localChange = true;
	for (auto fi = module->begin(); fi != module->end(); fi++ )
		for (auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
		{
			localChange = true;
			bool storeInsert = true;

			while (localChange) {
				localChange = false;

				for (auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;

					if ( CallInst *cInst = dyn_cast<CallInst>(inst) )
					{
						Function *fcn = cInst->getCalledFunction();
						if (fcn)
						{
							if ( fcn->getName() == "fpga_pthread_create" )
							{
								if (storeInsert)
								{
									Metadata *name = (Metadata*)MDString::get(Context, "store create info");

									Metadata *valuesArray[] = {name};
									ArrayRef<Metadata *> values(valuesArray, 1);
									MDNode *mdNode = MDNode::get(Context, values);

									Value *threadPtr = cInst->getArgOperand(0);
								
									Value *op1 = cInst->getArgOperand(1);
//									op1->dump();

									Value *fcnPtr;
									if ( Function *fcn = dyn_cast<Function>(op1) )
									{
										fcnPtr = op1;	
									}
									else
									{
										if ( User *uV = dyn_cast<User>(op1) )
										{
//											uV->dump();
											fcnPtr = uV->getOperand(0);
										}
										else
										assert(0 && "can not find pthread function pointer\n");
									}
									
									Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
									StoreInst *sInst = new StoreInst(dum, threadPtr, cInst);
									sInst->setMetadata("thread_store", mdNode);
//									cInst->insertBefore(sInst);

									setThreadFcnOperands(sInst, fcnPtr, nullptr, nullptr);

									storeInsert = false;
									localChange = true;

									break;
								}
								else
								{
								Metadata *name = (Metadata*)MDString::get(Context, "pthread_create");

								Metadata *valuesArray[] = {name};
								ArrayRef<Metadata *> values(valuesArray, 1);
								MDNode *mdNode = MDNode::get(Context, values);

								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "create", cInst);
								nop->setMetadata("thread", mdNode);

								setThreadFcnType(nop, CreateCall);

								Value *threadPtr = cInst->getArgOperand(0);
								
								Value *op1 = cInst->getArgOperand(1);
								Value *fcnPtr;
								if ( Function *fcn = dyn_cast<Function>(op1) )
								{
									fcnPtr = op1;	
								}
								else
								{
									if ( User *uV = dyn_cast<User>(op1) )
									{
//										uV->dump();
										fcnPtr = uV->getOperand(0);
									}
									else
										assert(0 && "can not find pthread function pointer\n");
								}

								if ( Function *fcn = dyn_cast<Function>(fcnPtr) )
									setThreadFcn(fcn);

								Value *argPtr = cInst->getArgOperand(2);

								setThreadFcnOperands(nop, threadPtr, fcnPtr, argPtr);

								cInst->replaceAllUsesWith(nop);
								cInst->eraseFromParent();
								
								storeInsert = true;
								localChange = true;

								break;
								}
							}
							else if ( fcn->getName() == "fpga_pthread_join" )
							{
								Metadata *name = (Metadata*)MDString::get(Context, "pthread_join");

								Metadata *valuesArray[] = {name};
								ArrayRef<Metadata *> values(valuesArray, 1);
								MDNode *mdNode = MDNode::get(Context, values);

								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "join", cInst);
								nop->setMetadata("thread", mdNode);

								setThreadFcnType(nop, JoinCall);

								Value *op0 = cInst->getArgOperand(0);

								setThreadFcnOperands(nop, op0, nullptr, nullptr);

								cInst->replaceAllUsesWith(nop);
								cInst->eraseFromParent();

								localChange = true;

								break;
							}
							else if ( fcn->getName() == "fpga_pthread_mutex_init" )
							{
								if (storeInsert)
								{
									Metadata *name = (Metadata*)MDString::get(Context, "store create mutex info");

									Metadata *valuesArray[] = {name};
									ArrayRef<Metadata *> values(valuesArray, 1);
									MDNode *mdNode = MDNode::get(Context, values);

									Value *mutexPtr = cInst->getArgOperand(0);

									Value *dum = ConstantInt::get(Type::getInt8Ty(Context), 0);
									StoreInst *sInst = new StoreInst(dum, mutexPtr, cInst);
									sInst->setMetadata("thread_mutex_store", mdNode);

									storeInsert = false;
									localChange = true;

									break;
								}
								else
								{
								Metadata *name = (Metadata*)MDString::get(Context, "pthread_mutex_init");

								Metadata *valuesArray[] = {name};
								ArrayRef<Metadata *> values(valuesArray, 1);
								MDNode *mdNode = MDNode::get(Context, values);

								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "init", cInst);
								nop->setMetadata("thread", mdNode);

								setThreadFcnType(nop, MutexInitCall);

								Value *op0 = cInst->getArgOperand(0);

								setThreadFcnOperands(nop, op0, nullptr, nullptr);

								cInst->replaceAllUsesWith(nop);
								cInst->eraseFromParent();

								storeInsert = true;
								localChange = true;

								break;
								}
							}
							else if ( fcn->getName() == "fpga_pthread_mutex_lock" )
							{
								Metadata *name = (Metadata*)MDString::get(Context, "pthread_mutex_lock");

								Metadata *valuesArray[] = {name};
								ArrayRef<Metadata *> values(valuesArray, 1);
								MDNode *mdNode = MDNode::get(Context, values);

								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "lock", cInst);
								nop->setMetadata("thread", mdNode);

								setThreadFcnType(nop, MutexLockCall);

								Value *op0 = cInst->getArgOperand(0);

								setThreadFcnOperands(nop, op0, nullptr, nullptr);

								cInst->replaceAllUsesWith(nop);
								cInst->eraseFromParent();

								localChange = true;

								setFcnUseMutex(&*fi);

								break;
							}
							else if ( fcn->getName() == "fpga_pthread_mutex_unlock" )
							{
								Metadata *name = (Metadata*)MDString::get(Context, "pthread_mutex_unlock");

								Metadata *valuesArray[] = {name};
								ArrayRef<Metadata *> values(valuesArray, 1);
								MDNode *mdNode = MDNode::get(Context, values);

								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "unlock", cInst);
								nop->setMetadata("thread", mdNode);

								setThreadFcnType(nop, MutexUnlockCall);

								Value *op0 = cInst->getArgOperand(0);

								setThreadFcnOperands(nop, op0, nullptr, nullptr);

								cInst->replaceAllUsesWith(nop);
								cInst->eraseFromParent();

								localChange = true;

								setFcnUseMutex(&*fi);

								break;
							}

						}
					}//call inst if
				}//for end
			}//while end
		}//for end

	//Remove Function Definitions

	localChange = true;

	while (localChange) {
		localChange = false;
		
		for (auto fi = module->begin(); fi != module->end(); fi++ )
		{
			Function *fcn = &*fi;

			if ( fcn->getName() == "fpga_pthread_create" ||
					fcn->getName() == "fpga_pthread_join" ||
					fcn->getName() == "fpga_pthread_mutex_init" ||
					fcn->getName() == "fpga_pthread_mutex_lock" ||
					fcn->getName() == "fpga_pthread_mutex_unlock" ) 
			{
				fcn->eraseFromParent();	
				localChange = true;
				break;
			}	
		}
	}

}

void NLT::sleepFcnTransform(void) {

	LLVMContext &Context = module->getContext();

	bool localChange = true;
	for (auto fi = module->begin(); fi != module->end(); fi++ )
		for (auto bi = (&*fi)->begin(); bi != (&*fi)->end(); bi++ )
		{
			localChange = true;

			while (localChange) {
				localChange = false;

				for (auto ii = (&*bi)->begin(); ii != (&*bi)->end(); ii++ )
				{
					Instruction *inst = &*ii;

					if ( CallInst *cInst = dyn_cast<CallInst>(inst) )
					{
						Function *fcn = cInst->getCalledFunction();
						if (fcn)
						{
							if ( fcn->getName() == "fpga_sleep" )
							{
								Metadata *name = (Metadata*)MDString::get(Context, "Sleep");

								Metadata *valuesArray[] = {name};
								ArrayRef<Metadata *> values(valuesArray, 1);
								MDNode *mdNode = MDNode::get(Context, values);

								Value *dum = ConstantInt::get(Type::getInt32Ty(Context), 0);
								Instruction *nop = (Instruction *)
									BinaryOperator::Create(Instruction::Add, dum, dum, "sleep", cInst);
								nop->setMetadata("sleep", mdNode);

								Value *op0 = cInst->getArgOperand(0);
								Value *constOp;
								if ( ConstantInt *cInt = dyn_cast<ConstantInt>(op0) )
									constOp = op0;
								else
									//bitcast user
									if ( User *uV = dyn_cast<User>(op0) )
										constOp = uV->getOperand(1);

								assert( isa<ConstantInt>(constOp) );
								setSleepFcnOperand(nop, constOp);

								setInstUseSleep(nop);
								setFcnUseSleep(&*fi);

								//cInst->replaceAllUsesWith(nop);
								cInst->eraseFromParent();
								
								localChange = true;

								break;
							}
						}//
					}//call inst begin
				}//block begin
			}//while begin
		}//module begin, fi begin

	localChange = true;

	while (localChange) {
		localChange = false;
		
		for (auto fi = module->begin(); fi != module->end(); fi++ )
		{
			Function *fcn = &*fi;

			if ( fcn->getName() == "fpga_sleep" )
			{
				fcn->eraseFromParent();	
				localChange = true;
				break;
			}	
		}
	}
}


bool NLT::runOnModule(Module &M) {
	errs() << "@@@@@@@@@@NLT Start@@@@@@@@@@\n";
	module = &M;

	networkFcnTransform();

	threadFcnTransform();

	sleepFcnTransform();

	errs() << "@@@@@@@@@@NLT END@@@@@@@@@@\n";
	return true;
}

