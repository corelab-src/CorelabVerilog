#ifndef NLT_H
#define NLT_H

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"

#include <list>
#include <set>

#define BUFFER_SIZE 64

namespace corelab
{
using namespace llvm;
using namespace std;

	class NLT : public ModulePass
	{
		public:
			enum NetworkType{
				NoNetwork = 0,
				SingleOpenCall = 1,
				ServerCreateCall = 2,
				ReadCall = 3,
				WriteCall = 4,
				NonBlockingReadCall = 5,
				AcceptCall = 6
			};

			enum NetworkMode{
				ClientMode = 0,
				ServerMode = 1
			};

			struct RWValues{
				Value *socket;
				Value *data;
				Value *size;
			};

			enum ThreadFcnType{
				NoThreadFcn = 0,
				CreateCall = 1,
				JoinCall = 2,
				MutexInitCall = 3,
				MutexLockCall = 4,
				MutexUnlockCall = 5,
				CreateStore = 6
			};

			struct ThreadFcnOperands{
				Value *op0;
				Value *op1;
				Value *op2;
			};

			NLT() : ModulePass(ID) {}

			virtual StringRef getPassName() const { return "Network Lib Transform"; }

			static char ID;

			virtual void getAnalysisUsage(AnalysisUsage &AU) const;
			bool runOnModule(Module &M);

			NLT *getNLT(void) { return this; };

			void setNetworkType(const Instruction *inst, NetworkType nty)
			{
				inst2Nty[inst] = nty;
			}

			NetworkType getNetworkType(const Instruction *inst)
			{
				if ( inst2Nty.find(inst) == inst2Nty.end() )
					return NoNetwork;
				else
					return inst2Nty[inst];
			}

			void setRWValues(const Instruction *inst,
					Value *socket, Value *data, Value *size)
			{
				RWValues rwValues;
				rwValues.socket = socket;
				rwValues.data = data;
				rwValues.size = size;

				inst2RWInfo[inst] = rwValues;
			}

			Value *getNSocket(const Instruction *inst)
			{ return inst2RWInfo[inst].socket; }
		
			Value *getNData(const Instruction *inst)
			{ return inst2RWInfo[inst].data; }

			Value *getNSize(const Instruction *inst)
			{ return inst2RWInfo[inst].size; }

			unsigned getNSizeUInt(const Instruction *inst)
			{
				Value *sizeV = getNSize(inst);
				assert(sizeV);

				if (ConstantInt *cInt = dyn_cast<ConstantInt>(sizeV))
					return cInt->getZExtValue();
				else
					return BUFFER_SIZE;
			}

			void setNetworkMode(NetworkMode nm_)	{	nm = nm_; }
			NetworkMode getNetworkMode(void) { return nm; }

			void setThreadFcnType(const Instruction *inst, ThreadFcnType type)
			{	inst2Type[inst] = type; }

			ThreadFcnType getThreadFcnType(const Instruction *inst)
			{
				if (inst2Type.find(inst) == inst2Type.end())
					return NoThreadFcn;
				else
					return inst2Type[inst];
			}

			void setThreadFcnOperands(const Instruction *inst, 
					Value *op0_, Value *op1_, Value *op2_)
			{
				ThreadFcnOperands ops;
				ops.op0 = op0_;
				ops.op1 = op1_;
				ops.op2 = op2_;

				inst2Ops[inst] = ops;
			}

			ThreadFcnOperands getThreadFcnOperands(const Instruction *inst)
			{
				if (inst2Ops.find(inst) == inst2Ops.end())
				{
					ThreadFcnOperands ops;
					ops.op0 = nullptr;
					ops.op1 = nullptr;
					ops.op2 = nullptr;

					return ops;
				}
				else
					return inst2Ops[inst];
			}

			void setFcnUseNetworkRead(Function *fcn)
			{ fcn2NetworkRead[fcn] = true; }
			bool getFcnUseNetworkRead(Function *fcn)
			{
				if ( fcn2NetworkRead.find(fcn) == fcn2NetworkRead.end() )
					return false;
				else
					return true;
			}

			void setFcnUseNetworkWrite(Function *fcn)
			{ fcn2NetworkWrite[fcn] = true; }
			bool getFcnUseNetworkWrite(Function *fcn)
			{
				if ( fcn2NetworkWrite.find(fcn) == fcn2NetworkWrite.end() )
					return false;
				else
					return true;
			}

			void setFcnUseMutex(Function *fcn)
			{ fcn2Mutex[fcn] = true; }
			bool getFcnUseMutex(Function *fcn)
			{
				if ( fcn2Mutex.find(fcn) == fcn2Mutex.end() )
					return false;
				else
					return true;
			}

			void setFcnUseSleep(Function *fcn)
			{ fcn2Sleep[fcn] = true; }
			bool getFcnUseSleep(Function *fcn)
			{
				if ( fcn2Sleep.find(fcn) == fcn2Sleep.end() )
					return false;
				else
					return true;
			}

			void setInstUseSleep(Instruction *inst)
			{ inst2Sleep[inst] = true; }
			bool getInstUseSleep(Instruction *inst)
			{
				if ( inst2Sleep.find(inst) == inst2Sleep.end() )
					return false;
				else
					return true;
			}

			void setSleepFcnOperand(Instruction *inst, Value *op)
			{ inst2SleepOp[inst] = op; }
			Value *getSleepFcnOperand(Instruction *inst)
			{ return inst2SleepOp[inst]; }

			void setThreadFcn(Function *fcn)
			{ threadFcnList.push_back(&*fcn); }
			list<Function *> getThreadFcnList(void)
			{ return threadFcnList; }

		private:
			Module *module;
			NetworkMode nm;

			void networkFcnTransform(void);
			void threadFcnTransform(void);
			void sleepFcnTransform(void);

			DenseMap<const Instruction *, NetworkType> inst2Nty;
			DenseMap<const Instruction *, RWValues> inst2RWInfo; 

			DenseMap<const Instruction *, ThreadFcnType> inst2Type;
			DenseMap<const Instruction *, ThreadFcnOperands> inst2Ops;
			list<Function *> threadFcnList;

			DenseMap<const Instruction *, bool> inst2Sleep;
			DenseMap<const Instruction *, Value *> inst2SleepOp;

			DenseMap<Function *, bool> fcn2NetworkRead;
			DenseMap<Function *, bool> fcn2NetworkWrite;
			DenseMap<Function *, bool> fcn2Mutex;
			DenseMap<Function *, bool> fcn2Sleep;
	};

}
#endif
