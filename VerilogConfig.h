#ifndef VERILOG_CONFIG_H
#define VERILOG_CONFIG_H

#include <set>
#include <map>
#include <string>
#include <stdio.h>

#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"

#include <tcl8.5/tcl.h>

using namespace std;
using namespace llvm;

namespace corelab {

	// This struct contains FPGA Operation Information.
	typedef struct OperationInfo {
		string opName;
		float delay;
		int latency[2];
	} OperationInfo;

	typedef string OperationInfoKey;
	typedef map<OperationInfoKey, OperationInfo> OperationInfoMap;

	// This class contains Verilog Configuration Information.
	class VerilogConfigInfo {
		public:
			VerilogConfigInfo() : bluetooth(false), wifi(false), 
			bufferSize(64), bufferSizeBit(6), thread(false), allLUT(false),
			privateBuffer(false), privateReg(false), fullChaining(false),
			ramInit(false), pipeline(false), accel(false), accelName(std::string("")),
			validMemory(false), callBus(false), flatOperator(false),
			fadd(0), fsub(0), fmul(0), fdiv(0), frem(0), dadd(0), dsub(0), dmul(0), ddiv(0), drem(0),
			memoryBitWidth(0), validation(false), no_mc(false), no_mf(false),
			centralizedMemory(false)
			{	};
			~VerilogConfigInfo() {};

			void addOperationInfo(string opName, float delay, int latency) {
				OperationInfoMap::iterator it = operationInfoMap.find(opName);

				if ( it == operationInfoMap.end() )
				{
					OperationInfo opInfo;
					opInfo.opName = opName;
					opInfo.delay = delay;
					opInfo.latency[0] = latency;
					opInfo.latency[1] = -1;
					operationInfoMap[opName] = opInfo;
				}
				else
				{
					if ( it->second.latency[1] != -1 )
						errs() << "Operation Info more than 2 for one op Name\n";
					else
						it->second.latency[1] = latency;
				}
			};

			void setClockPeriod(float period_) {
				clock_period = period_;
			};

			void setBluetooth(bool bluetooth_) {
				bluetooth = bluetooth_;
			};

			void setWifi(bool wifi_) {
				wifi = wifi_;
			};

			void setBufferSize(unsigned bufferSize_) { bufferSize = bufferSize_; };
			void setBufferSizeBit(unsigned bufferSizeBit_) { bufferSizeBit = bufferSizeBit_; };

			void setThread(bool thread_) {
				thread = thread_;
			};

			void setAllLUT(bool allLUT_)
			{ allLUT = allLUT_; };

			void setPrivateBuffer(bool privateBuffer_)
			{ privateBuffer = privateBuffer_; };

			void setPrivateReg(bool privateReg_)
			{ privateReg = privateReg_; };

			void setFullChaining(bool fullChaining_)
			{ fullChaining = fullChaining_; };

			void setRAMInit(bool ramInit_)
			{ ramInit = ramInit_; };

			void setPipeline(bool pipeline_)
			{ pipeline = pipeline_; };

			void setAccel(bool accel_)
			{ accel = accel_; };

			void setValidMemory(bool vm)
			{ validMemory = vm; };

			void setFAdd(int fadd_, int dadd_)
			{ 
				fadd = fadd_;
				dadd = dadd_;
			}

			void setFSub(int fsub_, int dsub_)
			{ 
				fsub = fsub_; 
				dsub = dsub_;
			}

			void setFMul(int fmul_, int dmul_)
			{ 
				fmul = fmul_; 
				dmul = dmul_;
			}
	
			void setFDiv(int fdiv_, int ddiv_)
			{ 
				fdiv = fdiv_; 
				ddiv = ddiv_;
			}		

			void setFRem(int frem_, int drem_)
			{ 
				frem = frem_; 
				drem = drem_;
			}

			void setAccelFunctionName(std::string accelName_)
			{ accelName = accelName_; };

			void setAccelFunction(Function *accelFunction_)
			{ accelFunction = accelFunction_; };

			void setCallBus(bool callBus_) { callBus = callBus_; }

			void setFlatOperator(bool flatOperator_) { flatOperator = flatOperator_; }

			void setMemoryBitWidth(unsigned memoryBitWidth_) { memoryBitWidth = memoryBitWidth_; }

			void setValidationMode(bool val) { validation = val; }

			void setNoMC(bool val) { no_mc = val; }
			void setNoMF(bool val) { no_mf = val; }

			void setCentralizedMemory(bool val) { centralizedMemory = val; }

			std::string getOpNameFromInst(Instruction *inst);

			OperationInfo getOperationInfo(Instruction *inst) {
				if ( !isa<GetElementPtrInst>(inst) )
					std::string opName = getOpNameFromInst(inst);
				std::string opName = "nothing";
				if ( operationInfoMap.find(opName) == operationInfoMap.end() )
				{
					//TODO : Local Memory
					if ( isa<LoadInst>(inst) )
					{
						OperationInfo opInfo;
						opInfo.opName = std::string("load");
						opInfo.delay = 0;
						opInfo.latency[0] = 1;
						opInfo.latency[1] = -1;
						return opInfo;
					}
					else if ( isa<StoreInst>(inst) )
					{
						OperationInfo opInfo;
						opInfo.opName = std::string("store");
						opInfo.delay = 0;
						opInfo.latency[0] = 1;
						opInfo.latency[1] = -1;
						return opInfo;
					}
					else if ( MDNode *md = inst->getMetadata("network") )
					{
						OperationInfo opInfo;
						opInfo.opName = std::string("network_call");
						opInfo.delay = 0;
						opInfo.latency[0] = 2;
						opInfo.latency[1] = -1;
						return opInfo;
					}
					else if ( MDNode *md = inst->getMetadata("thread") )
					{
						OperationInfo opInfo;
						opInfo.opName = std::string("thread_call");
						opInfo.delay = 0;
						opInfo.latency[0] = 2;
						opInfo.latency[1] = -1;
						return opInfo;
					}
					else if ( MDNode *md = inst->getMetadata("sleep") )
					{
						OperationInfo opInfo;
						opInfo.opName = std::string("sleep");
						opInfo.delay = 0;
						opInfo.latency[0] = 2;
						opInfo.latency[1] = -1;
						return opInfo;
					}
					else if ( inst->getOpcode() == Instruction::FAdd ) { 
						OperationInfo opInfo;
						opInfo.opName = std::string("fadd");
						opInfo.delay = 0;
						opInfo.latency[0] = fadd;
						opInfo.latency[1] = dadd;
						return opInfo;
					}
					else if ( inst->getOpcode() == Instruction::FSub ) { 
						OperationInfo opInfo;
						opInfo.opName = std::string("fadd");
						opInfo.delay = 0;
						opInfo.latency[0] = fsub;
						opInfo.latency[1] = dsub;
						return opInfo;
					}
					else if ( inst->getOpcode() == Instruction::FMul ) { 
						OperationInfo opInfo;
						opInfo.opName = std::string("fmul");
						opInfo.delay = 0;
						opInfo.latency[0] = fmul;
						opInfo.latency[1] = dmul;
						return opInfo;
					}
					else if ( inst->getOpcode() == Instruction::FDiv ) { 
						OperationInfo opInfo;
						opInfo.opName = std::string("fdiv");
						opInfo.delay = 0;
						opInfo.latency[0] = fdiv;
						opInfo.latency[1] = ddiv;
						return opInfo;
					}
					else if ( inst->getOpcode() == Instruction::FRem ) { 
						OperationInfo opInfo;
						opInfo.opName = std::string("frem");
						opInfo.delay = 0;
						opInfo.latency[0] = frem;
						opInfo.latency[1] = drem;
						return opInfo;
					}
					

					OperationInfo opInfo;
					opInfo.opName = std::string("nothing");
					opInfo.delay = 0;
					opInfo.latency[0] = 0;
					opInfo.latency[1] = -1;
					return opInfo;
				}
				return operationInfoMap[opName];
			};

			OperationInfo getOperationInfo(string opName) {
				return operationInfoMap[opName];
			};

			float getClockPeriod(void) { return clock_period;	};

			bool getBluetoothUse(void) { return bluetooth; };

			bool getWifiUse(void) { return wifi; };
			unsigned getBufferSize(void) { return bufferSize; };
			unsigned getBufferSizeBit(void) { return bufferSizeBit; };

			bool getThreadUse(void) { return thread; };
			bool getAllLUT(void) { return allLUT; };
			bool getPrivateBuffer(void) { return privateBuffer; };
			bool getPrivateReg(void) { return privateReg; };
			bool getFullChaining(void) { return fullChaining; };
			bool getRAMInit(void) { return ramInit; };
			bool getPipeline(void) { return pipeline; };
			bool getAccel(void) { return accel; };
			bool getValidMemory(void) { return validMemory; };
			std::string getAccelFunctionName(void) { return accelName; };
			Function *getAccelFunction(void) { return accelFunction; };

			bool getCallBus(void) { return callBus; }
			bool getFlatOperator(void) { return flatOperator; }

			unsigned getMemoryBitWidth(void) { return memoryBitWidth; }

			bool getValidationMode(void) { return validation; }

			bool getNoMC(void) { return no_mc; }
			bool getNoMF(void) { return no_mf; }

			bool getCentralizedMemory(void) { return centralizedMemory; }

		private:
			bool populateStringsForOneOperandInstr(Instruction *instr, std::string params[10]);
			bool populateStringsForTwoOperandInstr(Instruction *instr, std::string params[10]);
			bool populateStringsForThreeOperandInstr(Instruction *instr, std::string params[10]);
			int maxBitWidth(int width0, int width1, int width2);
			bool isSupportedBitwidth(int width);
			std::string assembleOpNameFromStringList(std::string params[10]);
			bool isAnyOfTwoOperandsZero(Instruction *instr);
			bool isSecondOperandZero(Instruction *instr);
			bool isSecondOperandPowerOfTwo(Instruction *instr);
			bool isSecondOperandConstant(Instruction *instr);
			bool isBinaryOperatorNoOp(Instruction *instr);
			bool populateStringsForBinaryOperator(Instruction *instr, std::string params[10]);
			void populateStringsForICmpInst(const ICmpInst *cmp, std::string params[10]);
			void populateStringsForFCmpInst(const FCmpInst *cmp, std::string params[10]);


			OperationInfoMap operationInfoMap;
			float clock_period;
			bool bluetooth;
			bool wifi;
			unsigned bufferSize;
			unsigned bufferSizeBit;
			bool thread;
			bool allLUT;
			bool privateBuffer;
			bool privateReg;
			bool fullChaining;
			bool ramInit;
			bool pipeline;
			bool accel;
			bool validMemory;
			bool validation;
			bool no_mc;
			bool no_mf;
			bool centralizedMemory;

			int fadd;
			int fsub;
			int fmul;
			int fdiv;
			int frem;
			int dadd;
			int dsub;
			int dmul;
			int ddiv;
			int drem;

			bool callBus;
			bool flatOperator;

			unsigned memoryBitWidth;

			std::string accelName;
			Function *accelFunction;
	};


	// This class contains methods for building VerilogConfigInfo object.
	class VerilogConfigBuilder {
		public:
			VerilogConfigBuilder(string configFileName_) {
				configFileName = configFileName_;
			};

			~VerilogConfigBuilder() {};

			VerilogConfigInfo *getVerilogConfigInfo(void);

		private:
			Tcl_Interp *setTclCommandToMyFunction(void);

			string configFileName;
			VerilogConfigInfo *verilogConfigInfo;
	};

} //end namespace

#endif
