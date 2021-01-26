#include "llvm/IR/Constants.h"
#include "llvm/ADT/StringExtras.h"

#include "VerilogConfig.h"


#define DEBUG_VERILOG_CONFIG

using namespace llvm;
using namespace std;

namespace corelab {

	std::string VerilogConfigInfo::getOpNameFromInst(Instruction *inst){
		std::string params[10];
    int width0 = 0, width1 = 0, width2 = 0;

    switch (inst->getNumOperands()) {
    case 1:
        if (isa<SIToFPInst>(inst)) {
            // altfp_sitofp32 converts int (32) to float (32)
            // altfp_sitofp64 converts int (32) to double (64)
            // width0 should be width of output
            width0 = inst->getType()->getPrimitiveSizeInBits();
        } else {
            width0 = inst->getOperand(0)->getType()->getPrimitiveSizeInBits();
        }
        if (!populateStringsForOneOperandInstr(inst, params))
            return "";
        break;

    case 2:
        width0 = inst->getOperand(0)->getType()->getPrimitiveSizeInBits();
        width1 = inst->getOperand(1)->getType()->getPrimitiveSizeInBits();
        if (!populateStringsForTwoOperandInstr(inst, params))
            return "";
        break;

    case 3:
        width0 = inst->getOperand(0)->getType()->getPrimitiveSizeInBits();
        width1 = inst->getOperand(1)->getType()->getPrimitiveSizeInBits();
        width2 = inst->getOperand(2)->getType()->getPrimitiveSizeInBits();
        if (!populateStringsForThreeOperandInstr(inst, params))
            return "";
        break;
    }

    int BitWidth = maxBitWidth(width0, width1, width2);
    int endindex;
    for (endindex = 0; params[endindex] != ""; endindex++)
        ;

    if (!isSupportedBitwidth(BitWidth) || endindex == 0) {
        return "";
    }

    params[endindex] = utostr(BitWidth);

    return assembleOpNameFromStringList(params);
	}

	bool VerilogConfigInfo::populateStringsForOneOperandInstr(Instruction *instr,
                                                    std::string params[10]) {
    switch (instr->getOpcode()) {
    case Instruction::FPTrunc:
        params[0] = "altfp";
        params[1] = "truncate";
        break;
    case Instruction::FPExt:
        params[0] = "altfp";
        params[1] = "extend";
        break;
    case Instruction::FPToSI:
        params[0] = "altfp";
        params[1] = "fptosi";
        break;
    case Instruction::SIToFP:
        params[0] = "altfp";
        params[1] = "sitofp";
        break;
    default:
        return false;
    }
    return true;
	}

	bool VerilogConfigInfo::populateStringsForTwoOperandInstr(Instruction *instr,
                                                    std::string params[10]) {
    if (isa<BinaryOperator>(instr)) {
        if (!populateStringsForBinaryOperator(instr, params))
            return false;

    } else if (const ICmpInst *cmp = dyn_cast<ICmpInst>(instr)) {
        populateStringsForICmpInst(cmp, params);
    } else if (const FCmpInst *cmp = dyn_cast<FCmpInst>(instr)) {
        populateStringsForFCmpInst(cmp, params);
    } else {
        // errs() << "Unrecognized instruction: " << *instr << "\n";
        // assert(isa<GetElementPtrInst>(instr) || isa<PHINode>(instr) ||
        //        isa<CallInst>(instr));
    }

    return true;
	}

	bool VerilogConfigInfo::populateStringsForThreeOperandInstr(Instruction *instr,
                                                      std::string params[10]) {
    if (instr->getOpcode() == Instruction::Select) {
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "eq";
        params[3] = "mux";
    } else {
        // DEBUG(errs() << "Unrecognized instruction: " << *instr << "\n");
        return false;
    }
    return true;
	}

	int VerilogConfigInfo::maxBitWidth(int width0, int width1, int width2) {
    return (width0 >= width1) ? (width0 >= width2) ? width0 : width2
                              : (width1 >= width2) ? width1 : width2;
	}

	bool VerilogConfigInfo::isSupportedBitwidth(int width) {
    return width == 8 || width == 16 || width == 32 || width == 64;
	}

	std::string VerilogConfigInfo::assembleOpNameFromStringList(std::string params[10]) {
    std::string op_name = params[0];
    for (int i = 1; params[i] != ""; i++) {
        // errs() << "PARAMS[i] is: " << params[i] << "\n";
        op_name = op_name + "_" + params[i];
    }

    return op_name;
	}

	bool VerilogConfigInfo::isAnyOfTwoOperandsZero(Instruction *instr) {
    for (int i = 0; i < 2; i++) {
        ConstantInt *ci = dyn_cast<ConstantInt>(instr->getOperand(i));
        if (ci && ci->isZero()) {
            return true;
        }
    }

    return false;
	}

	bool VerilogConfigInfo::isSecondOperandZero(Instruction *instr) {
    ConstantInt *ci = dyn_cast<ConstantInt>(instr->getOperand(1));
    return ci && ci->isZero();
	}

	bool VerilogConfigInfo::isSecondOperandPowerOfTwo(Instruction *instr) {
    ConstantInt *ci = dyn_cast<ConstantInt>(instr->getOperand(1));
    return ci && ci->getValue().isPowerOf2();
	}

	bool VerilogConfigInfo::isSecondOperandConstant(Instruction *instr) {
    return isa<ConstantInt>(instr->getOperand(1));
	}


	bool VerilogConfigInfo::isBinaryOperatorNoOp(Instruction *instr) {
    switch (instr->getOpcode()) {
    case Instruction::Add:
        // x + 0 = 0 + x = x
        if (isAnyOfTwoOperandsZero(instr))
            return true;
        break;
    case Instruction::Sub:
        // x - 0 = x
        if (isSecondOperandZero(instr))
            return true;
        break;
    case Instruction::Mul:
    case Instruction::URem:
    case Instruction::UDiv:
        if (isSecondOperandPowerOfTwo(instr))
            return true;
        break;
    case Instruction::And:
    case Instruction::Or:
    case Instruction::Shl:
    case Instruction::AShr:
    case Instruction::LShr:
        if (isSecondOperandConstant(instr)) {
            // If the second operand is a constant
            return true;
        }
        break;
    }

    return false;
	}


	bool VerilogConfigInfo::populateStringsForBinaryOperator(Instruction *instr,
                                                   std::string params[10]) {
    switch (instr->getOpcode()) {
    case Instruction::Add:
        params[0] = "signed";
        params[1] = "add";
        break;
    case Instruction::FAdd:
        params[0] = "altfp";
        params[1] = "add";
        break;
    case Instruction::FMul:
        params[0] = "altfp";
        params[1] = "multiply";
        break;
    case Instruction::FSub:
        params[0] = "altfp";
        params[1] = "subtract";
        break;
    case Instruction::FDiv:
        params[0] = "altfp";
        params[1] = "divide";
        break;
    case Instruction::Sub:
        params[0] = "signed";
        params[1] = "subtract";
        break;
    case Instruction::Mul:
        params[0] = "signed";
        params[1] = "multiply";
        break;
    case Instruction::URem:
        params[0] = "unsigned";
        params[1] = "modulus";
        break;
    case Instruction::UDiv:
        params[0] = "unsigned";
        params[1] = "divide";
        break;
    case Instruction::And:
        params[0] = "bitwise";
        params[1] = "AND";
        break;
    case Instruction::Or:
        params[0] = "bitwise";
        params[1] = "OR";
        break;
    case Instruction::Xor:
        params[0] = "bitwise";
        params[1] = "XOR";
        break;
    case Instruction::Shl:
        params[0] = "shift";
        params[1] = "ll";
        break;
    case Instruction::AShr:
        params[0] = "shift";
        params[1] = "ra";
        break;
    case Instruction::LShr:
        params[0] = "shift";
        params[1] = "rl";
        break;
    case Instruction::SRem:
        params[0] = "signed";
        params[1] = "modulus";
        break;
    case Instruction::SDiv:
        params[0] = "signed";
        params[1] = "divide";
        break;
    default:
        errs() << "Invalid operator type!\n";
    }

    return !isBinaryOperatorNoOp(instr);
	}


	void VerilogConfigInfo::populateStringsForICmpInst(const ICmpInst *cmp,
                                             std::string params[10]) {
    switch (cmp->getPredicate()) {
    case ICmpInst::ICMP_EQ:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "eq";
        break;
    case ICmpInst::ICMP_NE:
        // TODO: Add "not equal" operations to script and change
        // this accordingly
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "eq";
        break;
    case ICmpInst::ICMP_SLT:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "lt";
        break;
    case ICmpInst::ICMP_ULT:
        params[0] = "unsigned";
        params[1] = "comp";
        params[2] = "lt";
        break;
    case ICmpInst::ICMP_SLE:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "lte";
        break;
    case ICmpInst::ICMP_ULE:
        params[0] = "unsigned";
        params[1] = "comp";
        params[2] = "lte";
        break;
    case ICmpInst::ICMP_SGT:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "gt";
        break;
    case ICmpInst::ICMP_UGT:
        params[0] = "unsigned";
        params[1] = "comp";
        params[2] = "gt";
        break;
    case ICmpInst::ICMP_SGE:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "gte";
        break;
    case ICmpInst::ICMP_UGE:
        params[0] = "unsigned";
        params[1] = "comp";
        params[2] = "gte";
        break;
    default:
        errs() << "Illegal ICmp predicate.\n";
    }
	}

	void VerilogConfigInfo::populateStringsForFCmpInst(const FCmpInst *cmp,
                                             std::string params[10]) {
    switch (cmp->getPredicate()) {
    case FCmpInst::FCMP_OEQ:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "oeq";
        break;
    case FCmpInst::FCMP_UEQ:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "ueq";
        break;
    case FCmpInst::FCMP_ONE:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "one";
        break;
    case FCmpInst::FCMP_UNE:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "une";
        break;
    case FCmpInst::FCMP_OLT:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "olt";
        break;
    case FCmpInst::FCMP_ULT:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "ult";
        break;
    case FCmpInst::FCMP_OLE:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "ole";
        break;
    case FCmpInst::FCMP_ULE:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "ule";
        break;
    case FCmpInst::FCMP_OGT:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "ogt";
        break;
    case FCmpInst::FCMP_UGT:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "ugt";
        break;
    case FCmpInst::FCMP_OGE:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "oge";
        break;
    case FCmpInst::FCMP_UGE:
        params[0] = "signed";
        params[1] = "comp";
        params[2] = "uge";
        break;
    default:
        errs() << "Illegal FCmp predicate.\n";
    }
	}

	int set_operation_attributes (ClientData clientData, Tcl_Interp *tclInterp,
																int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		float delay = atof(argv[6]);
		int latency = atoi(argv[20]);

		verilogConfigInfo->addOperationInfo(argv[2], delay, latency);
		
		return TCL_OK;
	}

	int set_clock_period (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		float period = atof(argv[1]);

		verilogConfigInfo->setClockPeriod(period);

		return TCL_OK;
	}

	int set_bluetooth_use (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int bluetooth_use = atof(argv[1]);

		if ( bluetooth_use == 1 )
			verilogConfigInfo->setBluetooth(true);
		else
			verilogConfigInfo->setBluetooth(false);

		return TCL_OK;
	}

	int set_wifi_use (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int wifi_use = atof(argv[1]);

		if ( wifi_use == 1 )
			verilogConfigInfo->setWifi(true);
		else
			verilogConfigInfo->setWifi(false);

		return TCL_OK;
	}

	int set_buffer_size (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int buffer_size = atof(argv[1]);

		if ( buffer_size > 0 )
			verilogConfigInfo->setBufferSize(buffer_size);
		else
			verilogConfigInfo->setBufferSize((unsigned)64);

		return TCL_OK;
	}

	int set_buffer_size_bit (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int buffer_size_bit = atof(argv[1]);

		if ( buffer_size_bit > 0 )
			verilogConfigInfo->setBufferSizeBit(buffer_size_bit);
		else
			verilogConfigInfo->setBufferSizeBit((unsigned)6);

		return TCL_OK;
	}

	int set_thread_use (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int thread_use = atof(argv[1]);

		if ( thread_use == 1 )
			verilogConfigInfo->setThread(true);
		else
			verilogConfigInfo->setThread(false);

		return TCL_OK;
	}

	int set_all_lut (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int all_lut = atof(argv[1]);

		if ( all_lut == 1 )
			verilogConfigInfo->setAllLUT(true);
		else
			verilogConfigInfo->setAllLUT(false);

		return TCL_OK;
	}

	int set_private_buffer (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int private_buffer = atof(argv[1]);

		if ( private_buffer == 1 )
			verilogConfigInfo->setPrivateBuffer(true);
		else
			verilogConfigInfo->setPrivateBuffer(false);

		return TCL_OK;
	}

	int set_private_reg (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int private_reg = atof(argv[1]);

		if ( private_reg == 1 )
			verilogConfigInfo->setPrivateReg(true);
		else
			verilogConfigInfo->setPrivateReg(false);

		return TCL_OK;
	}

	int set_validation_mode (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int validation = atof(argv[1]);

		if ( validation == 1 )
			verilogConfigInfo->setValidationMode(true);
		else
			verilogConfigInfo->setValidationMode(false);

		return TCL_OK;
	}

	int set_no_mc (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int no_mc = atof(argv[1]);

		if ( no_mc == 1 )
			verilogConfigInfo->setNoMC(true);
		else
			verilogConfigInfo->setNoMC(false);

		return TCL_OK;
	}

	int set_no_mf (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int no_mf = atof(argv[1]);

		if ( no_mf == 1 )
			verilogConfigInfo->setNoMF(true);
		else
			verilogConfigInfo->setNoMF(false);

		return TCL_OK;
	}

	int set_centralized_memory (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int cen_mem = atof(argv[1]);

		if ( cen_mem == 1 )
			verilogConfigInfo->setCentralizedMemory(true);
		else
			verilogConfigInfo->setCentralizedMemory(false);

		return TCL_OK;
	}



	int set_full_chaining (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int full_chaining = atof(argv[1]);

		if ( full_chaining == 1 )
			verilogConfigInfo->setFullChaining(true);
		else
			verilogConfigInfo->setFullChaining(false);

		return TCL_OK;
	}

	int set_ram_init (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int ram_init = atof(argv[1]);

		if ( ram_init == 1 )
			verilogConfigInfo->setRAMInit(true);
		else
			verilogConfigInfo->setRAMInit(false);

		return TCL_OK;
	}

	int set_pipeline (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int pipeline = atof(argv[1]);

		if ( pipeline == 1 )
			verilogConfigInfo->setPipeline(true);
		else
			verilogConfigInfo->setPipeline(false);

		return TCL_OK;
	}

	int set_accel_function (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int accel = atof(argv[1]);

		if ( accel == 1 ) {
			verilogConfigInfo->setAccel(true);
			std::string accelName(argv[2]);
			verilogConfigInfo->setAccelFunctionName(accelName);
		}
		else {
			verilogConfigInfo->setAccel(false);
		}

		return TCL_OK;
	}

	int set_valid_memory (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int vm = atof(argv[1]);

		if ( vm == 1 )
			verilogConfigInfo->setValidMemory(true);
		else
			verilogConfigInfo->setValidMemory(false);

		return TCL_OK;
	}

	int set_fadd (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int fadd = atoi(argv[1]);
		int dadd = atoi(argv[2]);

		verilogConfigInfo->setFAdd(fadd, dadd);

		return TCL_OK;
	}

	int set_fsub(ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int fsub = atoi(argv[1]);
		int dsub = atoi(argv[2]);

		verilogConfigInfo->setFSub(fsub, dsub);

		return TCL_OK;
	}

	int set_fmul(ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int fmul = atoi(argv[1]);
		int dmul = atoi(argv[2]);

		verilogConfigInfo->setFMul(fmul, dmul);

		return TCL_OK;
	}

	int set_fdiv(ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int fdiv = atoi(argv[1]);
		int ddiv = atoi(argv[2]);

		verilogConfigInfo->setFDiv(fdiv, ddiv);

		return TCL_OK;
	}

	int set_frem(ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int frem = atoi(argv[1]);
		int drem = atoi(argv[2]);

		verilogConfigInfo->setFRem(frem, drem);

		return TCL_OK;
	}

	int set_call_bus (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int cb = atof(argv[1]);

		if ( cb == 1 )
			verilogConfigInfo->setCallBus(true);
		else
			verilogConfigInfo->setCallBus(false);

		return TCL_OK;
	}

	int set_flat_operator (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int fo = atof(argv[1]);

		if ( fo == 1 )
			verilogConfigInfo->setFlatOperator(true);
		else
			verilogConfigInfo->setFlatOperator(false);

		return TCL_OK;
	}


	int set_memory_bit_width (ClientData clientData, Tcl_Interp *tclInterp,
											int argc, const char *argv[])
	{
		VerilogConfigInfo *verilogConfigInfo = (VerilogConfigInfo *)clientData;
		if (!verilogConfigInfo)
			return TCL_ERROR;

		int bw = atof(argv[1]);

		verilogConfigInfo->setMemoryBitWidth(bw);

		return TCL_OK;
	}

	Tcl_Interp *VerilogConfigBuilder::setTclCommandToMyFunction()
	{
		Tcl_Interp *tclInterp = Tcl_CreateInterp();
		assert(tclInterp);

		Tcl_CreateCommand(tclInterp, "set_operation_attributes",
											set_operation_attributes, verilogConfigInfo, 0);

		Tcl_CreateCommand(tclInterp, "set_clock_period",
											set_clock_period, verilogConfigInfo, 0);

		Tcl_CreateCommand(tclInterp, "set_bluetooth_use",
											set_bluetooth_use, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_wifi_use",
											set_wifi_use, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_buffer_size",
											set_buffer_size, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_buffer_size_bit",
											set_buffer_size_bit, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_thread_use",
											set_thread_use, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_all_lut",
											set_all_lut, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_private_buffer",
											set_private_buffer, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_private_reg",
											set_private_reg, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_full_chaining",
											set_full_chaining, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_ram_init",
											set_ram_init, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_pipeline",
											set_pipeline, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_accel_function",
											set_accel_function, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_valid_memory",
											set_valid_memory, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_validation_mode",
											set_validation_mode, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_no_mc",
											set_no_mc, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_no_mf",
											set_no_mf, verilogConfigInfo, 0);

		Tcl_CreateCommand(tclInterp, "set_centralized_memory",
											set_centralized_memory, verilogConfigInfo, 0);



		Tcl_CreateCommand(tclInterp, "set_fadd",
											set_fadd, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_fsub",
											set_fsub, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_fmul",
											set_fmul, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_fdiv",
											set_fdiv, verilogConfigInfo, 0);
		Tcl_CreateCommand(tclInterp, "set_frem",
											set_frem, verilogConfigInfo, 0);


		Tcl_CreateCommand(tclInterp, "set_call_bus",
											set_call_bus, verilogConfigInfo, 0);

		Tcl_CreateCommand(tclInterp, "set_flat_operator",
											set_flat_operator, verilogConfigInfo, 0);

		Tcl_CreateCommand(tclInterp, "set_memory_bit_width",
											set_memory_bit_width, verilogConfigInfo, 0);

		return tclInterp;
	}

	VerilogConfigInfo *VerilogConfigBuilder::getVerilogConfigInfo(void)
	{
		verilogConfigInfo = new VerilogConfigInfo();
		Tcl_Interp *tclInterp;
		
		tclInterp = setTclCommandToMyFunction();

		int result = Tcl_EvalFile(tclInterp, configFileName.c_str());


		if ( result == TCL_OK )
		{
			Tcl_DeleteInterp(tclInterp);
			return verilogConfigInfo;
		}
		else
		{
#ifdef DEBUG_VERILOG_CONFIG
			errs() << tclInterp->errorLine << " : error result : " <<Tcl_GetStringResult(tclInterp) <<"\n";
#endif
			Tcl_DeleteInterp(tclInterp);
			return NULL;
		}
	}

} //end namespace
