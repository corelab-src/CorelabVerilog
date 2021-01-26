#include "CorelabVerilog.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetRegistry.h"
#include <iostream>
using namespace llvm;

Target &llvm::getTheCorelabVerilogTarget() {
//  std::cout << "I am getTheCorelabVerilogTarget()" << std::endl;
  static Target TheCorelabVerilogTarget;
  return TheCorelabVerilogTarget;
}

extern "C" void LLVMInitializeCorelabVerilogTargetInfo() { 
//  std::cout << "I am LLVMInitializeCorelabVerilogTargetInfo()" << std::endl;
  RegisterTarget<Triple::corelabv, false> X(
    getTheCorelabVerilogTarget(), "corelabv", "Corelab Verilog Backend",
		"Corelab Verilog Backend");
}
