#include "CorelabVerilogMCTargetDesc.h"
#include "CorelabVerilogMCAsmInfo.h"

#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/TargetRegistry.h"

#include <iostream>

using namespace llvm;

static MCAsmInfo* createCorelabVerilogMCAsmInfo(const MCRegisterInfo &MRI,
                                                const Triple &TT) {
  return new CorelabVerilogMCAsmInfo(TT);
}

extern "C" void LLVMInitializeCorelabVerilogTargetMC() {
//  std::cout << "I'm LLVMInitializeCorelabVerilogTargetMC()" << std::endl; 

  // This will register asm info
  RegisterMCAsmInfoFn X(
    getTheCorelabVerilogTarget(), createCorelabVerilogMCAsmInfo);
}
