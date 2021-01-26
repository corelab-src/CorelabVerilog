#ifndef CORELAB_VERILOG_MC_ASM_INFO_H
#define CORELAB_VERILOG_MC_ASM_INFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {

class Triple;

class CorelabVerilogMCAsmInfo : public MCAsmInfoELF {
  void anchor() override;

 public:
   explicit CorelabVerilogMCAsmInfo(const Triple &TargetTriple);
};

}
#endif

