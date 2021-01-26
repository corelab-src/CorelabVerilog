#ifndef CORELAB_VERILOG_TARGETMACHINE_H
#define CORELAB_VERILOG_TARGETMACHINE_H

#include "llvm/Target/TargetMachine.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
//#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/Support/FormattedStream.h"

namespace llvm {

//extern "C" int CorelabVerilogTargetMachineModule;
//int CorelabVerilogTargetMachineModule = 0;

class CorelabVerilogTargetMachine : public LLVMTargetMachine {

 public:
   CorelabVerilogTargetMachine(const Target &T, const Triple &TT,
                               StringRef CPU, StringRef FS,
                               const TargetOptions &Options,
                               Optional<Reloc::Model> RM,
                               Optional<CodeModel::Model> CM,
                               CodeGenOpt::Level OL, bool is64bit);
 
   TargetPassConfig* createPassConfig(PassManagerBase &PM) override;
 
   // This method generate output file
   virtual bool addPassesToEmitFile(PassManagerBase &PM,
 		  		                          raw_pwrite_stream &Out,
                                    raw_pwrite_stream *DwoOut,
                                    CodeGenFileType FileType,
                                    bool DisableVerify,
                                    MachineModuleInfo *MMI);
 
};

} // end llvm namespace
#endif
