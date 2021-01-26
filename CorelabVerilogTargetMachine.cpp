#include "llvm/Support/FormattedStream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/CodeGen/Passes.h"
#include "MCTargetDesc/CorelabVerilogMCTargetDesc.h"
//#include "CorelabVerilogTargetObjectFile.h"

#include "CorelabVerilogPass.h"
#include "CorelabVerilogTargetMachine.h"
#include "CorelabVerilog.h"

#include <fstream>

//using namespace corelab;
using namespace std; // Register the Verilog target with LLVM

extern "C" void LLVMInitializeCorelabVerilogTarget() { 
//  cout << "I'm LLVMInitializeCorelabVerilogTarget()" << endl;
  RegisterTargetMachine<CorelabVerilogTargetMachine> X(
    getTheCorelabVerilogTarget());

  PassRegistry *PR = PassRegistry::getPassRegistry();
//  cout << "before calling initializeAAResultsWrapperPassPass()" << endl;
	initializeAAResultsWrapperPassPass(*PR);
//  cout << "after calling initializeAAResultsWrapperPassPass()" << endl;

//  cout << "before calling initializeAAResultsWrapperPassPass()" << endl;
	initializeAAResultsWrapperPassPass(*PR);
//  cout << "after calling initializeAAResultsWrapperPassPass()" << endl;
//  cout << "before calling initializeLoopInfoWrapperPassPass()" << endl;
	initializeLoopInfoWrapperPassPass(*PR);
//  cout << "after calling initializeLoopInfoWrapperPassPass()" << endl;
//  cout << "before calling initializeScalarEvolutionWrapperPassPass()" << endl;
	initializeScalarEvolutionWrapperPassPass(*PR);
//  cout << "after calling initializeScalarEvolutionWrapperPassPass()" << endl;
}


CorelabVerilogTargetMachine::CorelabVerilogTargetMachine(
    const Target &T, const Triple &TT,
    StringRef CPU, StringRef FS,
    const TargetOptions &Options,
    Optional<Reloc::Model> RM,
    Optional<CodeModel::Model> CM,
    CodeGenOpt::Level OL, bool is64bit)
  : LLVMTargetMachine(T, "e-i64:64-i128:128-v16:16-v32:32-n16:32:64", TT, CPU, 
                      FS, Options, Reloc::Static, CodeModel::Medium, OL) {
//  std::cout << "I'm CorelabVerilogTargetMahine Constructor" << std::endl;
  initAsmInfo();
}

namespace {

// Do we need CorelabVerilogPassConfig??
class CorelabVerilogPassConfig : public TargetPassConfig {
 public:
   CorelabVerilogPassConfig(CorelabVerilogTargetMachine &TM, 
                            PassManagerBase &PM) 
     : TargetPassConfig(TM, PM) {}
};

} // end namespace

TargetPassConfig* CorelabVerilogTargetMachine::createPassConfig(
    PassManagerBase &PM) {
//  cout << "I'm createPassConfig()" << endl;
  return new CorelabVerilogPassConfig(*this, PM);
}

char corelab::CorelabVerilogPass::ID = 0;

bool CorelabVerilogTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                                      raw_pwrite_stream &Out,
                                                      raw_pwrite_stream *DwoOut,
                                                      CodeGenFileType FileType,
                                                      bool DisableVerify,
                                                      MachineModuleInfo *MMI) {
//  cout << "I'm addPassesToEmitFile()" << endl;
  // add passes here
  PM.add(new corelab::CorelabVerilogPass(Out));
  return false;
}
