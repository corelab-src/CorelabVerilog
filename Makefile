LEVEL = ../..
LIBRARYNAME = LLVMCorelabVerilog
TARGET = CorelabVerilog
SHARED_LIBRARY = 1
BUILD_ARCHIVE = 1

USEDLIBS = LLVMMC LLVMSupport LLVMTarget LLVMAsmPrinter LLVMCodeGen LLVMTarget

LIBS="${LIBS} -llpsolve55 -ltcl8.5"

#DIRS = TargetInfo

include $(LEVEL)/Makefile.common

CompileCommonOpts += -Wno-format 
