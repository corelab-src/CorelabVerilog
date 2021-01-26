add_llvm_target(CorelabVerilog
  CorelabVerilogTargetMachine.cpp
	CorelabVerilogPass.cpp
	BBCDFG.cpp
	NLT.cpp
	VerilogConfigBuilder.cpp
	GetSize.cpp
	GetMemOper.cpp
	GetDataLayout.cpp
	LoopPatternAnal.cpp
	FSM.cpp
	SDCScheduling.cpp
	PointerAnalysis.cpp
	PABuilder.cpp
	MemoryTable.cpp
	CallTable.cpp
	OperatorTable.cpp
	PrintVerilog.cpp
	PrintVerilogMemory.cpp
	PABasedAA.cpp
	BitWidthTable.cpp
)

target_link_libraries(LLVMCorelabVerilog lpsolve55)
target_link_libraries(LLVMCorelabVerilog tcl8.5)

add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
