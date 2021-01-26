#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/InstrTypes.h"

#include "PrintVerilog.h"

#include <iostream>
#include <fstream>

using namespace llvm;
using namespace std;

namespace corelab {

	static unsigned getRequiredBits ( unsigned n ) {
		assert ( n != 0 && " num of Something is zero ( state or function ) " );
		unsigned bits = 0;
		n = n - 1;
		if ( n == 0 )
			return 1;

		for ( ; n != 0; bits++)
			n = n >> 1;
		return bits;
	}


	//Define each memory module
	void PrintVerilog::memoryModuleGeneration(void) {
		std::error_code ec_print = std::make_error_code(std::errc::io_error);
		raw_fd_ostream out("memoryModule.v", ec_print, llvm::sys::fs::F_None);
		if ( memoryTable->isByteAddressedRAM() ) {
			for ( auto ram : targetRAMSet )
			{
				bool isDual = memoryTable->isDualPortRAM(ram);

				out << "module ram_dual_port_RAM" << ram->getRAMId() << "\n(\nclk\n";
				out << ",address_a\n,select_en_a\n,write_en_a\n,data_in_a\n,data_out_a\n";
				if ( isDual )
					out << ",address_b\n,select_en_b\n,write_en_b\n,data_in_b\n,data_out_b\n";
				
				if ( verilogConfigInfo->getValidationMode() )
					out << ",RAM" << ram->getRAMId() << "_print\n";
				out << ");\n\n";

				if ( !( verilogConfigInfo->getAccel() || isUsedInMemFunc(ram) ) &&
						((ram->getPossibleBitWidth()).size() == 1) ) {
					out << "input clk;\n";
					out << "input ["<<getRequiredBits(ram->getNumOfElements())-1<<":0] address_a;\n";
					out << "input select_en_a;\n";
					out << "input write_en_a;\n";
					out << "input [" << ram->getDataBitSize()-1 << ":0] data_in_a;\n";
					out << "output reg [" << ram->getDataBitSize()-1 << ":0] data_out_a;\n";
					if ( isDual ) {
						out << "input ["<<getRequiredBits(ram->getNumOfElements())-1<<":0] address_b;\n";
						out << "input select_en_b;\n";
						out << "input write_en_b;\n";
						out << "input [" << ram->getDataBitSize()-1 << ":0] data_in_b;\n";
						out << "output reg [" << ram->getDataBitSize()-1 << ":0] data_out_b;\n\n";
					}
				}
				else {
					out << "input clk;\n";
					out << "input [" << getRequiredBits(ram->getNumOfBlock())-1 << ":0] address_a;\n";
					out << "input select_en_a;\n";
					out << "input write_en_a;\n";
					out << "input [" << ram->getMemBitWidth()-1 << ":0] data_in_a;\n";
					out << "output reg [" << ram->getMemBitWidth()-1 << ":0] data_out_a;\n";
					if ( isDual ) {
						out << "input [" << getRequiredBits(ram->getNumOfBlock())-1 << ":0] address_b;\n";
						out << "input select_en_b;\n";
						out << "input write_en_b;\n";
						out << "input [" << ram->getMemBitWidth()-1 << ":0] data_in_b;\n";
						out << "output reg [" << ram->getMemBitWidth()-1 << ":0] data_out_b;\n\n";
					}
				}

				if ( verilogConfigInfo->getValidationMode() )
					out << "input RAM" << ram->getRAMId() << "_print;\n\n";

				if ( ram->getTotalBitSize() <= 1024 && verilogConfigInfo->getValidMemory() &&
						ram->isReadOnly() ) {
					out << "(* rom_style = \"distributed\" *) reg [";
				}
				else if ( (ram->getTotalBitSize() <= 1024 && verilogConfigInfo->getValidMemory()) ||
						ram->getNumOfBlock() == 1 ) {
					out << "(* ram_style = \"registers\" *) reg [";
				}
				else if ( verilogConfigInfo->getAllLUT() ) {
					out << "(* ram_style = \"distributed\" *) reg [";
				}
				else if ( ram->isReadOnly() && verilogConfigInfo->getValidMemory() ) {
					out << "(* rom_style = \"block\" *) reg [";
				}
				else if ( ram->getTotalBitSize() <= 1024 ) {
					out << "(* ram_style = \"distributed\" *) reg [";
				}
				else {
					out << "(* ram_style = \"block\" *) reg [";
				}

				if ( !( verilogConfigInfo->getAccel() || isUsedInMemFunc(ram) ) &&
						((ram->getPossibleBitWidth()).size() == 1) ) {
					out << ram->getDataBitSize()-1 << ":0] ram[";
					out << ram->getNumOfElements() << ":0];\n\n";

					if ( ram->getInitId() == 0 ) {
						out << "integer i;\n";
						out << "initial begin\n";
						out << "\tfor (i=0; i < " << ram->getNumOfElements() << "; i=i+1)\n";
						out << "\t\tram[i]=0;\n";
						out << "end\n\n";
					}
					else {
						out << "initial begin\n";
						out << "\t$readmemh(\"" << ram->getInitId() <<".mem\", ram);\n";
						out << "end\n\n";
					}

					if ( verilogConfigInfo->getValidationMode() ) {
						out << "integer k;\n";
						out << "always @(posedge clk) begin\n";
						out << "\tif ( RAM" << ram->getRAMId() << "_print ) begin\n";
						out << "\t\t$display(\"" << ram->getValue()->getName() << "\");\n";
						out << "\t\tfor (k=0; k <" << ram->getNumOfElements() << "; k=k+1)\n";
						out << "\t\t\t$display(\"%h\", ram[k]);\n";
						out << "\t\t$display(\"\");\n";
						out << "\tend\nend\n\n";
					}
				}
				else {
					out << ram->getMemBitWidth()-1 << ":0] ram[";
					out << ram->getNumOfBlock()-1 << ":0];\n\n";

					if ( ram->getInitId() == 0 ) {
						out << "integer i;\n";
						out << "initial begin\n";
						out << "\tfor (i=0; i < " << ram->getNumOfBlock() << "; i=i+1)\n";
						out << "\t\tram[i]=0;\n";
						out << "end\n\n";
					}
					else {
						out << "initial begin\n";
						out << "\t$readmemh(\"" << ram->getInitId() <<".mem\", ram);\n";
						out << "end\n\n";
					}

					if ( verilogConfigInfo->getValidationMode() ) {
						out << "integer k;\n";
						out << "always @(posedge clk) begin\n";
						out << "\tif ( RAM" << ram->getRAMId() << "_print ) begin\n";
						out << "\t\t$display(\"" << ram->getValue()->getName() << "\");\n";
						out << "\t\tfor (k=0; k <" << ram->getNumOfBlock() << "; k=k+1)\n";
						out << "\t\t\t$display(\"%h\", ram[k]);\n";
						out << "\t\t$display(\"\");\n";
						out << "\tend\nend\n\n";
					}
				}

				if ( (ram->getTotalBitSize() <= 1024 && verilogConfigInfo->getValidMemory()) ||
						ram->getNumOfBlock() == 1 )
					printRegisterRAMBody(&out, isDual);
				else if ( verilogConfigInfo->getAllLUT() )
					printLUTRAMBody(&out, isDual);
				else 
					printBlockRAMBody(&out, isDual);
				
				out << "endmodule\n\n";

				out << "module memory_controller_RAM" << ram->getRAMId() << "\n(\n";
				out << "input clk,\n";
				out << "input reset,\n";

				out << "input [" << ram->getElementBitSize()-1 << ":0] address_a,\n";
				out << "input select_en_a,\n";
				if ( !(ram->isReadOnly() && verilogConfigInfo->getValidMemory()) ) {
					out << "input write_en_a,\n";
					out << "input [" << ram->getDataBitSize()-1 << ":0] data_in_a,\n";
				}
				if ( !( verilogConfigInfo->getAccel() || isUsedInMemFunc(ram) ) &&
						((ram->getPossibleBitWidth()).size() == 1) ) {
					out << "output [" << ram->getDataBitSize()-1 << ":0] data_out_a,\n";
				}
				else {
					out << "output reg [" << ram->getDataBitSize()-1 << ":0] data_out_a,\n";
				}
				out << "input [3:0] size_a,\n";
				out << "output valid_a,\n";

//				if ( isDual ) {
					out << "input [" << ram->getElementBitSize()-1 << ":0] address_b,\n";
					out << "input select_en_b,\n";
					if ( !(ram->isReadOnly() && verilogConfigInfo->getValidMemory()) ) {
						out << "input write_en_b,\n";
						out << "input [" << ram->getDataBitSize()-1 << ":0] data_in_b,\n";
					}
					if ( !( verilogConfigInfo->getAccel() || isUsedInMemFunc(ram) ) &&
							((ram->getPossibleBitWidth()).size() == 1) ) {
						out << "output [" << ram->getDataBitSize()-1 << ":0] data_out_b,\n";
					}
					else {
						out << "output reg [" << ram->getDataBitSize()-1 << ":0] data_out_b,\n";
					}
					out << "input [3:0] size_b,\n";
					out << "output valid_b";
//				}

				if ( verilogConfigInfo->getAccel() || isUsedInMemFunc(ram) ) {
					out << ",\n";
					out << "input [" << ram->getElementBitSize()-1 << ":0] address_a_ex,\n";
					out << "input select_en_a_ex,\n";
					out << "input write_en_a_ex,\n";
					out << "input [" << memoryTable->getMemBitWidth()-1 << ":0] data_in_a_ex,\n";
					out << "output [" << memoryTable->getMemBitWidth()-1 << ":0] data_out_a_ex,\n";

//					if ( isDual ) {
						out << "input [" << ram->getElementBitSize()-1 << ":0] address_b_ex,\n";
						out << "input select_en_b_ex,\n";
						out << "input write_en_b_ex,\n";
						out << "input [" << memoryTable->getMemBitWidth()-1 << ":0] data_in_b_ex,\n";
						out << "output [" << memoryTable->getMemBitWidth()-1 << ":0] data_out_b_ex\n";
//					}
				}

				if ( verilogConfigInfo->getValidationMode() )
					out << ",\ninput RAM" << ram->getRAMId() << "_print";

				out << "\n);\n\n";

				////////
				if ( !( verilogConfigInfo->getAccel() || isUsedInMemFunc(ram) ) &&
						((ram->getPossibleBitWidth()).size() == 1) ) {
					unsigned shifted;
					if ( ram->getDataBitSize() == 8 )	shifted = 0;
					else if ( ram->getDataBitSize() == 16 )	shifted = 1;
					else if ( ram->getDataBitSize() == 32 )	shifted = 2;
					else if ( ram->getDataBitSize() == 64 )	shifted = 3;

					out << "assign valid_a = 1;\n";
					if ( isDual )
						out << "assign valid_b = 1;\n";
					else {
						out << "assign valid_b = 1;\n";
						out << "assign data_out_b = 0;\n";
					}

					if ( ram->isReadOnly() && verilogConfigInfo->getValidMemory() ) {
						out << "reg write_en_a;\n";
						out << "reg [" << ram->getDataBitSize()-1 << ":0] data_in_a;\n";
						if ( isDual ) {
							out << "reg write_en_b;\n";
							out << "reg [" << ram->getDataBitSize()-1 << ":0] data_in_b;\n";
						}
						out << "always @(posedge clk) begin\n";
						out << "\tif (reset) begin\n";
						out << "\t\twrite_en_a <= 0;\n";
						out << "\t\tdata_in_a <= 0;\n";
						if ( isDual ) {
							out << "\t\twrite_en_b <= 0;\n";
							out << "\t\tdata_in_b <= 0;\n";
						}
						out << "\tend\nend\n\n";
					}
					
					out << "ram_dual_port_RAM" << ram->getRAMId() << " RAM" << ram->getRAMId() << " (\n";
					out << ".clk(clk),\n";
					out << ".address_a(address_a[" << ram->getElementBitSize()-1 << ":";
					out << shifted << "]),\n";
					out << ".select_en_a(select_en_a),\n";
					out << ".write_en_a(write_en_a),\n";
					out << ".data_in_a(data_in_a),\n";
					out << ".data_out_a(data_out_a)";
					if ( isDual ) {
						out << ",\n";
						out << ".address_b(address_b[" << ram->getElementBitSize()-1 << ":";
						out << shifted << "]),\n";
						out << ".select_en_b(select_en_b),\n";
						out << ".write_en_b(write_en_b),\n";
						out << ".data_in_b(data_in_b),\n";
						out << ".data_out_b(data_out_b)";
					}

					if ( verilogConfigInfo->getValidationMode() ) {
						out << ",\n.RAM" << ram->getRAMId() << "_print";
						out << "(RAM" << ram->getRAMId() << "_print)";
					}

					out << "\n);\n\n";
					out << "endmodule\n\n";

					continue;
				}

				if ( ram->isReadOnly() && verilogConfigInfo->getValidMemory() ) {
					out << "wire [" << ram->getDataBitSize()-1 << ":0] data_in_a,\n";
					out << "wire [" << ram->getDataBitSize()-1 << ":0] data_in_b,\n";
				}
					

				unsigned memBitWidth = ram->getMemBitWidth();
				unsigned dataBitWidth = ram->getDataBitSize();

				unsigned memBlockAddressSize = getRequiredBits(memBitWidth / 8);
				unsigned dataAddressSize = (dataBitWidth == 8) ? 0 : getRequiredBits(dataBitWidth / 8);
				unsigned addressSize = ram->getElementBitSize();
//				unsigned memoryAddressSize = (ram->getElementBitSize()>memBlockAddressSize) ?
//					ram->getElementBitSize() - offsetAddressSize : 0;

				//ram dual port wire
				out<<"wire ["<< getRequiredBits(ram->getNumOfBlock())-1<<":0] address_a_RAM;\n";
				out<<"wire ["<< getRequiredBits(ram->getNumOfBlock())-1<<":0] address_b_RAM;\n";
				out << "wire select_en_a_RAM;\n";
				out << "wire select_en_b_RAM;\n";
				if ( ram->isReadOnly() && verilogConfigInfo->getValidMemory() ) {
				}
				else {
					out << "wire write_en_a_RAM;\n";
					out << "wire write_en_b_RAM;\n";
					out << "wire ["<< memBitWidth-1 << ":0] data_in_a_RAM;\n";
					out << "wire ["<< memBitWidth-1 << ":0] data_in_b_RAM;\n";
				}
				out << "wire ["<< memBitWidth-1 << ":0] data_out_a_RAM;\n";
				out << "wire ["<< memBitWidth-1 << ":0] data_out_b_RAM;\n\n";

				if ( verilogConfigInfo->getAllLUT() ) {
					out << "wire [" << ram->getDataBitSize()-1 << ":0] data_out_a_RAM_t;\n";
					out << "wire [" << ram->getDataBitSize()-1 << ":0] data_out_b_RAM_t;\n";
					out << "reg en_a_reg;\n";
					out << "reg en_b_reg;\n";
					out << "always @(posedge clk) begin\n";
					out << "\ten_a_reg <= select_en_a;\n";
					out << "\ten_b_reg <= select_en_b;\n";
					out << "end\n\n";
					out << "assign data_out_a_RAM = en_a_reg";
					out << " ? data_out_a_RAM_t : 0;\n";
					out << "assign data_out_b_RAM = en_b_reg";
					out << " ? data_out_b_RAM_t : 0;\n\n";
				}

				//additional reg allocation
				if ( ram->isReadOnly() && verilogConfigInfo->getValidMemory() ) {
				}
				else {
					out << "reg state_a;\n";
					out << "reg state_b;\n";
					out << "reg state_a_reg;\n";
					out << "reg state_b_reg;\n\n";

					out << "reg [" << memBitWidth-1 << ":0] data_in_a_buff;\n";
					out << "reg [" << memBitWidth-1 << ":0] data_in_b_buff;\n";
					out << "reg [" << memBitWidth-1 << ":0] data_in_same;\n";
					out << "wire same_address;\n";
					out << "reg [" << memBitWidth-1 << ":0] data_in_a_buff_reg;\n";
					out << "reg [" << memBitWidth-1 << ":0] data_in_b_buff_reg;\n";
					out << "reg [" << memBitWidth-1 << ":0] data_in_same_reg;\n";
					out << "reg same_address_reg;\n";
					out << "wire a_gt_b;\n";
				}

				out << "reg [" << ram->getElementBitSize()-1 << ":0] address_a_reg;\n";
				out << "reg [" << ram->getElementBitSize()-1 << ":0] address_b_reg;\n";

				//Read Only
				if ( ram->isReadOnly() && verilogConfigInfo->getValidMemory() ) {
					out << "always @(posedge clk) begin\n";
					out << "\tif ( select_en_a )\n";
					out << "\t\taddress_a_reg <= address_a;\n";
					out << "\telse\n";
					out << "\t\taddress_a_reg <= 0;\n";
					out << "end\n\n";
					out << "always @(posedge clk) begin\n";
					out << "\tif ( select_en_b )\n";
					out << "\t\taddress_b_reg <= address_b;\n";
					out << "\telse\n";
					out << "\t\taddress_b_reg <= 0;\n";
					out << "end\n\n";

					//Ex exist
					if ( verilogConfigInfo->getAccel() || isUsedInMemFunc(ram) ) {
//						out << "assign write_en_a_RAM = write_en_a_ex;\n";
//						out << "assign write_en_b_RAM = write_en_b_ex;\n";
//						out << "assign data_in_a_RAM = data_in_a_ex;\n";
//						out << "assign data_in_b_RAM = data_in_b_ex;\n";

						out << "assign valid_a = select_en_a_ex ? 0 : 1;\n";
						out << "assign valid_b = select_en_b_ex ? 0 : 1;\n\n";

						//address
						out << "assign address_a_RAM = select_en_a_ex ? ";
						if ( ram->getNumOfBlock() == 1 )
							out << "0 : 0;\n";
						else {
							out << "address_a_ex[" << addressSize-1 << ":";
							out << memBlockAddressSize << "] : address_a[" << addressSize-1;
							out << ":" << memBlockAddressSize << "];\n";
						}
						out << "assign address_b_RAM = select_en_b_ex ? ";
						if ( ram->getNumOfBlock() == 1 )
							out << "0 : 0;\n";
						else {
							out << "address_b_ex[" << addressSize-1 << ":";
							out << memBlockAddressSize << "] : address_b[" << addressSize-1;
							out << ":" << memBlockAddressSize << "];\n";
						}

						//enable connection
						out << "assign select_en_a_RAM = select_en_a_ex ? ";
						out << "select_en_a_ex : select_en_a;\n";
						out << "assign select_en_b_RAM = select_en_b_ex ? ";
						out << "select_en_b_ex : select_en_b;\n";

						out << "assign data_out_a_ex = data_out_a_RAM;\n";
						out << "assign data_out_b_ex = data_out_b_RAM;\n\n";

					} //No Ex
					else {
						out << "assign valid_a = 1;\n";
						out << "assign valid_b = 1;\n\n";

						//address
						if ( ram->getNumOfBlock() == 1 ) {
							out << "assign address_a_RAM = 0;\n";
							out << "assign address_b_RAM = 0;\n";
						}
						else {
							out << "assign address_a_RAM = address_a[" << addressSize-1 << ":";
							out << memBlockAddressSize << "];\n";
							out << "assign address_b_RAM = address_b[" << addressSize-1 << ":";
							out << memBlockAddressSize << "];\n";
						}

						//enable
						out << "assign select_en_a_RAM = select_en_a;\n";
						out << "assign select_en_b_RAM = select_en_b;\n";
					}

					unsigned minI = 8;
					for ( unsigned possibleSize : ram->getPossibleBitWidth() )
						if ( minI > possibleSize / 8 )
							minI = possibleSize / 8;

					//data_out_
					out << "always @(*) begin\n";
					out << "\tcase ( address_a_reg[" << memBlockAddressSize-1 << ":0] )\n";
					unsigned maxI = 1<<memBlockAddressSize;
					for ( unsigned i = 0; i < maxI; i = i + minI )
					{
						//TODO: Big Endian
						out << "\t\t" << i << " : data_out_a = {";
						unsigned maxJ = dataBitWidth/8;
						for ( unsigned j = 0; j < maxJ; ++j )
						{
							if ( ((maxI-i)*8) - ((maxJ-j-1)*8) > memBitWidth || 
									((maxI-i)*8) < ((maxJ-j-1)*8)	+1)
								continue;
							out << "data_out_a_RAM[" << ((maxI-i)*8) - ((maxJ-j-1)*8) -1 << ":";
							out << ((maxI-i)*8) - ((maxJ-j)*8) << "]";
							if ( ((maxI-i)*8) - ((maxJ-j-1-1)*8) <= memBitWidth &&
									j+1 != maxJ )
								out << ", ";
						}
						out << "};\n";
					}
					out << "\tdefault : data_out_a = 0;\n";
					out << "\tendcase\n";
					out << "end\n\n";

					out << "always @(*) begin\n";
					out << "\tcase ( address_b_reg[" << memBlockAddressSize-1 << ":0] )\n";
					maxI = 1<<memBlockAddressSize;
					for ( unsigned i = 0; i < maxI; i = i + minI )
					{
						//TODO: Big Endian
						out << "\t\t" << i << " : data_out_b = {";
						unsigned maxJ = dataBitWidth/8;
						for ( unsigned j = 0; j < maxJ; ++j )
						{
							if ( ((maxI-i)*8) - ((maxJ-j-1)*8) > memBitWidth ||
									((maxI-i)*8) < ((maxJ-j-1)*8) +1)
								continue;
							out << "data_out_b_RAM[" << ((maxI-i)*8) - ((maxJ-j-1)*8) -1 << ":";
							out << ((maxI-i)*8) - ((maxJ-j)*8) << "]";
							if ( ((maxI-i)*8) - ((maxJ-j-1-1)*8) <= memBitWidth &&
									j+1 != maxJ )
								out << ", ";
						}
						out << "};\n";
					}
					out << "\tdefault : data_out_b = 0;\n";
					out << "\tendcase\n";
					out << "end\n\n";

				}
				else {  //Not read only
					out << "reg [" << ram->getElementBitSize()-1 << ":0] address_a_w_reg;\n";
					out << "reg [" << ram->getElementBitSize()-1 << ":0] address_b_w_reg;\n";
					out << "reg [" << ram->getDataBitSize()-1 << ":0] data_in_a_w_reg;\n";
					out << "reg [" << ram->getDataBitSize()-1 << ":0] data_in_b_w_reg;\n";
					out << "reg [3:0] size_a_w_reg;\n";
					out << "reg [3:0] size_b_w_reg;\n";
					out << "reg [" << memBitWidth-1 << ":0] data_out_a_RAM_reg;\n";
					out << "reg [" << memBitWidth-1 << ":0] data_out_b_RAM_reg;\n";
					out << "wire [" << memBitWidth-1 << ":0] data_a_w_RAM;\n";
					out << "wire [" << memBitWidth-1 << ":0] data_b_w_RAM;\n\n";

					out << "wire valid;\n";
					out << "wire stall_case;\n";
					out << "assign stall_case = (state_a||state_b) && (write_en_a||write_en_b);\n";
					out << "assign valid = !stall_case;\n";
					out << "assign valid_a = valid;\n";
					out << "assign valid_b = valid;\n\n";

					out << "wire isRead_a;\n";
					out << "wire isRead_b;\n";
					out << "reg isRead_a_reg;\n";
					out << "reg isRead_b_reg;\n";
					out << "assign isRead_a = select_en_a && !write_en_a;\n";
					out << "assign isRead_b = select_en_b && !write_en_b;\n\n";

					out << "wire not_alias_a;\n";
					out << "wire not_alias_b;\n";
					out << "wire stop_store;\n";

					if ( ram->getNumOfBlock() == 1 ) {
						out << "assign not_alias_a = 0;\n";
						out << "assign not_alias_b = 0;\n";
					}
					else {
						out << "assign not_alias_a = (isRead_a && address_a_w_reg[" << addressSize-1;
						out << ":" << memBlockAddressSize << "] != address_a[" << addressSize-1;
						out << ":" << memBlockAddressSize << "]) || (isRead_b && address_a_w_reg[" << addressSize-1;
						out << ":" << memBlockAddressSize << "] != address_b[" << addressSize-1;
						out << ":" << memBlockAddressSize << "]);\n";
						out << "assign not_alias_b = (isRead_a && address_b_w_reg[" << addressSize-1;
						out << ":" << memBlockAddressSize << "] != address_a[" << addressSize-1;
						out << ":" << memBlockAddressSize << "]) || (isRead_b && address_b_w_reg[" << addressSize-1;
						out << ":" << memBlockAddressSize << "] != address_b[" << addressSize-1;
						out << ":" << memBlockAddressSize << "]);\n";
					}
					out << "assign stop_store = (not_alias_a && state_a) || (not_alias_b && state_b);\n";
					out << "wire can_store;\n";
					out << "assign can_store = !stop_store || stall_case;\n\n";

					out << "always @(posedge clk) begin\n";
					out << "\tif(reset)begin\n";
					out << "\t\tstate_a <= 0;\n";
					out << "\t\tstate_b <= 0;\n";
					out << "\t\taddress_a_reg <= 0;\n";
					out << "\t\taddress_b_reg <= 0;\n";
					out << "\t\taddress_a_w_reg <= 0;\n";
					out << "\t\taddress_b_w_reg <= 0;\n";
					out << "\t\tdata_in_a_w_reg <= 0;\n";
					out << "\t\tdata_in_b_w_reg <= 0;\n";
					out << "\t\tsize_a_w_reg <= 0;\n";
					out << "\t\tsize_b_w_reg <= 0;\n";
					out << "\t\tdata_out_a_RAM_reg <= 0;\n";
					out << "\t\tdata_out_b_RAM_reg <= 0;\n";
					out << "\tend\n";
					out << "\telse begin\n\tif(select_en_a)\n";
					out << "\t\taddress_a_reg <= address_a;\n";
					out << "\tif(select_en_b)\n";
					out << "\t\taddress_b_reg <= address_b;\n\n";
					out << "\tif(state_a==0) begin\n";
					out << "\t\tif(write_en_a&&state_b==0) begin\n";
					out << "\t\t\tstate_a <= 1;\n";
					out << "\t\t\taddress_a_w_reg <= address_a;\n";
					out << "\t\t\tdata_in_a_w_reg <= data_in_a;\n";
					out << "\t\t\tsize_a_w_reg <= size_a;\n";
					out << "\t\tend\n\tend\n\telse begin\n";
					out << "\t\tif(!can_store)\n";
					out << "\t\t\tstate_a <= 1;\n";
					out << "\t\telse\n";
					out << "\t\t\tstate_a <= 0;\n";
					out << "\t\tif(state_a_reg==0)\n";
					out << "\t\t\tdata_out_a_RAM_reg <= data_out_a_RAM;\n";
					out << "\tend\n\n";

					out << "\tif(state_b==0) begin\n";
					out << "\t\tif(write_en_b&&state_a==0) begin\n";
					out << "\t\t\tstate_b <= 1;\n";
					out << "\t\t\taddress_b_w_reg <= address_b;\n";
					out << "\t\t\tdata_in_b_w_reg <= data_in_b;\n";
					out << "\t\t\tsize_b_w_reg <= size_b;\n";
					out << "\t\tend\n\tend\n\telse begin\n";
					out << "\t\tif(!can_store)\n";
					out << "\t\t\tstate_b <= 1;\n";
					out << "\t\telse\n";
					out << "\t\t\tstate_b <= 0;\n";
					out << "\t\tif(state_b_reg==0)\n";
					out << "\t\t\tdata_out_b_RAM_reg <= data_out_b_RAM;\n";
					out << "\tend\n\n";

					out << "\tstate_a_reg <= state_a;\n";
					out << "\tstate_b_reg <= state_b;\n";
					out << "\tisRead_a_reg <= isRead_a;\n";
					out << "\tisRead_b_reg <= isRead_b;\n";
					out << "\tsame_address_reg <= same_address;\n";
					out << "\tdata_in_a_buff_reg <= data_in_a_buff;\n";
					out << "\tdata_in_b_buff_reg <= data_in_b_buff;\n";
					out << "\tdata_in_same_reg <= data_in_same;\n";
					out << "\tend\n";
					out << "end\n\n";

					out << "assign data_a_w_RAM = (state_a_reg) == 0 ? data_out_a_RAM : data_out_a_RAM_reg;\n";
					out << "assign data_b_w_RAM = (state_b_reg) == 0 ? data_out_b_RAM : data_out_b_RAM_reg;\n\n";

					//Ex exist
					if ( verilogConfigInfo->getAccel() || isUsedInMemFunc(ram) ) {
						//address
						if ( ram->getNumOfBlock() == 1 ) {
							out << "assign address_a_RAM = 0;\n";
							out << "assign address_b_RAM = 0;\n";
						}
						else {
							out << "assign address_a_RAM = select_en_a_ex ? address_a_ex[" << addressSize-1;
							out << ":" << memBlockAddressSize << "] : ( can_store && state_a ? address_a_w_reg[";
							out << addressSize-1 << ":" << memBlockAddressSize << "] : address_a[" << addressSize-1;
							out << ":" << memBlockAddressSize << "] );\n";
							out << "assign address_b_RAM = select_en_b_ex ? address_b_ex[" << addressSize-1;
							out << ":" << memBlockAddressSize << "] : ( can_store && state_b ? address_b_w_reg[";
							out << addressSize-1 << ":" << memBlockAddressSize << "] : address_b[" << addressSize-1;
							out << ":" << memBlockAddressSize << "] );\n";
						}

						//
						out << "assign select_en_a_RAM = select_en_a_ex ? 1 : ";
						out << "( can_store && state_a ? 1 : select_en_a );\n";
						out << "assign select_en_b_RAM = select_en_b_ex ? 1 : ";
						out << "( can_store && state_b ? 1 : select_en_b );\n";

						out << "assign write_en_a_RAM = write_en_a_ex ? 1 : ( can_store && state_a );\n";
						out << "assign write_en_b_RAM = write_en_b_ex ? 1 : ( can_store && state_b ";
						out << "&& !same_address);\n";

						out << "assign data_in_a_RAM = write_en_a_ex ? data_in_a_ex : ( can_store && state_a ? ";
						out << "(same_address ? data_in_same : data_in_a_buff ) : 0 );\n";
						out << "assign data_in_b_RAM = write_en_b_ex ? data_in_b_ex : ( can_store && state_b ? ";
						out << "(same_address ? 0 : data_in_b_buff ) : 0 );\n";

						out << "assign data_out_a_ex = data_out_a_RAM;\n";
						out << "assign data_out_b_ex = data_out_b_RAM;\n\n";
					}
					else { //no Ex
						//address
						if ( ram->getNumOfBlock() == 1 ) {
							out << "assign address_a_RAM = 0;\n";
							out << "assign address_b_RAM = 0;\n";
						}
						else {
							out << "assign address_a_RAM = can_store && state_a ? address_a_w_reg[";
							out << addressSize-1 << ":" << memBlockAddressSize << "] : address_a[" << addressSize-1;
							out << ":" << memBlockAddressSize << "];\n";
							out << "assign address_b_RAM = can_store && state_b ? address_b_w_reg[";
							out << addressSize-1 << ":" << memBlockAddressSize << "] : address_b[" << addressSize-1;
							out << ":" << memBlockAddressSize << "];\n";
						}

						//
						out << "assign select_en_a_RAM = can_store && state_a ? 1 : select_en_a;\n";
						out << "assign select_en_b_RAM = can_store && state_b ? 1 : select_en_b;\n";

						out << "assign write_en_a_RAM = can_store && state_a;\n";
						out << "assign write_en_b_RAM = can_store && state_b && !same_address;\n";

						out << "assign data_in_a_RAM = can_store && state_a ? ";
						out << "(same_address ? data_in_same : data_in_a_buff ) : 0;\n";
						out << "assign data_in_b_RAM = can_store && state_b ? ";
						out << "(same_address ? 0 : data_in_b_buff ) : 0;\n\n";
					}

					out << "wire isAliasRAWA;\n";
					out << "wire isAliasRAWB;\n";
					out << "wire isAliasRBWA;\n";
					out << "wire isAliasRBWB;\n";
					if ( ram->getNumOfBlock() == 1 ) {
						out << "assign isAliasRAWA = 1;\n";
						out << "assign isAliasRAWB = 1;\n";
						out << "assign isAliasRBWA = 1;\n";
						out << "assign isAliasRBWB = 1;\n";
					}
					else {
						out << "assign isAliasRAWA = address_a_reg[" << addressSize-1 << ":" << memBlockAddressSize;
						out << "] == address_a_w_reg[" << addressSize-1 << ":" << memBlockAddressSize << "];\n";
						out << "assign isAliasRAWB = address_a_reg[" << addressSize-1 << ":" << memBlockAddressSize;
						out << "] == address_b_w_reg[" << addressSize-1 << ":" << memBlockAddressSize << "];\n";
						out << "assign isAliasRBWA = address_b_reg[" << addressSize-1 << ":" << memBlockAddressSize;
						out << "] == address_a_w_reg[" << addressSize-1 << ":" << memBlockAddressSize << "];\n";
						out << "assign isAliasRBWB = address_b_reg[" << addressSize-1 << ":" << memBlockAddressSize;
						out << "] == address_b_w_reg[" << addressSize-1 << ":" << memBlockAddressSize << "];\n";
					}

					out << "wire [" << memBitWidth-1 << ":0] data_out_a_ref;\n";
					out << "wire [" << memBitWidth-1 << ":0] data_out_b_ref;\n";
					out << "assign data_out_a_ref = ( isRead_a_reg ) ? ((state_a_reg||state_b_reg) ? ";
					out << "( state_a_reg&&isAliasRAWA ? ( same_address_reg ? ";
					out << "data_in_same_reg : data_in_a_buff_reg ) : ( state_b_reg&&isAliasRAWB ? data_in_b_buff_reg : ";
					out << "data_out_a_RAM ) ) : data_out_a_RAM ) : 0;\n";
					out << "assign data_out_b_ref = ( isRead_b_reg ) ? ((state_a_reg||state_b_reg) ? ";
					out << "( state_a_reg&&isAliasRBWA ? ( same_address_reg ? ";
					out << "data_in_same_reg : data_in_a_buff_reg ) : ( state_b_reg&&isAliasRBWB ? data_in_b_buff_reg : ";
					out << "data_out_b_RAM ) ) : data_out_b_RAM ) : 0;\n\n";

					if ( memBitWidth > dataBitWidth ) {
						//data_in_buff
						//Port A
						out << "always @(*) begin\n";
						out << "\tcase ( address_a_w_reg[" << memBlockAddressSize-1 << ":";
						out << dataAddressSize << "] )\n";
						unsigned endPoint = 1 << (memBlockAddressSize-dataAddressSize);
						for ( unsigned i=0; i < endPoint; ++i)
						{
							out << "\t\t" << i << " : begin\n";
							out << "\t\tcase ( size_a_w_reg )\n";
							for ( unsigned possible : ram->getPossibleBitWidth() )
							{
								unsigned possible_byte = possible / 8;
								if ( possible < dataBitWidth ) { // dataBitWidth != 8 && .... 
									out << "\t\t\t4'd" << possible_byte << " : begin\n";
									out << "\t\t\tcase ( address_a_w_reg[" << dataAddressSize-1 << ":0] )\n";
									for ( unsigned k = 0; k < dataBitWidth / 8; k+=possible_byte )
									{
										out << "\t\t\t\t" << k << " : data_in_a_buff = {";
										if ( i != 0 || k != 0) {
											out << "data_a_w_RAM[" << memBitWidth-1 << ":";
											out << ((endPoint-i)*dataBitWidth) - (k*8) << "], ";
										}
										for ( unsigned j = 0; j < possible_byte; ++j )
										{
											//TODO : Big Endian
											out << "data_in_a_w_reg[" << ((j+1)*8) -1 << ":" << (j*8) << "]";
											if ( ((endPoint-i)*dataBitWidth != possible_byte*8 + (k*8) )
													|| (j+1 !=possible_byte) )
												out << ", ";
										}
										if ( ((endPoint-i)*dataBitWidth) != possible_byte*8 + (k*8) ) {
											out << "data_a_w_RAM[";
											out << ((endPoint-i)*dataBitWidth) - (possible_byte*8) - (k*8) -1;
											out << ":0]";
										}
										out << "};\n";
									}
									out << "\t\t\t\tdefault : data_in_a_buff = 0;\n";
									out << "\t\t\tendcase\n";
									out << "\t\t\tend\n";
								}
								else { // without nested case statement
									out << "\t\t\t4'd" << possible_byte << " : data_in_a_buff = {";
									if ( i != 0 ) {
										out << "data_a_w_RAM[" << memBitWidth-1 << ":";
										out << (endPoint-i)*dataBitWidth << "], ";
									}
									for ( unsigned j = 0; j < possible_byte; ++j )
									{
										//TODO : Big Endian
										out << "data_in_a_w_reg[" << (j+1)*8 -1 << ":" << j*8 << "]";
										if ( ((endPoint-i)*dataBitWidth != possible_byte*8)
												|| (j+1 !=possible_byte) )
											out << ", ";
									}
									if ( (endPoint-i)*dataBitWidth != possible_byte*8 ) {
										out << "data_a_w_RAM[";
										out << ((endPoint-i)*dataBitWidth) - (possible_byte*8) - 1;
										out << ":0]";
									}
									out << "};\n";
								}
							}
							out << "\t\t\tdefault : data_in_a_buff = 0;\n";
							out << "\t\tendcase\n";
							out << "\t\tend\n";
						}
						out << "\t\tdefault : data_in_a_buff = 0;\n";
						out << "\tendcase\n";
						out << "end\n\n";

						//Port B
						out << "always @(*) begin\n";
						out << "\tcase ( address_b_w_reg[" << memBlockAddressSize-1 << ":";
						out << dataAddressSize << "] )\n";
						//					unsigned endPoint = 1 << (memBlockAddressSize-dataAddressSize);
						for ( unsigned i=0; i < endPoint; ++i)
						{
							out << "\t\t" << i << " : begin\n";
							out << "\t\tcase ( size_b_w_reg )\n";
							for ( unsigned possible : ram->getPossibleBitWidth() )
							{
								unsigned possible_byte = possible / 8;
								if ( possible < dataBitWidth ) {
									out << "\t\t\t4'd" << possible_byte << " : begin\n";
									out << "\t\t\tcase ( address_b_w_reg[" << dataAddressSize-1 << ":0] )\n";
									for ( unsigned k = 0; k < dataBitWidth / 8; k+=possible_byte )
									{
										out << "\t\t\t\t" << k << " : data_in_b_buff = {";
										if ( i != 0 || k != 0 ) {
											out << "data_b_w_RAM[" << memBitWidth-1 << ":";
											out << ((endPoint-i)*dataBitWidth) - (k*8) << "], ";
										}
										for ( unsigned j = 0; j < possible_byte; ++j )
										{
											out << "data_in_b_w_reg[" << ((j+1)*8) -1 << ":" << (j*8) << "]";
											if ( ((endPoint-i)*dataBitWidth != possible_byte*8 + (k*8) )
													|| (j+1 !=possible_byte) )
												out << ", ";
										}
										if ( ((endPoint-i)*dataBitWidth) != (possible_byte*8) + (k*8) ) {
											out << "data_b_w_RAM[";
											out << ((endPoint-i)*dataBitWidth) - (possible_byte*8) - (k*8) -1;
											out << ":0]";
										}
										out << "};\n";
									}
									out << "\t\t\t\tdefault : data_in_b_buff = 0;\n";
									out << "\t\t\tendcase\n";
									out << "\t\t\tend\n";
								}
								else {// without nested case statement
									out << "\t\t\t4'd" << possible_byte << " : data_in_b_buff = {";
									if ( i != 0 ) {
										out << "data_b_w_RAM[" << memBitWidth-1 << ":";
										out << (endPoint-i)*dataBitWidth << "], ";
									}
									for ( unsigned j = 0; j < possible_byte; ++j )
									{
										//TODO : Big Endian
										out << "data_in_b_w_reg[" << (j+1)*8 -1 << ":" << j*8 << "]";
										if ( ((endPoint-i)*dataBitWidth != possible_byte*8)
												|| (j+1 !=possible_byte) )
											out << ", ";
									}
									if ( (endPoint-i)*dataBitWidth != possible_byte*8 ) {
										out << "data_b_w_RAM[";
										out << ((endPoint-i)*dataBitWidth) - (possible_byte*8) - 1;
										out << ":0]";
									}
									out << "};\n";
								}
							}
							out << "\t\t\tdefault : data_in_b_buff = 0;\n";
							out << "\t\tendcase\n";
							out << "\t\tend\n";
						}
						out << "\t\tdefault : data_in_b_buff = 0;\n";
						out << "\tendcase\n";
						out << "end\n\n";
					} // MemBitWidth > DataBitWidth End
					else { // MemBitWidth == DataBitWidth
						//data_in_buff
						out << "always @(*) begin\n";
						out << "\tcase ( size_a_w_reg )\n";
						for ( unsigned possible : ram->getPossibleBitWidth() )
						{
							unsigned possible_byte = possible / 8;
							if ( possible < dataBitWidth ) {
								out << "\t\t4'd" << possible_byte << " : begin\n";
								out << "\t\tcase ( address_a_w_reg[" << dataAddressSize-1 << ":0] )\n";
								for ( unsigned k = 0; k < dataBitWidth / 8; k+=possible_byte )
								{
									out << "\t\t\t" << k << " : data_in_a_buff = {";
									if ( k != 0 ) {
										out << "data_a_w_RAM[" << dataBitWidth - 1 << ":";
										out << dataBitWidth - (k*8) << "], ";
									}
									unsigned i = 0;
									for ( i = 0; i < possible_byte; ++i )
									{
										//TODO : Big Endian
										out << "data_in_a_w_reg[" << (i+1)*8 -1 << ":" << (i*8) << "]";
										if ( dataBitWidth != ((i+1)*8) + (k*8) )
											out << ", ";
									}
									if ( dataBitWidth != (i*8) + (k*8) )
										out << "data_a_w_RAM[" << dataBitWidth -(i*8) -(k*8) -1 << ":0]";
									out << "};\n";
								}
								out << "\t\t\tdefault : data_in_a_buff = 0;\n";
								out << "\t\tendcase\n";
								out << "\t\tend\n";
							}
							else {
								out << "\t\t4'd" << possible_byte << " : ";
								out << "data_in_a_buff = {";
								unsigned i = 0;
								for ( i = 0; i < possible_byte; ++i )
								{
									//TODO : Big Endian
									out << "data_in_a_w_reg[" << (i+1)*8 -1 << ":" << i*8 << "]";
									if ( (i+1)*8 != dataBitWidth )
										out << ", ";
								}
								if ( i*8 != dataBitWidth )
									out << "data_a_w_RAM[" << dataBitWidth-(i*8)-1 << ":0]";
								out << "};\n";
							}
						}
						out << "\t\tdefault : data_in_a_buff = 0;\n";
						out << "\tendcase\n";
						out << "end\n\n";

						out << "always @(*) begin\n";
						out << "\tcase ( size_b_w_reg )\n";
						for ( unsigned possible : ram->getPossibleBitWidth() )
						{
							unsigned possible_byte = possible / 8;
							if ( possible < dataBitWidth ) {
								out << "\t\t4'd" << possible_byte << " : begin\n";
								out << "\t\tcase ( address_b_w_reg[" << dataAddressSize-1 << ":0] )\n";
								for ( unsigned k = 0; k < dataBitWidth / 8; k+=possible_byte )
								{
									out << "\t\t\t" << k << " : data_in_b_buff = {";
									if ( k != 0 ) {
										out << "data_b_w_RAM[" << dataBitWidth - 1 << ":";
										out << dataBitWidth - (k*8) << "], ";
									}
									unsigned i = 0;
									for ( i = 0; i < possible_byte; ++i )
									{
										//TODO : Big Endian
										out << "data_in_b_w_reg[" << (i+1)*8 -1 << ":" << (i*8) << "]";
										if ( dataBitWidth != ((i+1)*8) + (k*8) )
											out << ", ";
									}
									if ( dataBitWidth != (i*8) + (k*8) )
										out << "data_b_w_RAM[" << dataBitWidth -(i*8) -(k*8) -1 << ":0]";
									out << "};\n";
								}
								out << "\t\t\tdefault : data_in_b_buff = 0;\n";
								out << "\t\tendcase\n";
								out << "\t\tend\n";
							}
							else {
								out << "\t\t4'd" << possible_byte << " : ";
								out << "data_in_b_buff = {";
								unsigned i = 0;
								for ( i = 0; i < possible_byte; ++i )
								{
									out << "data_in_b_w_reg[" << (i+1)*8 -1 << ":" << i*8 << "]";
									if ( (i+1)*8 != dataBitWidth )
										out << ", ";
								}
								if ( i*8 != dataBitWidth )
									out << "data_b_w_RAM[" << dataBitWidth-(i*8)-1 << ":0]";
								out << "};\n";
							}
						}
						out << "\t\tdefault : data_in_b_buff = 0;\n";
						out << "\tendcase\n";
						out << "end\n\n";

					}// MemBitWidth == DataBitWidth End

					unsigned minI = 8;
					for ( unsigned possibleSize : ram->getPossibleBitWidth() )
						if ( minI > possibleSize / 8 )
							minI = possibleSize / 8;

					//Same Address Handler
					if ( ram->getNumOfBlock() == 1 ) {
						out << "assign same_address = state_a && state_b;\n";
					}
					else {
						out << "assign same_address = (address_a_w_reg[" << addressSize-1 << ":";
						out << memBlockAddressSize << "] == address_b_w_reg[" << addressSize-1 << " :";
						out << memBlockAddressSize << "]) && state_a && state_b;\n";
					}
					out << "assign a_gt_b = address_a_w_reg > address_b_w_reg;\n\n";


					out << "always @(*) begin\n";
					out << "\tif ( same_address ) begin\n";
					out << "\t\tcase ( a_gt_b )\n";
					out << "\t\t0 : begin\n";
					out << "\t\t\tcase ( address_b_w_reg[" << memBlockAddressSize-1 << ":0] )\n";
					for ( unsigned i = minI; i < (1<<memBlockAddressSize); i = i + minI )
					{
						out << "\t\t\t" << i << " : data_in_same = {";
						out << "data_in_a_buff[" << memBitWidth -1 << ":" << memBitWidth-(i*8) << "], ";
						out << "data_in_b_buff[" << memBitWidth -(i*8) -1 << ":0]";
						out << "};\n";
					}
					out << "\t\t\t default : data_in_same = 0;\n";
					out << "\t\t\tendcase\n";
					out << "\t\tend\n";

					out << "\t\t1 : begin\n";
					out << "\t\t\tcase ( address_a_w_reg[" << memBlockAddressSize-1 << ":0] )\n";
					for ( unsigned i = minI; i < (1<<memBlockAddressSize); i = i + minI )
					{
						out << "\t\t\t" << i << " : data_in_same = {";
						out << "data_in_b_buff[" << memBitWidth -1 << ":" << memBitWidth-(i*8) << "], ";
						out << "data_in_a_buff[" << memBitWidth -(i*8) -1 << ":0]";
						out << "};\n";
					}
					out << "\t\t\t default : data_in_same = 0;\n";
					out << "\t\t\tendcase\n";
					out << "\t\tend\n";
					out << "\t\tendcase\n";
					out << "\tend\n";
//					out << "\telse\n";
//					out << "\t\tdata_in_same = 0;\n";
					out << "end\n\n";

					//data_out_
					out << "always @(*) begin\n";
					out << "if (!reset) begin\n";
					out << "\tcase ( address_a_reg[" << memBlockAddressSize-1 << ":0] )\n";
					unsigned maxI = 1<<memBlockAddressSize;
					//				for ( unsigned i = 0; i < maxI; ++i )
					for ( unsigned i = 0; i < maxI; i = i + minI )
					{
						//TODO: Big Endian
						out << "\t\t" << i << " : data_out_a = {";
						unsigned maxJ = dataBitWidth/8;
						for ( unsigned j = 0; j < maxJ; ++j )
						{
							if ( ((maxI-i)*8) - ((maxJ-j-1)*8) > memBitWidth || 
									((maxI-i)*8) < ((maxJ-j-1)*8)	+1)
								continue;
							out << "data_out_a_ref[" << ((maxI-i)*8) - ((maxJ-j-1)*8) -1 << ":";
							out << ((maxI-i)*8) - ((maxJ-j)*8) << "]";
							if ( ((maxI-i)*8) - ((maxJ-j-1-1)*8) <= memBitWidth &&
									j+1 != maxJ )
								out << ", ";
						}
						out << "};\n";
					}
					out << "\tdefault : data_out_a = 0;\n";
					out << "\tendcase\n";
					out << "end\nend\n\n";

					out << "always @(*) begin\n";
					out << "if (!reset) begin\n";
					out << "\tcase ( address_b_reg[" << memBlockAddressSize-1 << ":0] )\n";
					maxI = 1<<memBlockAddressSize;
					//				for ( unsigned i = 0; i < maxI; ++i )
					for ( unsigned i = 0; i < maxI; i = i + minI )
					{
						//TODO: Big Endian
						out << "\t\t" << i << " : data_out_b = {";
						unsigned maxJ = dataBitWidth/8;
						for ( unsigned j = 0; j < maxJ; ++j )
						{
							if ( ((maxI-i)*8) - ((maxJ-j-1)*8) > memBitWidth ||
									((maxI-i)*8) < ((maxJ-j-1)*8) +1)
								continue;
							out << "data_out_b_ref[" << ((maxI-i)*8) - ((maxJ-j-1)*8) -1 << ":";
							out << ((maxI-i)*8) - ((maxJ-j)*8) << "]";
							if ( ((maxI-i)*8) - ((maxJ-j-1-1)*8) <= memBitWidth &&
									j+1 != maxJ )
								out << ", ";
						}
						out << "};\n";
					}
					out << "\tdefault : data_out_b = 0;\n";
					out << "\tendcase\n";
					out << "end\nend\n\n";
				}

				//BRAM Instance
				if ( isDual ) {
					out << "ram_dual_port_RAM" << ram->getRAMId() << " RAM" << ram->getRAMId() << " (\n";
					out << ".clk(clk),\n";
					out << ".address_a(address_a_RAM),\n";
					out << ".address_b(address_b_RAM),\n";
					out << ".select_en_a(select_en_a_RAM),\n";
					out << ".select_en_b(select_en_b_RAM),\n";
					if ( ram->isReadOnly() && verilogConfigInfo->getValidMemory() &&
							!verilogConfigInfo->getAccel() ) {
						out << ".write_en_a(0),\n";
						out << ".write_en_b(0),\n";
						out << ".data_in_a(data_in_a),\n";
						out << ".data_in_b(data_in_b),\n";
					}
					else {
						out << ".write_en_a(write_en_a_RAM),\n";
						out << ".write_en_b(write_en_b_RAM),\n";
						out << ".data_in_a(data_in_a_RAM),\n";
						out << ".data_in_b(data_in_b_RAM),\n";
					}
					if (  verilogConfigInfo->getAllLUT() ) {
						out << ".data_out_a(data_out_a_RAM_t),\n";
						out << ".data_out_b(data_out_b_RAM_t)";
					}
					else {
						out << ".data_out_a(data_out_a_RAM),\n";
						out << ".data_out_b(data_out_b_RAM)";
					}

					if ( verilogConfigInfo->getValidationMode() ) {
						out << ",\n.RAM" << ram->getRAMId() << "_print";
						out << "(RAM" << ram->getRAMId() << "_print)";
					}
					out << "\n);\n\n";
				}
				else {
					out << "ram_dual_port_RAM" << ram->getRAMId() << " RAM" << ram->getRAMId() << " (\n";
					out << ".clk(clk),\n";
					out << ".address_a(address_a_RAM),\n";
					out << ".select_en_a(select_en_a_RAM),\n";
					if ( ram->isReadOnly() && verilogConfigInfo->getValidMemory() &&
							!verilogConfigInfo->getAccel() ) {
						out << ".write_en_a(0),\n";
						out << ".data_in_a(data_in_a),\n";
					}
					else {
						out << ".write_en_a(write_en_a_RAM),\n";
						out << ".data_in_a(data_in_a_RAM),\n";
					}
					if (  verilogConfigInfo->getAllLUT() ) {
						out << ".data_out_a(data_out_a_RAM_t)";
					}
					else {
						out << ".data_out_a(data_out_a_RAM)";
					}

					if ( verilogConfigInfo->getValidationMode() ) {
						out << ",\n.RAM" << ram->getRAMId() << "_print";
						out << "(RAM" << ram->getRAMId() << "_print)";
					}
					out << "\n);\n\n";
				}

				out << "endmodule\n\n";
			} // ram for loop
		}// mc ( byte addressable )
		else {
			for ( auto ram : targetRAMSet )
			{
				bool isDual = memoryTable->isDualPortRAM(ram);

				if ( verilogConfigInfo->getPrivateBuffer() )
					out << "(* keep_hierarchy = \"yes\" *)";

				out << "module ram_dual_port_RAM" << ram->getRAMId() << "\n(\nclk\n";
				out << ",address_a\n,select_en_a\n,write_en_a\n,data_in_a\n,data_out_a\n";
				if ( isDual )
					out << ",address_b\n,select_en_b\n,write_en_b\n,data_in_b\n,data_out_b\n";
				
				if ( verilogConfigInfo->getValidationMode() )
					out << ",RAM" << ram->getRAMId() << "_print\n";
				out << ");\n\n";

				out << "input clk;\n";
				out << "input [" << ram->getElementBitSize()-1 << ":0] address_a;\n";
				out << "input select_en_a;\n";
				out << "input write_en_a;\n";
				out << "input [" << ram->getDataBitSize()-1 << ":0] data_in_a;\n";
				out << "output reg [" << ram->getDataBitSize()-1 << ":0] data_out_a;\n";
				
				if ( isDual ) {
					out << "input [" << ram->getElementBitSize()-1 << ":0] address_b;\n";
					out << "input select_en_b;\n";
					out << "input write_en_b;\n";
					out << "input [" << ram->getDataBitSize()-1 << ":0] data_in_b;\n";
					out << "output reg [" << ram->getDataBitSize()-1 << ":0] data_out_b;\n\n";
				}

				if ( verilogConfigInfo->getValidationMode() )
					out << "input RAM" << ram->getRAMId() << "_print;\n\n";

				if ( ram->getTotalBitSize() <= 1024 && verilogConfigInfo->getValidMemory() &&
						ram->isReadOnly() ) {
					out << "(* ram_style = \"distributed\" *) reg [";
					out << ram->getDataBitSize()-1 << ":0] ram[";
					out << ram->getNumOfElements() << ":0];\n\n";
				}
				else if ( ram->getTotalBitSize() <= 1024 && verilogConfigInfo->getValidMemory() ) {
					out << "(* ram_style = \"registers\" *) reg [";
					out << ram->getDataBitSize()-1 << ":0] ram[";
					out << ram->getNumOfElements() << ":0];\n\n";
				}
				else if ( verilogConfigInfo->getAllLUT() ) {
					out << "(* ram_style = \"distributed\" *) reg [";
					out << ram->getMemBitWidth()-1 << ":0] ram[";
					out << ram->getNumOfBlock()-1 << ":0];\n\n";
				}
				else if ( ram->getTotalBitSize() <= 1024 ) {
					out << "(* ram_style = \"distributed\" *) reg [";
					out << ram->getDataBitSize()-1 << ":0] ram[";
					out << ram->getNumOfElements() << ":0];\n\n";
				}
				else {
					out << "(* ram_style = \"block\" *) reg [";
					out << ram->getDataBitSize()-1 << ":0] ram[";
					out << ram->getNumOfElements()-1 << ":0];\n\n";
				}

				if ( ram->getInitId() == 0 ) {
					out << "integer i;\n";
					out << "initial begin\n";
					out << "\tfor (i=0; i < " << ram->getNumOfElements() << "; i=i+1)\n";
					out << "\t\tram[i]=0;\n";
					out << "end\n\n";
				}
				else {
					out << "initial begin\n";
					out << "\t$readmemh(\"" << ram->getInitId() <<".mem\", ram);\n";
					out << "end\n\n";
				}

				if ( verilogConfigInfo->getValidationMode() ) {
					out << "integer k;\n";
					out << "always @(posedge clk) begin\n";
					out << "\tif ( RAM" << ram->getRAMId() << "_print ) begin\n";
					out << "\t\t$display(\"" << ram->getValue()->getName() << "\");\n";
					out << "\t\tfor (k=0; k <" << ram->getNumOfElements() << "; k=k+1)\n";
					out << "\t\t\t$display(\"%h\", ram[k]);\n";
					out << "\tend\nend\n\n";
				}

				if ( (ram->getTotalBitSize() <= 1024 && verilogConfigInfo->getValidMemory()) )
					printRegisterRAMBody(&out, isDual);
				else if ( verilogConfigInfo->getAllLUT() )
					printLUTRAMBody(&out, isDual);
				else
					printBlockRAMBody(&out, isDual);

				out << "endmodule\n\n";
			}
		}
		out.close();
	}

	//topModule Generation uses this function
	//RAM Module Instantiation & related wires/regs defition
	void PrintVerilog::printRAMInstance(RAM_ *ram, raw_fd_ostream *topFile) {
		bool isDual = memoryTable->isDualPortRAM(ram);

		if ( memoryTable->isByteAddressedRAM() ) {
			//For Functions
			*topFile << "wire [" << ram->getElementBitSize()-1 << ":0] address_a_RAM";
			*topFile << ram->getRAMId() << ";\n";
			*topFile << "wire  select_en_a_RAM" << ram->getRAMId() << ";\n";
			if ( !(ram->isReadOnly() && verilogConfigInfo->getValidMemory()) ) {
				*topFile << "wire  write_en_a_RAM" << ram->getRAMId() << ";\n";
				*topFile << "wire  [" << ram->getDataBitSize()-1 << ":0] data_in_a_RAM";
				*topFile << ram->getRAMId() << ";\n";
			}
			*topFile << "wire [" << ram->getDataBitSize()-1 << ":0] data_out_a_RAM";
			*topFile << ram->getRAMId() << ";\n";
			*topFile << "wire [3:0] size_a_RAM" << ram->getRAMId() << ";\n";
			*topFile << "wire valid_a_RAM" << ram->getRAMId() << ";\n";

			if ( isDual ) {
				*topFile << "wire [" << ram->getElementBitSize()-1 << ":0] address_b_RAM";
				*topFile << ram->getRAMId() << ";\n";
				*topFile << "wire  select_en_b_RAM" << ram->getRAMId() << ";\n";
				if ( !(ram->isReadOnly() && verilogConfigInfo->getValidMemory()) ) {
					*topFile << "wire  write_en_b_RAM" << ram->getRAMId() << ";\n";
					*topFile << "wire  [" << ram->getDataBitSize()-1 << ":0] data_in_b_RAM";
					*topFile << ram->getRAMId() << ";\n";
				}
				*topFile << "wire [" << ram->getDataBitSize()-1 << ":0] data_out_b_RAM";
				*topFile << ram->getRAMId() << ";\n";
				*topFile << "wire [3:0] size_b_RAM" << ram->getRAMId() << ";\n";
				*topFile << "wire valid_b_RAM" << ram->getRAMId() << ";\n";
			}
			else { //temporal
				*topFile << "wire [" << ram->getDataBitSize()-1 << ":0] data_out_b_RAM";
				*topFile << ram->getRAMId() << ";\n";
				*topFile << "wire valid_b_RAM" << ram->getRAMId() << ";\n";
			}

			if ( verilogConfigInfo->getValidationMode() ) 
				*topFile << "reg RAM" << ram->getRAMId() << "_print;\n\n";

			//For External Modules
			// Defined as topmodule input&output wires
			if ( verilogConfigInfo->getAccel() || isUsedInMemFunc(ram) ) {
				*topFile << "wire [" << ram->getElementBitSize()-1 << ":0] address_a_ex_RAM";
				*topFile << ram->getRAMId() << ";\n";
				*topFile << "wire  select_en_a_ex_RAM" << ram->getRAMId() << ";\n";
				*topFile << "wire  write_en_a_ex_RAM" << ram->getRAMId() << ";\n";
				*topFile << "wire  [" << memoryTable->getMemBitWidth()-1 << ":0] data_in_a_ex_RAM";
				*topFile << ram->getRAMId() << ";\n";
				*topFile << "wire [" << memoryTable->getMemBitWidth()-1 << ":0] data_out_a_ex_RAM";
				*topFile << ram->getRAMId() << ";\n";

				if ( isDual ) {
					*topFile << "wire [" << ram->getElementBitSize()-1 << ":0] address_b_ex_RAM";
					*topFile << ram->getRAMId() << ";\n";
					*topFile << "wire  select_en_b_ex_RAM" << ram->getRAMId() << ";\n";
					*topFile << "wire  write_en_b_ex_RAM" << ram->getRAMId() << ";\n";
					*topFile << "wire  [" << memoryTable->getMemBitWidth()-1 << ":0] data_in_b_ex_RAM";
					*topFile << ram->getRAMId() << ";\n";
					*topFile << "wire [" << memoryTable->getMemBitWidth()-1 << ":0] data_out_b_ex_RAM";
					*topFile << ram->getRAMId() << ";\n";
				}
				else { //temporal
					*topFile << "wire [" << memoryTable->getMemBitWidth()-1 << ":0] data_out_b_ex_RAM";
					*topFile << ram->getRAMId() << ";\n";
				}
			}


			*topFile<<"memory_controller_RAM"<<ram->getRAMId()<<" mc_RAM"<<ram->getRAMId()<<" (\n";
			*topFile << ".clk(clk),\n";
			*topFile << ".reset(reset),\n";
			*topFile << ".address_a(address_a_RAM" << ram->getRAMId() << "),\n";
			*topFile << ".select_en_a(select_en_a_RAM" << ram->getRAMId() << "),\n";
			if ( !(ram->isReadOnly() && verilogConfigInfo->getValidMemory()) ) {
				*topFile << ".write_en_a(write_en_a_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".data_in_a(data_in_a_RAM" << ram->getRAMId() << "),\n";
			}
			*topFile << ".data_out_a(data_out_a_RAM" << ram->getRAMId() << "),\n";
			*topFile << ".size_a(size_a_RAM" << ram->getRAMId() << "),\n";
			*topFile << ".valid_a(valid_a_RAM" << ram->getRAMId() << ")";

			if ( isDual ) {
				*topFile << ",\n";
				*topFile << ".address_b(address_b_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".select_en_b(select_en_b_RAM" << ram->getRAMId() << "),\n";
				if ( !(ram->isReadOnly() && verilogConfigInfo->getValidMemory()) ) {
					*topFile << ".write_en_b(write_en_b_RAM" << ram->getRAMId() << "),\n";
					*topFile << ".data_in_b(data_in_b_RAM" << ram->getRAMId() << "),\n";
				}
				*topFile << ".data_out_b(data_out_b_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".size_b(size_b_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".valid_b(valid_b_RAM" << ram->getRAMId() << ")";
			}
			else { //temporal
				*topFile << ",\n";
				*topFile << ".address_b(0),\n";
				*topFile << ".select_en_b(0),\n";
				if ( !(ram->isReadOnly() && verilogConfigInfo->getValidMemory()) ) {
					*topFile << ".write_en_b(0),\n";
					*topFile << ".data_in_b(0),\n";
				}
				*topFile << ".data_out_b(data_out_b_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".size_b(0),\n";
				*topFile << ".valid_b(valid_b_RAM" << ram->getRAMId() << ")";
			}

			if ( verilogConfigInfo->getAccel() || isUsedInMemFunc(ram) ) {
				*topFile << ",\n";
				*topFile << ".address_a_ex(address_a_ex_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".select_en_a_ex(select_en_a_ex_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".write_en_a_ex(write_en_a_ex_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".data_in_a_ex(data_in_a_ex_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".data_out_a_ex(data_out_a_ex_RAM" << ram->getRAMId() << ")";
				if ( isDual ) {
					*topFile << ",\n";
					*topFile << ".address_b_ex(address_b_ex_RAM" << ram->getRAMId() << "),\n";
					*topFile << ".select_en_b_ex(select_en_b_ex_RAM" << ram->getRAMId() << "),\n";
					*topFile << ".write_en_b_ex(write_en_b_ex_RAM" << ram->getRAMId() << "),\n";
					*topFile << ".data_in_b_ex(data_in_b_ex_RAM" << ram->getRAMId() << "),\n";
					*topFile << ".data_out_b_ex(data_out_b_ex_RAM" << ram->getRAMId() << ")";
				}
				else { //temporal
					*topFile << ",\n";
					*topFile << ".address_b_ex(0),\n";
					*topFile << ".select_en_b_ex(0),\n";
					*topFile << ".write_en_b_ex(0),\n";
					*topFile << ".data_in_b_ex(0),\n";
					*topFile << ".data_out_b_ex(data_out_b_ex_RAM" << ram->getRAMId() << ")";
				}
			}
			if ( verilogConfigInfo->getValidationMode() ) {
				*topFile << ",\n.RAM" << ram->getRAMId() << "_print(";
				*topFile << "RAM" << ram->getRAMId() << "_print)";
			}
			*topFile << "\n);\n\n";
		}
		else {
			*topFile << "wire [" << ram->getElementBitSize()-1 << ":0] address_a_RAM";
			*topFile << ram->getRAMId() << ";\n";
			*topFile << "wire  select_en_a_RAM" << ram->getRAMId() << ";\n";
			*topFile << "wire  write_en_a_RAM" << ram->getRAMId() << ";\n";
			*topFile << "wire  [" << ram->getDataBitSize()-1 << ":0] data_in_a_RAM";
			*topFile << ram->getRAMId() << ";\n";
			*topFile << "wire [" << ram->getDataBitSize()-1 << ":0] data_out_a_RAM";
			*topFile << ram->getRAMId() << ";\n\n";
			if ( isDual ) {
				*topFile << "wire [" << ram->getElementBitSize()-1 << ":0] address_b_RAM";
				*topFile << ram->getRAMId() << ";\n";
				*topFile << "wire  select_en_b_RAM" << ram->getRAMId() << ";\n";
				*topFile << "wire  write_en_b_RAM" << ram->getRAMId() << ";\n";
				*topFile << "wire  [" << ram->getDataBitSize()-1 << ":0] data_in_b_RAM";
				*topFile << ram->getRAMId() << ";\n";
				*topFile << "wire [" << ram->getDataBitSize()-1 << ":0] data_out_b_RAM";
				*topFile << ram->getRAMId() << ";\n\n";
			}

			if ( verilogConfigInfo->getValidationMode() ) 
				*topFile << "reg RAM" << ram->getRAMId() << "_print;\n\n";

			if ( verilogConfigInfo->getAllLUT() ) {
				*topFile << "wire [" << ram->getDataBitSize()-1 << ":0] data_out_a_RAM_t";
				*topFile << ram->getRAMId() << ";\n";
				*topFile << "reg en_a_reg_" << ram->getRAMId() << ";\n";
				*topFile << "assign data_out_a_RAM" << ram->getRAMId() << " = en_a_reg_";
				*topFile << ram->getRAMId() << " ? data_out_a_RAM_t" << ram->getRAMId() << " : 0;\n\n";

				if ( isDual ) {
					*topFile << "wire [" << ram->getDataBitSize()-1 << ":0] data_out_b_RAM_t";
					*topFile << ram->getRAMId() << ";\n";
					*topFile << "reg en_b_reg_" << ram->getRAMId() << ";\n";
					*topFile << "assign data_out_b_RAM" << ram->getRAMId() << " = en_b_reg_";
					*topFile << ram->getRAMId() << " ? data_out_b_RAM_t"<<ram->getRAMId()<<" : 0;\n\n";
				}

				*topFile << "always @(posedge clk) begin\n";
				*topFile << "\ten_a_reg_" << ram->getRAMId() << " <= ";
				*topFile << "select_en_a_RAM" << ram->getRAMId() << ";\n";
				if ( isDual ) {
					*topFile << "\ten_b_reg_" << ram->getRAMId() << " <= ";
					*topFile << "select_en_b_RAM" << ram->getRAMId() << ";\n";
				}
				*topFile << "end\n\n";
			}

			if ( verilogConfigInfo->getPrivateBuffer() )
				*topFile << "(* keep_hierarchy = \"yes\" *)";

			*topFile << "ram_dual_port_RAM" << ram->getRAMId() << " RAM" << ram->getRAMId() << " (\n";
			*topFile << ".clk(clk),\n";
			*topFile << ".address_a(address_a_RAM" << ram->getRAMId() << "),\n";
			*topFile << ".select_en_a(select_en_a_RAM" << ram->getRAMId() << "),\n";
			*topFile << ".write_en_a(write_en_a_RAM" << ram->getRAMId() << "),\n";
			*topFile << ".data_in_a(data_in_a_RAM" << ram->getRAMId() << "),\n";
			if ( verilogConfigInfo->getAllLUT() )
				*topFile << ".data_out_a(data_out_a_RAM_t" << ram->getRAMId() << ")";
			else
				*topFile << ".data_out_a(data_out_a_RAM" << ram->getRAMId() << ")";

			if ( isDual ) {
				*topFile << ",\n";
				*topFile << ".address_b(address_b_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".select_en_b(select_en_b_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".write_en_b(write_en_b_RAM" << ram->getRAMId() << "),\n";
				*topFile << ".data_in_b(data_in_b_RAM" << ram->getRAMId() << "),\n";
				if ( verilogConfigInfo->getAllLUT() )
					*topFile << ".data_out_b(data_out_b_RAM_t" << ram->getRAMId() << ")";
				else
					*topFile << ".data_out_b(data_out_b_RAM" << ram->getRAMId() << ")";
			}

			if ( verilogConfigInfo->getValidationMode() ) {
				*topFile << ",\n.RAM" << ram->getRAMId() << "_print(";
				*topFile << "RAM" << ram->getRAMId() << "_print)";
			}
			*topFile << "\n);\n\n";
		}
	}

	void PrintVerilog::printRegisterRAMBody(raw_fd_ostream *out, bool isDual) {
		if ( isDual ) {
			*out << "always @(posedge clk) begin\n";
			*out << "\tif ( write_en_a && write_en_b ) begin\n";
			*out << "\t\tram[address_a] = data_in_a;\n";
			*out << "\t\tram[address_b] <= data_in_b;\n";
			*out << "\tend\n";
			*out << "\telse if ( write_en_a )\n";
			*out << "\t\tram[address_a] <= data_in_a;\n";
			*out << "\telse if ( write_en_b )\n";
			*out << "\t\tram[address_b] <= data_in_b;\n";
			*out << "\telse if ( select_en_a && select_en_b ) begin\n";
			*out << "\t\tdata_out_a <= ram[address_a];\n";
			*out << "\t\tdata_out_b <= ram[address_b];\n";
			*out << "\tend\n";
			*out << "\telse if ( select_en_a )\n";
			*out << "\t\tdata_out_a <= ram[address_a];\n";
			*out << "\telse if ( select_en_b )\n";
			*out << "\t\tdata_out_b <= ram[address_b];\n";
			*out << "\telse begin\n";
			*out << "\t\tdata_out_a <= 0;\n";
			*out << "\t\tdata_out_b <= 0;\n";
			*out << "\tend\n";
			*out << "end\n\n";
		}
		else {
			*out << "always @(posedge clk) begin\n";
			*out << "\tif ( write_en_a )\n";
			*out << "\t\tram[address_a] <= data_in_a;\n";
			*out << "\telse if ( select_en_a )\n";
			*out << "\t\tdata_out_a <= ram[address_a];\n";
			*out << "\telse\n";
			*out << "\t\tdata_out_a <= 0;\n";
			*out << "end\n\n";
		}
	}

	void PrintVerilog::printLUTRAMBody(raw_fd_ostream *out, bool isDual) {
		*out << "always @(posedge clk) begin\n";
		*out << "\tif (write_en_a)\n";
		*out << "\t\tram[address_a] <= data_in_a;\n";
		*out << "\telse\n";
		*out << "\t\tdata_out_a <= ram[address_a];\n";
		*out << "end\n\n";
		if ( isDual ) {
			*out << "always @(posedge clk) begin\n";
			*out << "\tdata_out_b <= ram[address_b];\n";
			*out << "end\n\n";
		}
	}

	void PrintVerilog::printBlockRAMBody(raw_fd_ostream *out, bool isDual) {
		*out << "always @(posedge clk) begin\n";
		*out << "\tif (select_en_a) begin\n";
		*out << "\t\tif (write_en_a)\n";
		*out << "\t\t\tram[address_a] <= data_in_a;\n";
		*out << "\t\telse\n";
		*out << "\t\t\tdata_out_a <= ram[address_a];\n";
		*out << "\tend\n";
		*out << "\telse\n";
		*out << "\t\tdata_out_a <= 0;\n";
		*out << "end\n\n";
		if ( isDual ) {
			*out << "always @(posedge clk) begin\n";
			*out << "\tif (select_en_b) begin\n";
			*out << "\t\tif (write_en_b)\n";
			*out << "\t\t\tram[address_b] <= data_in_b;\n";
			*out << "\t\telse\n";
			*out << "\t\t\tdata_out_b <= ram[address_b];\n";
			*out << "\tend\n";
			*out << "\telse\n";
			*out << "\t\tdata_out_b <= 0;\n";
			*out << "end\n\n";
		}
	}


}
