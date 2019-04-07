// Amazon FPGA Hardware Development Kit
//
// Copyright 2016 Amazon.com, Inc. or its affiliates. All Rights Reserved.
//
// Licensed under the Amazon Software License (the "License"). You may not use
// this file except in compliance with the License. A copy of the License is
// located at
//
//    http://aws.amazon.com/asl/
//
// or in the "license" file accompanying this file. This file is distributed on
// an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, express or
// implied. See the License for the specific language governing permissions and
// limitations under the License.


module bram_read_write_final();

import tb_type_defines_pkg::*;
`include "cl_common_defines.vh" // CL Defines with register addresses

// AXI ID
parameter [5:0] AXI_ID = 6'h0;

logic [31:0] rdata;
logic [31:0] rand_32[45];

//int count = 0;
   initial begin
      tb.power_up();
      
      for (int i=0; i<11; i++) begin
        rand_32[i] = 32'h00000000;
      end
      rand_32[11] = 32'h00000007;
      for (int i=12; i<32; i++) begin
        rand_32[i] = 32'h00000000;
      end
      
      rand_32[32] = 32'h00000013;
      rand_32[33] = 32'h00000013;
      rand_32[34] = 32'h00000013;
      rand_32[35] = 32'h00000013;
      rand_32[36] = 32'h00700613;
      rand_32[37] = 32'h00000693;
      rand_32[38] = 32'h00000793;
      rand_32[39] = 32'h00169713;
      rand_32[40] = 32'h00e787b3;
      rand_32[41] = 32'h00168693;
      rand_32[42] = 32'hfec69ae3;
      rand_32[43] = 32'h00f02023;
      rand_32[44] = 32'hffffffff;
      
        // $display ("Set new write address : Writing 0x%x to address 0x%x", 127, `WRITE_ADDR_SET_REG);
        // tb.poke(.addr(`WRITE_ADDR_SET_REG), .data(32'h0000_007F), .id(AXI_ID), .size(DataSize::UINT16), .intf(AxiPort::PORT_OCL)); // write register
        // $display ("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");

      for (int j=0; j<45; j++) begin
        $display ("Writing 0x%x to address 0x%x", rand_32[j], `HELLO_WORLD_REG_ADDR);
        tb.poke(.addr(`HELLO_WORLD_REG_ADDR), .data(rand_32[j]), .id(AXI_ID), .size(DataSize::UINT16), .intf(AxiPort::PORT_OCL)); // write register
      end

      $display ("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");

        // $display ("Set new read address : Writing 0x%x to address 0x%x", 132, `READ_ADDR_SET_REG);
        // tb.poke(.addr(`READ_ADDR_SET_REG), .data(32'h0000_0084), .id(AXI_ID), .size(DataSize::UINT16), .intf(AxiPort::PORT_OCL)); // write register
        // $display ("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
      
      // for (int k=32; k<40; k++) begin
      //   tb.peek(.addr(`HELLO_WORLD_REG_ADDR), .data(rdata), .id(AXI_ID), .size(DataSize::UINT16), .intf(AxiPort::PORT_OCL));
      //   $display ("Reading 0x%x from address 0x%x", rdata, `HELLO_WORLD_REG_ADDR);
      //   if (rdata == rand_32[k]) // Check for byte swap in register read
      //     $display ("Test PASSED");
      //   else
      //     $display ("Test FAILED");
      // end

      tb.peek(.addr(`HELLO_WORLD_REG_ADDR), .data(rdata), .id(AXI_ID), .size(DataSize::UINT16), .intf(AxiPort::PORT_OCL));
      $display ("Reading 0x%x from address 0x%x", rdata, `HELLO_WORLD_REG_ADDR);
      
      if (rdata == 32'h00000002a)
        $display ("Test PASSED");
      else
        $display ("Test FAILED");
      
      tb.kernel_reset();

      tb.power_down();
      
      $finish;
   end

endmodule // test_hello_world
