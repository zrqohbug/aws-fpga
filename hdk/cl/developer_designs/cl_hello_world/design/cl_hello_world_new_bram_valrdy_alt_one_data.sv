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

module cl_hello_world 

(
   `include "cl_ports.vh" // Fixed port definition

);

`include "cl_common_defines.vh"      // CL Defines for all examples
`include "cl_id_defines.vh"          // Defines for ID0 and ID1 (PCI ID's)
`include "cl_hello_world_defines.vh" // CL Defines for cl_hello_world

logic rst_main_n_sync;


//--------------------------------------------0
// Start with Tie-Off of Unused Interfaces
//---------------------------------------------
// the developer should use the next set of `include
// to properly tie-off any unused interface
// The list is put in the top of the module
// to avoid cases where developer may forget to
// remove it from the end of the file

`include "unused_flr_template.inc"
`include "unused_ddr_a_b_d_template.inc"
`include "unused_ddr_c_template.inc"
`include "unused_pcim_template.inc"
`include "unused_dma_pcis_template.inc"
`include "unused_cl_sda_template.inc"
`include "unused_sh_bar1_template.inc"
`include "unused_apppf_irq_template.inc"

// initial begin
//   $monitor("new_read_addr_dram = %x \n", new_read_addr_dram);
// end

//-------------------------------------------------
// Wires
//-------------------------------------------------
  logic        arvalid_q;
  logic [31:0] araddr_q;
  logic [31:0] hello_world_q;
  logic [7:0]  write_addr;
  logic [7:0]  read_addr;
  logic [31:0] ram_data;

  // Weier: need two more wires for dram and iram
  logic [31:0] dram_data;
  logic [31:0] iram_data;
  assign ram_data = (counter < 6'd32) ? memresp_data_dram : memresp_data_iram;

  // Ashish - begin
  // added to handle random address
  // read and write
  logic [7:0]  new_read_addr_dram;
  logic [7:0]  new_write_addr_dram;
  logic        set_read_addr_dram;
  logic        set_write_addr_dram;

  // Ashish - end

  // Weier: declare iram signals/addresses
  // Ashish's declarations above have been modified for dram

  logic [7:0]  new_read_addr_iram;
  logic [7:0]  new_write_addr_iram;
  logic        set_read_addr_iram;
  logic        set_write_addr_iram;

//-------------------------------------------------
// ID Values (cl_hello_world_defines.vh)
//-------------------------------------------------
  assign cl_sh_id0[31:0] = `CL_SH_ID0;
  assign cl_sh_id1[31:0] = `CL_SH_ID1;

//-------------------------------------------------
// Reset Synchronization
//-------------------------------------------------
logic pre_sync_rst_n;

always_ff @(negedge rst_main_n or posedge clk_main_a0)
   if (!rst_main_n)
   begin
      pre_sync_rst_n  <= 0;
      rst_main_n_sync <= 0;
   end
   else
   begin
      pre_sync_rst_n  <= 1;
      rst_main_n_sync <= pre_sync_rst_n;
   end

//-------------------------------------------------
// PCIe OCL AXI-L (SH to CL) Timing Flops
//-------------------------------------------------

  // Write address                                                                                                              
  logic        sh_ocl_awvalid_q;
  logic [31:0] sh_ocl_awaddr_q;
  logic        ocl_sh_awready_q;
                                                                                                                              
  // Write data                                                                                                                
  logic        sh_ocl_wvalid_q;
  logic [31:0] sh_ocl_wdata_q;
  logic [ 3:0] sh_ocl_wstrb_q;
  logic        ocl_sh_wready_q;
                                                                                                                              
  // Write response                                                                                                            
  logic        ocl_sh_bvalid_q;
  logic [ 1:0] ocl_sh_bresp_q;
  logic        sh_ocl_bready_q;
                                                                                                                              
  // Read address                                                                                                              
  logic        sh_ocl_arvalid_q;
  logic [31:0] sh_ocl_araddr_q;
  logic        ocl_sh_arready_q;
                                                                                                                              
  // Read data/response                                                                                                        
  logic        ocl_sh_rvalid_q;
  logic [31:0] ocl_sh_rdata_q;
  logic [ 1:0] ocl_sh_rresp_q;
  logic        sh_ocl_rready_q;

  axi_register_slice_light AXIL_OCL_REG_SLC (
   .aclk          (clk_main_a0),
   .aresetn       (rst_main_n_sync),
   .s_axi_awaddr  (sh_ocl_awaddr),
   .s_axi_awprot   (2'h0),
   .s_axi_awvalid (sh_ocl_awvalid),
   .s_axi_awready (ocl_sh_awready),
   .s_axi_wdata   (sh_ocl_wdata),
   .s_axi_wstrb   (sh_ocl_wstrb),
   .s_axi_wvalid  (sh_ocl_wvalid),
   .s_axi_wready  (ocl_sh_wready),
   .s_axi_bresp   (ocl_sh_bresp),
   .s_axi_bvalid  (ocl_sh_bvalid),
   .s_axi_bready  (sh_ocl_bready),
   .s_axi_araddr  (sh_ocl_araddr),
   .s_axi_arvalid (sh_ocl_arvalid),
   .s_axi_arready (ocl_sh_arready),
   .s_axi_rdata   (ocl_sh_rdata),
   .s_axi_rresp   (ocl_sh_rresp),
   .s_axi_rvalid  (ocl_sh_rvalid),
   .s_axi_rready  (sh_ocl_rready),
   .m_axi_awaddr  (sh_ocl_awaddr_q),
   .m_axi_awprot  (),
   .m_axi_awvalid (sh_ocl_awvalid_q),
   .m_axi_awready (ocl_sh_awready_q),
   .m_axi_wdata   (sh_ocl_wdata_q),
   .m_axi_wstrb   (sh_ocl_wstrb_q),
   .m_axi_wvalid  (sh_ocl_wvalid_q),
   .m_axi_wready  (ocl_sh_wready_q),
   .m_axi_bresp   (ocl_sh_bresp_q),
   .m_axi_bvalid  (ocl_sh_bvalid_q),
   .m_axi_bready  (sh_ocl_bready_q),
   .m_axi_araddr  (sh_ocl_araddr_q),
   .m_axi_arvalid (sh_ocl_arvalid_q),
   .m_axi_arready (ocl_sh_arready_q),
   .m_axi_rdata   (ocl_sh_rdata_q),
   .m_axi_rresp   (ocl_sh_rresp_q),
   .m_axi_rvalid  (ocl_sh_rvalid_q),
   .m_axi_rready  (sh_ocl_rready_q)
  );

//--------------------------------------------------------------
// PCIe OCL AXI-L Slave Accesses (accesses from PCIe AppPF BAR0)
//--------------------------------------------------------------
// Only supports single-beat accesses.

   logic        awvalid;
   logic [31:0] awaddr;
   logic        wvalid;
   logic [31:0] wdata;
   logic [3:0]  wstrb;
   logic        bready;
   logic        arvalid;
   logic [31:0] araddr;
   logic        rready;

   logic        awready;
   logic        wready;
   logic        bvalid;
   logic [1:0]  bresp;
   logic        arready;
   logic        rvalid;
   logic [31:0] rdata;
   logic [1:0]  rresp;

   // Inputs
   assign awvalid         = sh_ocl_awvalid_q;
   assign awaddr[31:0]    = sh_ocl_awaddr_q;
   assign wvalid          = sh_ocl_wvalid_q;
   assign wdata[31:0]     = sh_ocl_wdata_q;
   assign wstrb[3:0]      = sh_ocl_wstrb_q;
   assign bready          = sh_ocl_bready_q;
   assign arvalid         = sh_ocl_arvalid_q;
   assign araddr[31:0]    = sh_ocl_araddr_q;
   assign rready          = sh_ocl_rready_q;

   // Outputs
   assign ocl_sh_awready_q = awready;
   assign ocl_sh_wready_q  = wready;
   assign ocl_sh_bvalid_q  = bvalid;
   assign ocl_sh_bresp_q   = bresp[1:0];
   assign ocl_sh_arready_q = arready;
   assign ocl_sh_rvalid_q  = rvalid;
   assign ocl_sh_rdata_q   = rdata;
   assign ocl_sh_rresp_q   = rresp[1:0];

// Write Request
logic        wr_active;
logic [31:0] wr_addr;

// Ashish - begin

// need to pipeline write address and ready
logic [31:0] wr_addr_q;
logic wready_q;

// Ashish - end

always_ff @(posedge clk_main_a0)
  if (!rst_main_n_sync) begin
     wr_active <= 0;
     wr_addr   <= 0;
     wready_q  <= 0;
     wr_addr_q <= 0;
  end
  else begin
     wr_active <=  wr_active && bvalid  && bready ? 1'b0     :
                  ~wr_active && awvalid           ? 1'b1     :
                                                    wr_active;
     wr_addr <= awvalid && ~wr_active ? awaddr : wr_addr     ;
     wready_q <= wready;
     wr_addr_q <= wr_addr;
  end

assign awready = ~wr_active;
assign wready  =  wr_active && wvalid;

// Write Response
always_ff @(posedge clk_main_a0)
  if (!rst_main_n_sync) 
    bvalid <= 0;
  else
    bvalid <=  bvalid &&  bready           ? 1'b0  : 
                         ~bvalid && wready ? 1'b1  :
                                             bvalid;
assign bresp = 0;

// Read Request
always_ff @(posedge clk_main_a0)
   if (!rst_main_n_sync) begin
      arvalid_q <= 0;
      araddr_q  <= 0;
   end
   else begin
      arvalid_q <= arvalid;
      araddr_q  <= arvalid ? araddr : araddr_q;
   end

assign arready = !arvalid_q && !rvalid;

// Read Response
logic ret_wait;

logic [31:0] print;

assign print = {16'b0, wr_data_done, wr_inst_done, rd_data_done, rd_inst_done, 2'b0, counter, state};
// example: 0x0000_0            00      0
//                 done signals counter state

// Ashish - begin

// read address incremented when arvalid_q and not arvalid
// set ram_data to rdata when arvalid_q since BRAM is synchronous
always_ff @(posedge clk_main_a0)
   if (!rst_main_n_sync)
   begin
      rvalid         <= 0;
      rdata          <= 0;
      rresp          <= 0;
      ret_wait       <= 0;
   end
   else if (rvalid && rready)
   begin
      rvalid <= 0;
      rdata  <= 0;
      rresp  <= 0;
   end
   else if (state == READ_DATA || state == READ_WAIT) begin
    if (memresp_val_dram) begin
      rvalid    <= 1;
      rdata     <= memresp_data_dram;
      rresp     <= 0;
     end
   end
   //else if (read_data_done) read_addr <= 8'h00;

// Ashish - end

logic [5:0] counter;
logic [3:0] state, next_state;
logic  change_state;
// FSM

// State declarations

// localparam IDLE0      = 3'b000;
// localparam WRITE_DATA = 3'b001;
// localparam IDLE1      = 3'b010;
// localparam WRITE_INST = 3'b011;
// localparam IDLE2      = 3'b100;
// //localparam WB_REG     = 3'b101;
// localparam READ_DATA  = 3'b110; // for testing only
// localparam READ_INST  = 3'b111; // for testing only
// localparam READ_WAIT  = 3'b101;

localparam WRITE_DATA  = 4'd0;
localparam WRITE_WAIT  = 4'd1;
localparam READ_DATA   = 4'd2;
localparam READ_WAIT   = 4'd3;
//localparam SEND_BACK   = 4'd4;
localparam IDLE        = 4'd5;


// State transition blocks

always_ff @(posedge clk_main_a0) begin
  if (!rst_main_n_sync)
    state <= IDLE;
  else
    state <= next_state;
    //$display("state = %b, next_state = %b, rd_data_done = %d, rd_inst_done = %d", state, next_state, rd_data_done, rd_inst_done);
end

always_comb begin

  next_state = state;

  case(state)
    IDLE: begin
      if (wready_q && wr_addr_q == `HELLO_WORLD_REG_ADDR) next_state = WRITE_DATA;
    end
    WRITE_DATA: begin
      if (memreq_rdy_dram) next_state = WRITE_WAIT;
    end
    WRITE_WAIT: begin
      if (arvalid && araddr == `HELLO_WORLD_REG_ADDR) next_state = READ_DATA;
    end
    READ_DATA: begin
      if (memreq_rdy_dram) next_state = READ_WAIT;
    end
    READ_WAIT: begin
      if (memresp_val_dram) next_state = IDLE;
    end
    default: next_state = IDLE;
  endcase
end

// Control signals

logic wready_q_dram, wready_q_iram, arvalid_dram, arvalid_iram;
// assign wready_q_dram = !wready  && wready_q  && wr_addr_q == `HELLO_WORLD_REG_ADDR && state == WRITE_DATA;
// assign wready_q_iram = !wready  && wready_q  && wr_addr_q == `HELLO_WORLD_REG_ADDR && state == WRITE_INST && wdata != 32'hffffffff;
// assign arvalid_dram  = !arvalid && arvalid_q && araddr_q  == `HELLO_WORLD_REG_ADDR && state == READ_DATA;
// assign arvalid_iram  = !arvalid && arvalid_q && araddr_q  == `HELLO_WORLD_REG_ADDR && state == READ_INST;
assign wready_q_dram = wready_q  && wr_addr_q == `HELLO_WORLD_REG_ADDR && state == WRITE_DATA;
assign wready_q_iram = wready_q  && wr_addr_q == `HELLO_WORLD_REG_ADDR && state == WRITE_WAIT && wdata != 32'hffffffff;
assign arvalid_dram  = arvalid_q && araddr_q  == `HELLO_WORLD_REG_ADDR && state == READ_DATA;
assign arvalid_iram  = arvalid_q && araddr_q  == `HELLO_WORLD_REG_ADDR && state == READ_WAIT;

logic wr_data_done, wr_inst_done, rd_data_done, rd_inst_done;

// State output block

always_comb begin
  if (!rst_main_n_sync) begin
    hello_world_q[31:0] = 32'h0000_0000;
    write_addr = 0;
    read_addr  = 0;
    memreq_val_dram = 0;
    memreq_type_dram = 0;
    memresp_rdy_dram = 0;
    memresp_rdy_iram = 0;
  end
  case(state)
    IDLE: begin
      hello_world_q[31:0] = hello_world_q[31:0];
      write_addr = 0;
      read_addr  = 0;
      memreq_val_dram = 0;
      memreq_type_dram = 0;
      memresp_rdy_dram = 0;
      memresp_rdy_iram = 0;
    end
    WRITE_DATA: begin
      hello_world_q[31:0] = wdata[31:0];
      read_addr = 0;
      write_addr = 8'hf;
      memreq_val_dram = 1;
      memreq_type_dram = 1;
      memresp_rdy_dram = 0;
      memresp_rdy_iram = 0;
    end
    WRITE_WAIT: begin
      hello_world_q[31:0] = hello_world_q[31:0];
      write_addr = 0;
      read_addr  = 0;
      memreq_val_dram = 0;
      memreq_type_dram = 0;
      memresp_rdy_dram = 1;
      memresp_rdy_iram = 0;
    end
    READ_DATA: begin
      hello_world_q[31:0] = hello_world_q[31:0];
      write_addr = 0;
      read_addr  = 8'hf;
      memreq_val_dram = 1;
      memreq_type_dram = 0;
      memresp_rdy_dram = 1;
      memresp_rdy_iram = 0;
    end
    READ_WAIT: begin
      hello_world_q[31:0] = hello_world_q[31:0];
      write_addr = 0;
      read_addr  = 0;
      memreq_val_dram = 0;
      memreq_type_dram = 0;
      memresp_rdy_dram = 1;
      memresp_rdy_iram = 0;
    end    
    // WB_REG:
    // default: begin
    //   hello_world_q[31:0] = hello_world_q[31:0];
    //   write_addr = 0;
    //   read_addr  = 0;
    //   memresp_rdy_dram = 0;
    //   memresp_rdy_iram = 0;
    // end
  endcase
end

//-------------------------------------------------
// Hello World Register
//-------------------------------------------------


// Memory ports

logic [76:0] memreq_msg_dram;
logic        memreq_val_dram, memreq_rdy_dram;
logic [46:0] memresp_msg_dram;
logic        memresp_val_dram, memresp_rdy_dram;
logic [76:0] memreq_msg_iram;
logic        memreq_val_iram, memreq_rdy_iram;
logic [46:0] memresp_msg_iram;
logic        memresp_val_iram, memresp_rdy_iram;

logic [31:0] memreq_data_dram, memreq_addr_dram;
logic [1:0]  memreq_len_dram;
logic [7:0]  memreq_opaque_dram;
logic [2:0]  memreq_type_dram;
logic [31:0] memresp_data_dram;
logic [1:0]  memresp_len_dram, memresp_test_dram;
logic [7:0]  memresp_opaque_dram;
logic [2:0]  memresp_type_dram;

logic [31:0] memreq_data_iram, memreq_addr_iram;
logic [1:0]  memreq_len_iram;
logic [7:0]  memreq_opaque_iram;
logic [2:0]  memreq_type_iram;
logic [31:0] memresp_data_iram;
logic [1:0]  memresp_len_iram, memresp_test_iram;
logic [7:0]  memresp_opaque_iram;
logic [2:0]  memresp_type_iram;

assign memreq_data_dram      = memreq_type_dram == 3'd1 ? hello_world_q : 32'b0;
assign memreq_data_iram      = wready_q_iram ? hello_world_q : 32'b0;
assign memreq_addr_dram      = memreq_type_dram == 3'd1 ? {22'b0, write_addr, 2'b0} : {22'b0, read_addr, 2'b0}; // maybe change to sth else
assign memreq_addr_iram      = wready_q_iram ? {22'b0, write_addr, 2'b0} : {22'b0, read_addr, 2'b0};
assign memreq_len_dram       = 2'b0;
assign memreq_len_iram       = 2'b0;
assign memreq_opaque_dram    = 8'h0;
assign memreq_opaque_iram    = 8'h0;
//assign memreq_type_dram      = wready_q_dram ? 3'b001        : 3'b000;
assign memreq_type_iram      = wready_q_iram ? 3'b001        : 3'b000;

assign memreq_msg_dram = { memreq_type_dram, 
                           memreq_opaque_dram, 
                           memreq_addr_dram, 
                           memreq_len_dram, 
                           memreq_data_dram
};
assign memreq_msg_iram = { memreq_type_iram, 
                           memreq_opaque_iram, 
                           memreq_addr_iram, 
                           memreq_len_iram, 
                           memreq_data_iram
};

//assign memreq_val_dram = memreq_rdy_dram ? wready_q_dram | arvalid_dram : 0;
assign memreq_val_iram = memreq_rdy_iram ? wready_q_iram | arvalid_iram : 0;

// assign memresp_rdy_dram = 1;
// assign memresp_rdy_iram = 1;

assign memresp_data_dram   = memresp_msg_dram[31:0];
assign memresp_len_dram    = memresp_msg_dram[33:32];
assign memresp_test_dram   = memresp_msg_dram[35:34];
assign memresp_opaque_dram = memresp_msg_dram[43:36];
assign memresp_type_dram   = memresp_msg_dram[46:44];

assign memresp_data_iram   = memresp_msg_iram[31:0];
assign memresp_len_iram    = memresp_msg_iram[33:32];
assign memresp_test_iram   = memresp_msg_iram[35:34];
assign memresp_opaque_iram = memresp_msg_iram[43:36];
assign memresp_type_iram   = memresp_msg_iram[46:44];

// always_ff @(clk_main_a0) begin
//   if (memreq_val_dram && memreq_rdy_dram) begin
//     $display("memreq_msg = %x, memreq_addr = %x, memreq_data = %x, memreq_type = %b", memreq_msg_dram, memreq_addr_dram, memreq_data_dram, memreq_type_dram);
//   end
//   if (memresp_val_dram && memresp_rdy_dram) begin
//     $display("memresp_msg = %x, memresp_data = %x, memresp_type = %b", memresp_msg_dram, memresp_data_dram, memresp_type_dram);
//   end
// end

// always_ff @(clk_main_a0) begin
//   if (memresp_val_dram) $display("state = %d", state);
// end

//assign ram_data = (state == READ_DATA) ? memresp_data_dram : (state == READ_INST) ? memresp_data_iram : 0;

//-------------------------------------------------
// BRAM instantiation
//-------------------------------------------------

BramValRdy dram(

    .clk         (clk_main_a0),
    .reset       (!rst_main_n_sync),
    .memreq      (memreq_msg_dram),
    .memreq_val  (memreq_val_dram),
    .memreq_rdy  (memreq_rdy_dram),
    .memresp     (memresp_msg_dram),
    .memresp_val (memresp_val_dram),
    .memresp_rdy (memresp_rdy_dram)

);

BramValRdy iram(

    .clk         (clk_main_a0),
    .reset       (!rst_main_n_sync),
    .memreq      (memreq_msg_iram),
    .memreq_val  (memreq_val_iram),
    .memreq_rdy  (memreq_rdy_iram),
    .memresp     (memresp_msg_iram),
    .memresp_val (memresp_val_iram),
    .memresp_rdy (memresp_rdy_iram)

);

//-------------------------------------------
// Tie-Off Global Signals
//-------------------------------------------
`ifndef CL_VERSION
   `define CL_VERSION 32'hee_ee_ee_00
`endif  


  assign cl_sh_status0[31:0] =  32'h0000_0FF0;
  assign cl_sh_status1[31:0] = `CL_VERSION;

endmodule

// Translated BramValRdyPRTL.py

module BramValRdy
(
  input  logic        clk,
  input  logic        reset,
  input  logic [76:0] memreq,
  input  logic        memreq_val,
  output logic        memreq_rdy,
  output logic [46:0] memresp,
  output logic        memresp_val,
  input  logic        memresp_rdy
);
  // Unused parameters

  // num_words  = 256
  // num_bits   = 32
  // addr_width = 8
  // addr_start = 2
  // addr_end   = 10

  // always_comb begin
  //   if (memreq_val)
  //     $display("memreq_data = %x, bram_a_addr_M0 = %x, memreq_type = %b", memreq_data, memreq_addr, memreq_type);
  //   if (memresp_val)
  //     $display("memresp_data = %x, memresp_type = %b", memresp_data, memresp_type);
  // end
 
  // Wires

  logic [31:0] memreq_data, memreq_addr;
  logic [1:0]  memreq_len;
  logic [7:0]  memreq_opaque;
  logic [2:0]  memreq_type;

  assign memreq_data   = memreq[31:0];
  assign memreq_len    = memreq[33:32];
  assign memreq_addr   = memreq[65:34];
  assign memreq_opaque = memreq[73:66];
  assign memreq_type   = memreq[76:74];

  logic [31:0] memresp_data;
  logic [1:0]  memresp_len, memresp_test;
  logic [7:0]  memresp_opaque;
  logic [2:0]  memresp_type;

  assign memresp_data   = memresp[31:0];
  assign memresp_len    = memresp[33:32];
  assign memresp_test   = memresp[35:34];
  assign memresp_opaque = memresp[43:36];
  assign memresp_type   = memresp[46:44];

  // M0 stage

  logic        memreq_go_M0;
  logic [31:0] bram_a_addr_32_M0;
  logic [7:0]  bram_a_addr_M0;
  logic        bram_a_wen_M0, bram_a_en_M0;
  logic [31:0] bram_a_wdata_M0, bram_a_rdata_M1;

  assign memreq_go_M0      = memreq_val & memreq_rdy;
  assign bram_a_addr_32_M0 = memreq_addr;
  assign bram_a_addr_M0    = bram_a_addr_32_M0[9:2];
  assign bram_a_wen_M0     = memreq_val & memreq_type == 1;
  assign bram_a_en_M0      = memreq_go_M0;
  assign bram_a_wdata_M0   = memreq_data;

  // BRAM

  // BramRTL bram(
  //   .clk   (clk),
  //   .en    (bram_a_en_M0),
  //   .we    (bram_a_wen_M0),
  //   .addr  (bram_a_addr_M0),
  //   .wdata (bram_a_wdata_M0),
  //   .rdata (bram_a_rdata_M1)
  // );

  cl_bram_wrapper bram(

    .clk(clk),
    .write_en_a(bram_a_wen_M0),
    .en_a(bram_a_en_M0),
    .addr_a(bram_a_addr_M0),
    .write_data_a(bram_a_wdata_M0), 
    .read_data_a(),


    .write_en_b(1'b0),
    .en_b(bram_a_en_M0),
    .addr_b(bram_a_addr_M0),//new_read_addr_dram),
    .write_data_b(32'b0),
    .read_data_b(bram_a_rdata_M1)

  );

  // Pipeline registers

  logic        memreq_go_M0_reg;
  logic [31:0] memreq_data_reg, memreq_addr_reg;
  logic [1:0]  memreq_len_reg;
  logic [7:0]  memreq_opaque_reg;
  logic [2:0]  memreq_type_reg;
  logic        memreq_val_reg;

  always_ff @(posedge clk) begin
    if (reset) begin
      memreq_go_M0_reg  <= 0;
      memreq_data_reg   <= 0;
      memreq_addr_reg   <= 0;
      memreq_len_reg    <= 0;
      memreq_opaque_reg <= 0;
      memreq_type_reg   <= 0;
      memreq_val_reg    <= 0;
    end
    else begin
      memreq_go_M0_reg  <= memreq_go_M0;
      memreq_data_reg   <= memreq_data;
      memreq_addr_reg   <= memreq_addr;
      memreq_len_reg    <= memreq_len;
      memreq_opaque_reg <= memreq_opaque;
      memreq_type_reg   <= memreq_type;
      memreq_val_reg    <= memreq_val;
    end
  end

  // M1 stage

  logic [31:0] memresp_msg_data_M1;

  assign memresp_msg_data_M1 = memreq_type_reg == 3'b000 ? bram_a_rdata_M1 : 32'h0;

  // Bypass queues

  logic memresp_queue_rdy;
  logic memresp_val_temp;

  assign memresp_val = memresp_val_temp & memresp_rdy;//& bram_a_rdata_M1 != 0 & memreq_type_reg == 3'b000;
  //assign memresp = 0;
  TwoElementBypassQueue_bram queue(
    .clk (clk),
    .deq_msg (memresp),
    .deq_rdy (memresp_rdy),
    .deq_val (memresp_val_temp),//(memresp_val),
    .empty   (memreq_rdy),
    .enq_msg ({memreq_type_reg, 
               memreq_opaque_reg, 
               2'b00, // test
               memreq_len_reg, 
               memresp_msg_data_M1}),
    .enq_rdy (memresp_queue_rdy),
    .enq_val (memreq_val_reg),
    .full    (),
    .reset   (reset)
  );

endmodule

// Divya's BRAM Implementation

//
// Single Port RAM
//

module BramRTL
(
  input  logic        clk,
  input  logic        en,
  input  logic        we,
  input  logic [4:0]  addr,
  input  logic [31:0] wdata,
  output logic [31:0] rdata
);

  logic  [31:0] ram [31:0];
  logic  [31:0] rdata_in;

  always @(posedge clk) begin
    if (en) begin
      if (we)  ram[addr] <= wdata;
      if (!we) rdata_in <= ram[addr];
    end
  end

  assign rdata = rdata_in ;

endmodule

// TwoElementBypassQueue
// Taken from translated code in ProcRTL.v

module TwoElementBypassQueue_bram
(
  input  wire [   0:0] clk,
  output wire [  46:0] deq_msg,
  input  wire [   0:0] deq_rdy,
  output wire [   0:0] deq_val,
  output reg  [   0:0] empty,
  input  wire [  46:0] enq_msg,
  output wire [   0:0] enq_rdy,
  input  wire [   0:0] enq_val,
  output reg  [   0:0] full,
  input  wire [   0:0] reset
);

  // queue1 temporaries
  wire   [   0:0] queue1$clk;
  wire   [  46:0] queue1$enq_msg;
  wire   [   0:0] queue1$enq_val;
  wire   [   0:0] queue1$reset;
  wire   [   0:0] queue1$deq_rdy;
  wire   [   0:0] queue1$enq_rdy;
  wire   [   0:0] queue1$full;
  wire   [  46:0] queue1$deq_msg;
  wire   [   0:0] queue1$deq_val;

  SingleElementBypassQueue_bram queue1
  (
    .clk     ( queue1$clk ),
    .enq_msg ( queue1$enq_msg ),
    .enq_val ( queue1$enq_val ),
    .reset   ( queue1$reset ),
    .deq_rdy ( queue1$deq_rdy ),
    .enq_rdy ( queue1$enq_rdy ),
    .full    ( queue1$full ),
    .deq_msg ( queue1$deq_msg ),
    .deq_val ( queue1$deq_val )
  );

  // queue0 temporaries
  wire   [   0:0] queue0$clk;
  wire   [  46:0] queue0$enq_msg;
  wire   [   0:0] queue0$enq_val;
  wire   [   0:0] queue0$reset;
  wire   [   0:0] queue0$deq_rdy;
  wire   [   0:0] queue0$enq_rdy;
  wire   [   0:0] queue0$full;
  wire   [  46:0] queue0$deq_msg;
  wire   [   0:0] queue0$deq_val;

  SingleElementBypassQueue_bram queue0
  (
    .clk     ( queue0$clk ),
    .enq_msg ( queue0$enq_msg ),
    .enq_val ( queue0$enq_val ),
    .reset   ( queue0$reset ),
    .deq_rdy ( queue0$deq_rdy ),
    .enq_rdy ( queue0$enq_rdy ),
    .full    ( queue0$full ),
    .deq_msg ( queue0$deq_msg ),
    .deq_val ( queue0$deq_val )
  );

  // signal connections
  assign deq_msg        = queue1$deq_msg;
  assign deq_val        = queue1$deq_val;
  assign enq_rdy        = queue0$enq_rdy;
  assign queue0$clk     = clk;
  assign queue0$deq_rdy = queue1$enq_rdy;
  assign queue0$enq_msg = enq_msg;
  assign queue0$enq_val = enq_val;
  assign queue0$reset   = reset;
  assign queue1$clk     = clk;
  assign queue1$deq_rdy = deq_rdy;
  assign queue1$enq_msg = queue0$deq_msg;
  assign queue1$enq_val = queue0$deq_val;
  assign queue1$reset   = reset;

  // logic for full_empty()
  always @ (*) begin
    full = (queue0$full&queue1$full);
    empty = (~queue0$full&~queue1$full);
  end


endmodule // TwoElementBypassQueue_bram
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueue_bram
//-----------------------------------------------------------------------------
// dtype: 77
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueue_bram
(
  input  wire [   0:0] clk,
  output wire [  46:0] deq_msg,
  input  wire [   0:0] deq_rdy,
  output wire [   0:0] deq_val,
  input  wire [  46:0] enq_msg,
  output wire [   0:0] enq_rdy,
  input  wire [   0:0] enq_val,
  output wire [   0:0] full,
  input  wire [   0:0] reset
);

  // ctrl temporaries
  wire   [   0:0] ctrl$clk;
  wire   [   0:0] ctrl$enq_val;
  wire   [   0:0] ctrl$reset;
  wire   [   0:0] ctrl$deq_rdy;
  wire   [   0:0] ctrl$bypass_mux_sel;
  wire   [   0:0] ctrl$wen;
  wire   [   0:0] ctrl$deq_val;
  wire   [   0:0] ctrl$full;
  wire   [   0:0] ctrl$enq_rdy;

  SingleElementBypassQueueCtrl_bram ctrl
  (
    .clk            ( ctrl$clk ),
    .enq_val        ( ctrl$enq_val ),
    .reset          ( ctrl$reset ),
    .deq_rdy        ( ctrl$deq_rdy ),
    .bypass_mux_sel ( ctrl$bypass_mux_sel ),
    .wen            ( ctrl$wen ),
    .deq_val        ( ctrl$deq_val ),
    .full           ( ctrl$full ),
    .enq_rdy        ( ctrl$enq_rdy )
  );

  // dpath temporaries
  wire   [   0:0] dpath$wen;
  wire   [   0:0] dpath$bypass_mux_sel;
  wire   [   0:0] dpath$clk;
  wire   [   0:0] dpath$reset;
  wire   [  46:0] dpath$enq_bits;
  wire   [  46:0] dpath$deq_bits;

  SingleElementBypassQueueDpath_bram dpath
  (
    .wen            ( dpath$wen ),
    .bypass_mux_sel ( dpath$bypass_mux_sel ),
    .clk            ( dpath$clk ),
    .reset          ( dpath$reset ),
    .enq_bits       ( dpath$enq_bits ),
    .deq_bits       ( dpath$deq_bits )
  );

  // signal connections
  assign ctrl$clk             = clk;
  assign ctrl$deq_rdy         = deq_rdy;
  assign ctrl$enq_val         = enq_val;
  assign ctrl$reset           = reset;
  assign deq_msg              = dpath$deq_bits;
  assign deq_val              = ctrl$deq_val;
  assign dpath$bypass_mux_sel = ctrl$bypass_mux_sel;
  assign dpath$clk            = clk;
  assign dpath$enq_bits       = enq_msg;
  assign dpath$reset          = reset;
  assign dpath$wen            = ctrl$wen;
  assign enq_rdy              = ctrl$enq_rdy;
  assign full                 = ctrl$full;



endmodule // SingleElementBypassQueue_bram
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueueCtrl_0x2a979dc5ff91cb88
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueueCtrl_bram
(
  output reg  [   0:0] bypass_mux_sel,
  input  wire [   0:0] clk,
  input  wire [   0:0] deq_rdy,
  output reg  [   0:0] deq_val,
  output reg  [   0:0] enq_rdy,
  input  wire [   0:0] enq_val,
  output reg  [   0:0] full,
  input  wire [   0:0] reset,
  output reg  [   0:0] wen
);

  // register declarations
  reg    [   0:0] do_bypass;
  reg    [   0:0] do_deq;
  reg    [   0:0] do_enq;



  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def seq():
  //
  //       # TODO: can't use temporaries here, verilog simulation semantics
  //       #       don't match the Python semantics!
  //       ## helper signals
  //
  //       #do_deq    = s.deq_rdy and s.deq_val
  //       #do_enq    = s.enq_rdy and s.enq_val
  //       #do_bypass = ~s.full and do_deq and do_enq
  //
  //       # full bit calculation: the full bit is cleared when a dequeue
  //       # transaction occurs; the full bit is set when the queue storage is
  //       # empty and a enqueue transaction occurs and when we are not bypassing
  //
  //       if   s.reset:                      s.full.next = 0
  //       elif s.do_deq:                     s.full.next = 0
  //       elif s.do_enq and not s.do_bypass: s.full.next = 1
  //       else:                              s.full.next = s.full

  // logic for seq()
  always @ (posedge clk) begin
    if (reset) begin
      full <= 0;
    end
    else begin
      if (do_deq) begin
        full <= 0;
      end
      else begin
        if ((do_enq&&!do_bypass)) begin
          full <= 1;
        end
        else begin
          full <= full;
        end
      end
    end
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb():
  //
  //       # bypass is always enabled when the queue is empty
  //
  //       s.bypass_mux_sel.value = ~s.full
  //
  //       # wen control signal: set the write enable signal if the storage queue
  //       # is empty and a valid enqueue request is present
  //
  //       s.wen.value = ~s.full & s.enq_val
  //
  //       # enq_rdy signal is asserted when the single element queue storage is
  //       # empty
  //
  //       s.enq_rdy.value = ~s.full
  //
  //       # deq_val signal is asserted when the single element queue storage is
  //       # full or when the queue is empty but we are bypassing
  //
  //       s.deq_val.value = s.full | ( ~s.full & s.enq_val )
  //
  //       # TODO: figure out how to make these work as temporaries
  //       # helper signals
  //
  //       s.do_deq.value    = s.deq_rdy and s.deq_val
  //       s.do_enq.value    = s.enq_rdy and s.enq_val
  //       s.do_bypass.value = ~s.full and s.do_deq and s.do_enq

  // logic for comb()
  always @ (*) begin
    bypass_mux_sel = ~full;
    wen = (~full&enq_val);
    enq_rdy = ~full;
    deq_val = (full|(~full&enq_val));
    do_deq = (deq_rdy&&deq_val);
    do_enq = (enq_rdy&&enq_val);
    do_bypass = (~full&&do_deq&&do_enq);
  end


endmodule // SingleElementBypassQueueCtrl_bram
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueueDpath_bram
//-----------------------------------------------------------------------------
// dtype: 77
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueueDpath_bram
(
  input  wire [   0:0] bypass_mux_sel,
  input  wire [   0:0] clk,
  output wire [  46:0] deq_bits,
  input  wire [  46:0] enq_bits,
  input  wire [   0:0] reset,
  input  wire [   0:0] wen
);

  // bypass_mux temporaries
  wire   [   0:0] bypass_mux$reset;
  wire   [  46:0] bypass_mux$in_$000;
  wire   [  46:0] bypass_mux$in_$001;
  wire   [   0:0] bypass_mux$clk;
  wire   [   0:0] bypass_mux$sel;
  wire   [  46:0] bypass_mux$out;

  Mux_bram bypass_mux
  (
    .reset   ( bypass_mux$reset ),
    .in_$000 ( bypass_mux$in_$000 ),
    .in_$001 ( bypass_mux$in_$001 ),
    .clk     ( bypass_mux$clk ),
    .sel     ( bypass_mux$sel ),
    .out     ( bypass_mux$out )
  );

  // queue temporaries
  wire   [   0:0] queue$reset;
  wire   [  46:0] queue$in_;
  wire   [   0:0] queue$clk;
  wire   [   0:0] queue$en;
  wire   [  46:0] queue$out;

  RegEn_bram queue
  (
    .reset ( queue$reset ),
    .in_   ( queue$in_ ),
    .clk   ( queue$clk ),
    .en    ( queue$en ),
    .out   ( queue$out )
  );

  // signal connections
  assign bypass_mux$clk     = clk;
  assign bypass_mux$in_$000 = queue$out;
  assign bypass_mux$in_$001 = enq_bits;
  assign bypass_mux$reset   = reset;
  assign bypass_mux$sel     = bypass_mux_sel;
  assign deq_bits           = bypass_mux$out;
  assign queue$clk          = clk;
  assign queue$en           = wen;
  assign queue$in_          = enq_bits;
  assign queue$reset        = reset;



endmodule // SingleElementBypassQueueDpath_bram
`default_nettype wire

//-----------------------------------------------------------------------------
// Mux_bram
//-----------------------------------------------------------------------------
// dtype: 77
// nports: 2
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module Mux_bram
(
  input  wire [   0:0] clk,
  input  wire [  46:0] in_$000,
  input  wire [  46:0] in_$001,
  output reg  [  46:0] out,
  input  wire [   0:0] reset,
  input  wire [   0:0] sel
);

  // localparam declarations
  localparam nports = 2;

  // array declarations
  wire   [  46:0] in_[0:1];
  assign in_[  0] = in_$000;
  assign in_[  1] = in_$001;

  // logic for comb_logic()
  always @ (*) begin
    out = in_[sel];
  end


endmodule // Mux_bram
`default_nettype wire

//-----------------------------------------------------------------------------
// RegEn_bram
//-----------------------------------------------------------------------------
// dtype: 77
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module RegEn_bram
(
  input  wire [   0:0] clk,
  input  wire [   0:0] en,
  input  wire [  46:0] in_,
  output reg  [  46:0] out,
  input  wire [   0:0] reset
);

  // logic for seq_logic()
  always @ (posedge clk) begin
    if (en) begin
      out <= in_;
    end
    else begin
    end
  end


endmodule
`default_nettype wire