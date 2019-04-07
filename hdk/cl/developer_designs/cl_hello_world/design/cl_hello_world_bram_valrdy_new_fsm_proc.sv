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
// `include "ProcRTL.sv"
// `include "DPBramValRdyRTL.sv"

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

// State declarations

localparam IDLE        = 4'b0000;
localparam SET_WR_DATA = 4'b0001;
localparam WRITE_DATA  = 4'b0010;
localparam SET_WR_INST = 4'b0011;
localparam WRITE_INST  = 4'b0100;
localparam SET_RD_DATA = 4'b0101;
localparam READ_DATA   = 4'b0110;
localparam SET_RD_INST = 4'b0111;
localparam READ_INST   = 4'b1000;

// new states
localparam WR_DATA_WAIT = 4'b1001;
localparam WR_INST_WAIT = 4'b1010;
localparam RD_DATA_WAIT = 4'b1011;
localparam RD_INST_WAIT = 4'b1100;

localparam POLLING      = 4'b1101;
localparam POLLING_WAIT = 4'b1110;
localparam RETURN       = 4'b1111;

// Ashish - begin

// read address incremented when arvalid_q and not arvalid
// set ram_data to rdata when arvalid_q since BRAM is synchronous
always_ff @(posedge clk_main_a0)
   if (!rst_main_n_sync)
   begin
      rvalid         <= 0;
      rdata          <= 0;
      rresp          <= 0;
      read_addr[7:0] <= 8'h00;
      ret_wait       <= 0;
   end
   // set random read address
   else if (set_read_addr_dram)
   begin
     read_addr <= new_read_addr_dram;
   end
   else if (set_read_addr_iram)
   begin
     read_addr <= new_read_addr_iram;
   end
   else if (rvalid && rready)
   begin
      rvalid <= 0;
      rdata  <= 0;
      rresp  <= 0;
   end
   // Weier: changed the logic below to allow the host 
   // to peek more registers to help debugging
   // rdata's assignment in the first two branches (
   // araddr_q = WRITE_ADDR_SET_REG or READ_ADDR_SET_REG)
   // could be changed into other signals that we are
   // interested in. Host could use "peek" to read them
   else if (arvalid_q)
   begin
      if (araddr_q == `WRITE_ADDR_SET_REG) begin
        rvalid <= 1;
        rdata  <= print;
        rresp  <= 0;
      end
      else if (araddr_q == `READ_ADDR_SET_REG) begin
        rvalid <= 1;
        rdata  <= counter;
        rresp  <= 0;
      end
      else if (araddr_q == `HELLO_WORLD_REG_ADDR) begin
        if (state == RETURN) begin
          rvalid <= 1;
          rdata  <= memresp_data_dram_reg;
          rresp  <= 0;
        end
      end
    end
    else if (state == RETURN) begin
      rvalid <= 1;
      rdata  <= memresp_data_dram_reg;
      rresp  <= 0;
    end
   else rvalid <= 0;

// Ashish - end

// FSM

logic [5:0] counter;
logic [3:0] state, next_state;
logic  change_state;

// State transition blocks
// The following states are not used, but are preserved to
// help debugging once needed:
// SET_RD_DATA, READ_DATA, RD_DATA_WAIT, SET_RD_INST,
// READ_INST, RD_INST_WAIT

always_ff @(posedge clk_main_a0) begin
  if (!rst_main_n_sync)
    state <= IDLE;
  else
    state <= next_state;
end

always_comb begin

  next_state = state;

  case(state)
    IDLE: begin
      if      (wr_data_done) next_state = SET_WR_INST;
      else if (wr_inst_done) next_state = POLLING;
      else                   next_state = SET_WR_DATA;
    end
    SET_WR_DATA:
      next_state = WRITE_DATA;
    SET_WR_INST:
      next_state = WRITE_INST;
    SET_RD_DATA:
      next_state = READ_DATA;
    SET_RD_INST:
      next_state = READ_INST;
    WRITE_DATA: begin
      if (wready_q_dram) begin
        if (!memreq_rdy_dram) next_state = WRITE_DATA;
        else                  next_state = WR_DATA_WAIT;
      end
      else                    next_state = WRITE_DATA;
    end
    WR_DATA_WAIT: begin
      if (counter < 31) begin
        if (memresp_val_dram) next_state = WRITE_DATA;
        else                  next_state = WR_DATA_WAIT;
      end
      else                    next_state = IDLE;
    end
    WRITE_INST: begin
      if (wdata[31:0] == 32'hffffffff) next_state = IDLE;
      else if (wready_q_iram) begin
        if (!memreq_rdy_iram)          next_state = WRITE_INST;
        else                           next_state = WR_INST_WAIT;
      end
      else                             next_state = WRITE_INST;
    end
    WR_INST_WAIT: begin
      if (wdata[31:0] == 32'hffffffff) next_state = IDLE;
      else if (memresp_val_iram)       next_state = WRITE_INST;
      else                             next_state = WR_INST_WAIT;
    end
    POLLING: begin
      if (memreq_rdy_dram) next_state = POLLING_WAIT;
      else                 next_state = POLLING;
    end
    POLLING_WAIT: begin
      if (memresp_val_dram) begin
        if (terminate) next_state = RETURN;
        else           next_state = POLLING;
      end
    end
    RETURN: begin
      if (arvalid_q && araddr_q == `HELLO_WORLD_REG_ADDR)
        next_state = IDLE;
      else
        next_state = RETURN;
    end
    READ_DATA: begin
      if (arvalid_q_dram) begin
        if (!memreq_rdy_dram) next_state = READ_DATA;
        else                  next_state = RD_DATA_WAIT;
      end
      else                    next_state = READ_DATA;
    end
    RD_DATA_WAIT: begin
      if (counter < 31) begin
        if (memresp_val_dram) next_state = READ_DATA;
        else                  next_state = RD_DATA_WAIT;
      end
      else                    next_state = IDLE;
    end
    READ_INST: begin
      if (arvalid_q_iram) begin
        if (!memreq_rdy_iram) next_state = READ_INST;
        else                  next_state = RD_INST_WAIT;
      end
      else                    next_state = READ_INST;
    end
    RD_INST_WAIT: begin
      if (memresp_val_iram) begin
        if (inst_count_current == inst_count_total) next_state = IDLE;
        else                                        next_state = READ_INST;
      end
      else                                          next_state = RD_INST_WAIT;
    end
    default: next_state = IDLE;
  endcase
end

// Control signals

logic wready_q_dram, wready_q_iram, arvalid_q_dram, arvalid_q_iram;
assign wready_q_dram  = wready_q  && wr_addr_q == `HELLO_WORLD_REG_ADDR;
assign wready_q_iram  = wready_q  && wr_addr_q == `HELLO_WORLD_REG_ADDR && wdata != 32'hffffffff;
assign arvalid_q_dram = arvalid_q && araddr_q  == `HELLO_WORLD_REG_ADDR;
assign arvalid_q_iram = arvalid_q && araddr_q  == `HELLO_WORLD_REG_ADDR;

logic wr_data_done, wr_inst_done, rd_data_done, rd_inst_done;

always_ff @(posedge clk_main_a0) begin
  if (!rst_main_n_sync) begin
    wr_data_done <= 0;
    wr_inst_done <= 0;
    rd_data_done <= 0;
    rd_inst_done <= 0;
  end
  if (next_state == IDLE) begin
    if (state == WR_DATA_WAIT || state == WRITE_DATA)
      wr_data_done <= 1;
    else if (state == WR_INST_WAIT || state == WRITE_INST)
      wr_inst_done <= 1;
    else if (state == RD_DATA_WAIT || state == READ_DATA)
      rd_data_done <= 1;
    else if (state == RD_INST_WAIT || state == READ_INST)
      rd_inst_done <= 1;
  end
  else if (state != IDLE) begin
    wr_data_done <= 0;
    wr_inst_done <= 0;
    rd_data_done <= 0;
    rd_inst_done <= 0;
  end
end

// go signal for processor
logic proc_go;

always_ff @(posedge clk_main_a0) begin
  if (!rst_main_n_sync) begin
    proc_go <= 0;
  end
  else if (!proc_go && !terminate) begin
    if (next_state == POLLING && state == IDLE)
      proc_go <= 1;
  end
  else if (terminate) begin
    proc_go <= 0;
  end
end

// Counter increment
logic inc_en;
logic [7:0] inst_count_total, inst_count_current; // might need to increase size later

always_ff @(posedge clk_main_a0) begin
  if (!rst_main_n_sync) begin
    counter            <= 6'd0;
    inc_en             <= 0;
    inst_count_total   <= 0;
    inst_count_current <= 0;
  end
  else if (state == WRITE_DATA)
    inc_en <= 1;
  else if (state == WR_DATA_WAIT) begin
    if (inc_en) begin
      counter <= counter + 1;
      inc_en  <= 0;
    end
  end
  else if (state == WRITE_INST) begin
    inc_en <= 1;
  end
  else if (state == WR_INST_WAIT) begin
    if (inc_en) begin
      inst_count_total <= inst_count_total + 1;
      inc_en           <= 0;
    end
  end
  else if (state == READ_DATA) begin
    inc_en <= 1;
  end
  else if (state == RD_DATA_WAIT) begin
    if (inc_en) begin
      counter <= counter + 1;
      inc_en  <= 0;
    end
  end
  else if (state == READ_INST) begin
    counter <= 32;
    inc_en  <= 1;
  end
  else if (state == RD_INST_WAIT) begin
    if (inc_en) begin
      inst_count_current <= inst_count_current + 1;
      inc_en             <= 0;
    end
  end
  else begin
    counter <= 0;
    inc_en  <= 0;
  end
end

// State output block

logic terminate;

assign terminate = memresp_data_dram == 32'h0000002a;

always_comb begin
  if (!rst_main_n_sync) begin
    memreq_data_dram         = 32'h00000000;
    memreq_addr_dram         = 32'h00000000;
    memreq_type_dram         = 3'b000;
    memreq_data_iram         = 32'h00000000;
    memreq_addr_iram         = 32'h00000000;
    memreq_type_iram         = 3'b000;
    memreq_val_dram          = 1'b0;
    memreq_val_iram          = 1'b0;
    memresp_rdy_dram         = 1'b0;
    memresp_rdy_iram         = 1'b0;
    set_read_addr_dram       = 1'b0;
    new_read_addr_dram[7:0]  = 8'h00;
    set_write_addr_dram      = 1'b0;
    new_write_addr_dram[7:0] = 8'hff;
    set_read_addr_iram       = 1'b0;
    new_read_addr_iram[7:0]  = 8'h00;
    set_write_addr_iram      = 1'b0;
    new_write_addr_iram[7:0] = 8'hff;
  end
  case(state)
    IDLE: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    SET_WR_DATA: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b1;
      new_write_addr_dram[7:0] = 8'hff;
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    SET_WR_INST: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b1;
      new_write_addr_iram[7:0] = 8'hff;
    end
    SET_RD_DATA: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b1;
      new_read_addr_dram[7:0]  = 8'h00;
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    SET_RD_INST: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b1;
      new_read_addr_iram[7:0]  = 8'h00;
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    WRITE_DATA: begin
      memreq_data_dram         = wdata;
      memreq_addr_dram         = {22'b0, write_addr, 2'b0};
      memreq_type_dram         = 3'b001;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = wready_q_dram & memreq_rdy_dram;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    WR_DATA_WAIT: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b1;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    WRITE_INST: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = wdata;
      memreq_addr_iram         = {22'b0, write_addr, 2'b0};
      memreq_type_iram         = 3'b001;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = wready_q_iram & memreq_rdy_iram;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    WR_INST_WAIT: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b1;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    POLLING: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b1;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    POLLING_WAIT: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b1;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    RETURN: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    READ_DATA: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = {22'b0, read_addr, 2'b0};
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = arvalid_q_dram & memreq_rdy_dram;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    RD_DATA_WAIT: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b1;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    READ_INST: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = {22'b0, read_addr, 2'b0};
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = arvalid_q_iram & memreq_rdy_iram;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    RD_INST_WAIT: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b1;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
    // WB_REG:
    default: begin
      memreq_data_dram         = 32'h00000000;
      memreq_addr_dram         = 32'h00000000;
      memreq_type_dram         = 3'b000;
      memreq_data_iram         = 32'h00000000;
      memreq_addr_iram         = 32'h00000000;
      memreq_type_iram         = 3'b000;
      memreq_val_dram          = 1'b0;
      memreq_val_iram          = 1'b0;
      memresp_rdy_dram         = 1'b0;
      memresp_rdy_iram         = 1'b0;
      set_read_addr_dram       = 1'b0;
      new_read_addr_dram[7:0]  = new_read_addr_dram[7:0];
      set_write_addr_dram      = 1'b0;
      new_write_addr_dram[7:0] = new_write_addr_dram[7:0];
      set_read_addr_iram       = 1'b0;
      new_read_addr_iram[7:0]  = new_read_addr_iram[7:0];
      set_write_addr_iram      = 1'b0;
      new_write_addr_iram[7:0] = new_write_addr_iram[7:0];
    end
  endcase
end

logic [31:0] memresp_data_dram_reg;

always_ff @(posedge clk_main_a0) begin
  if (!rst_main_n_sync)      memresp_data_dram_reg <= 0;
  else if (memresp_val_dram) memresp_data_dram_reg <= memresp_data_dram;
end

//-------------------------------------------------
// Hello World Register
//-------------------------------------------------

// Ashish - begin

// write address incremented when wready_q and not wready
always_ff @(posedge clk_main_a0) begin
   if (!rst_main_n_sync) begin                    // Reset
      write_addr[7:0]     <= 8'h0000_0000;
   end
   // set random write address
   else if (set_write_addr_dram)
   begin
     write_addr <= new_write_addr_dram;
   end
   else if (set_write_addr_iram)
   begin
    write_addr <= new_write_addr_iram;
   end
   else if (wready && (wr_addr == `HELLO_WORLD_REG_ADDR)) begin
      write_addr          <= write_addr + 1;
   end
   else begin                                // Hold Value
      write_addr          <= write_addr;
   end
end
// Ashish - end

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

assign memreq_len_dram       = 2'b0;
assign memreq_len_iram       = 2'b0;
assign memreq_opaque_dram    = 8'h0;
assign memreq_opaque_iram    = 8'h0;

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

// Processor wire bundles

logic [76:0] dmemreq_msg,  imemreq_msg;
logic [46:0] dmemresp_msg, imemresp_msg;
logic        dmemreq_rdy,  dmemreq_val, dmemresp_rdy, dmemresp_val;
logic        imemreq_rdy,  imemreq_val, imemresp_rdy, imemresp_val;

//-------------------------------------------------
// BRAM instantiation
//-------------------------------------------------

DPBramValRdyRTL dram(

    .clk           (clk_main_a0),
    .reset         (!rst_main_n_sync),
    .memreq_a_msg  (memreq_msg_dram[76:74] == 3'b001 ? memreq_msg_dram : dmemreq_msg),
    .memreq_a_val  (memreq_val_dram),
    .memreq_a_rdy  (memreq_rdy_dram),
    .memreq_b_msg  (dmemreq_msg),
    .memreq_b_val  (dmemreq_val),
    .memreq_b_rdy  (dmemreq_rdy),
    .memresp_a_msg (memresp_msg_dram),
    .memresp_a_val (memresp_val_dram),
    .memresp_a_rdy (memresp_rdy_dram),
    .memresp_b_msg (dmemresp_msg),
    .memresp_b_val (dmemresp_val),
    .memresp_b_rdy (dmemresp_rdy)

);

DPBramValRdyRTL iram(

    .clk           (clk_main_a0),
    .reset         (!rst_main_n_sync),
    .memreq_a_msg  (memreq_msg_iram),
    .memreq_a_val  (memreq_val_iram),
    .memreq_a_rdy  (memreq_rdy_iram),
    .memreq_b_msg  (imemreq_msg),
    .memreq_b_val  (imemreq_val),
    .memreq_b_rdy  (imemreq_rdy),
    .memresp_a_msg (memresp_msg_iram),
    .memresp_a_val (memresp_val_iram),
    .memresp_a_rdy (memresp_rdy_iram),
    .memresp_b_msg (imemresp_msg),
    .memresp_b_val (imemresp_val),
    .memresp_b_rdy (imemresp_rdy)

);

// Processor instantiation

logic [31:0] inst_count, cycle_count;
logic        commit_inst, stats_en;

Verilog_ProcRTL proc(

    .clk           (clk_main_a0),
    .commit_inst   (commit_inst),
    .dmemreq_msg   (dmemreq_msg),
    .dmemreq_rdy   (dmemreq_rdy),
    .dmemreq_val   (dmemreq_val),
    .dmemresp_msg  (dmemresp_msg),
    .dmemresp_rdy  (dmemresp_rdy),
    .dmemresp_val  (dmemresp_val),
    .go            (proc_go),
    .imemreq_msg   (imemreq_msg),
    .imemreq_rdy   (imemreq_rdy),
    .imemreq_val   (imemreq_val),
    .imemresp_msg  (imemresp_msg),
    .imemresp_rdy  (imemresp_rdy),
    .imemresp_val  (imemresp_val),
    .mngr2proc_msg (),
    .mngr2proc_rdy (),
    .mngr2proc_val (),
    .proc2mngr_msg (),
    .proc2mngr_rdy (),
    .proc2mngr_val (),
    .reset         (!rst_main_n_sync),
    .stats_en      (stats_en)

);

always_ff @(posedge clk_main_a0) begin
  if (!rst_main_n_sync) begin
    inst_count  <= 0;
    cycle_count <= 0;
  end
  else if (stats_en) begin
    cycle_count <= cycle_count + 1;
    if (commit_inst)
      inst_count <= inst_count + 1;
  end
end

//-------------------------------------------
// Tie-Off Global Signals
//-------------------------------------------
`ifndef CL_VERSION
   `define CL_VERSION 32'hee_ee_ee_00
`endif  


  assign cl_sh_status0[31:0] =  32'h0000_0FF0;
  assign cl_sh_status1[31:0] = `CL_VERSION;

  // Nitish: dump vcd file
  initial begin
    $dumpfile("proc.vcd");
    $dumpvars();
  end

endmodule

//-----------------------------------------------------------------------------
// DPBramValRdyRTL
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module DPBramValRdyRTL
(
  input  wire [   0:0] clk,
  input  wire [  76:0] memreq_a_msg,
  output wire [   0:0] memreq_a_rdy,
  input  wire [   0:0] memreq_a_val,
  input  wire [  76:0] memreq_b_msg,
  output wire [   0:0] memreq_b_rdy,
  input  wire [   0:0] memreq_b_val,
  output wire [  46:0] memresp_a_msg,
  input  wire [   0:0] memresp_a_rdy,
  output wire [   0:0] memresp_a_val,
  output wire [  46:0] memresp_b_msg,
  input  wire [   0:0] memresp_b_rdy,
  output wire [   0:0] memresp_b_val,
  input  wire [   0:0] reset
);

  // wire declarations
  wire   [  31:0] bram_a_rdata_M1;
  wire   [   0:0] memresp_b_queue_rdy;
  wire   [  31:0] memreq_a_msg_reg_out_addr;
  wire   [  31:0] bram_b_rdata_M1;
  wire   [   0:0] memresp_a_queue_rdy;
  wire   [  31:0] memreq_b_msg_reg_out_addr;


  // register declarations
  reg    [  31:0] bram_a_addr_32_M0;
  reg    [   4:0] bram_a_addr_M0;
  reg    [   0:0] bram_a_en_M0;
  reg    [  31:0] bram_a_wdata_M0;
  reg    [   0:0] bram_a_wen_M0;
  reg    [  31:0] bram_b_addr_32_M0;
  reg    [   4:0] bram_b_addr_M0;
  reg    [   0:0] bram_b_en_M0;
  reg    [   0:0] memreq_a_go_M0;
  reg    [   0:0] memreq_b_go_M0;
  reg    [  31:0] memresp_a_msg_data_M1;
  reg    [  31:0] memresp_b_msg_data_M1;

  // localparam declarations
  localparam addr_end = 7;
  localparam addr_start = 2;

  // memreq_b_val_reg temporaries
  wire   [   0:0] memreq_b_val_reg$reset;
  wire   [   0:0] memreq_b_val_reg$in_;
  wire   [   0:0] memreq_b_val_reg$clk;
  wire   [   0:0] memreq_b_val_reg$out;

  RegRst_0x2ce052f8c32c5c39 memreq_b_val_reg
  (
    .reset ( memreq_b_val_reg$reset ),
    .in_   ( memreq_b_val_reg$in_ ),
    .clk   ( memreq_b_val_reg$clk ),
    .out   ( memreq_b_val_reg$out )
  );

  // memresp_a_queue temporaries
  wire   [   0:0] memresp_a_queue$clk;
  wire   [  46:0] memresp_a_queue$enq_msg;
  wire   [   0:0] memresp_a_queue$enq_val;
  wire   [   0:0] memresp_a_queue$reset;
  wire   [   0:0] memresp_a_queue$deq_rdy;
  wire   [   0:0] memresp_a_queue$enq_rdy;
  wire   [   0:0] memresp_a_queue$empty;
  wire   [   0:0] memresp_a_queue$full;
  wire   [  46:0] memresp_a_queue$deq_msg;
  wire   [   0:0] memresp_a_queue$deq_val;

  TwoElementBypassQueue_0x54ac45b19128a76b memresp_a_queue
  (
    .clk     ( memresp_a_queue$clk ),
    .enq_msg ( memresp_a_queue$enq_msg ),
    .enq_val ( memresp_a_queue$enq_val ),
    .reset   ( memresp_a_queue$reset ),
    .deq_rdy ( memresp_a_queue$deq_rdy ),
    .enq_rdy ( memresp_a_queue$enq_rdy ),
    .empty   ( memresp_a_queue$empty ),
    .full    ( memresp_a_queue$full ),
    .deq_msg ( memresp_a_queue$deq_msg ),
    .deq_val ( memresp_a_queue$deq_val )
  );

  // memreq_a_val_reg temporaries
  wire   [   0:0] memreq_a_val_reg$reset;
  wire   [   0:0] memreq_a_val_reg$in_;
  wire   [   0:0] memreq_a_val_reg$clk;
  wire   [   0:0] memreq_a_val_reg$out;

  RegRst_0x2ce052f8c32c5c39 memreq_a_val_reg
  (
    .reset ( memreq_a_val_reg$reset ),
    .in_   ( memreq_a_val_reg$in_ ),
    .clk   ( memreq_a_val_reg$clk ),
    .out   ( memreq_a_val_reg$out )
  );

  // memreq_a_msg_reg temporaries
  wire   [   0:0] memreq_a_msg_reg$reset;
  wire   [  76:0] memreq_a_msg_reg$in_;
  wire   [   0:0] memreq_a_msg_reg$clk;
  wire   [  76:0] memreq_a_msg_reg$out;

  RegRst_0x252e3010658927d3 memreq_a_msg_reg
  (
    .reset ( memreq_a_msg_reg$reset ),
    .in_   ( memreq_a_msg_reg$in_ ),
    .clk   ( memreq_a_msg_reg$clk ),
    .out   ( memreq_a_msg_reg$out )
  );

  // memresp_b_queue temporaries
  wire   [   0:0] memresp_b_queue$clk;
  wire   [  46:0] memresp_b_queue$enq_msg;
  wire   [   0:0] memresp_b_queue$enq_val;
  wire   [   0:0] memresp_b_queue$reset;
  wire   [   0:0] memresp_b_queue$deq_rdy;
  wire   [   0:0] memresp_b_queue$enq_rdy;
  wire   [   0:0] memresp_b_queue$empty;
  wire   [   0:0] memresp_b_queue$full;
  wire   [  46:0] memresp_b_queue$deq_msg;
  wire   [   0:0] memresp_b_queue$deq_val;

  TwoElementBypassQueue_0x54ac45b19128a76b memresp_b_queue
  (
    .clk     ( memresp_b_queue$clk ),
    .enq_msg ( memresp_b_queue$enq_msg ),
    .enq_val ( memresp_b_queue$enq_val ),
    .reset   ( memresp_b_queue$reset ),
    .deq_rdy ( memresp_b_queue$deq_rdy ),
    .enq_rdy ( memresp_b_queue$enq_rdy ),
    .empty   ( memresp_b_queue$empty ),
    .full    ( memresp_b_queue$full ),
    .deq_msg ( memresp_b_queue$deq_msg ),
    .deq_val ( memresp_b_queue$deq_val )
  );

  // memreq_b_msg_reg temporaries
  wire   [   0:0] memreq_b_msg_reg$reset;
  wire   [  76:0] memreq_b_msg_reg$in_;
  wire   [   0:0] memreq_b_msg_reg$clk;
  wire   [  76:0] memreq_b_msg_reg$out;

  RegRst_0x252e3010658927d3 memreq_b_msg_reg
  (
    .reset ( memreq_b_msg_reg$reset ),
    .in_   ( memreq_b_msg_reg$in_ ),
    .clk   ( memreq_b_msg_reg$clk ),
    .out   ( memreq_b_msg_reg$out )
  );

  // bram temporaries
  wire   [   0:0] bram$ena;
  wire   [   0:0] bram$enb;
  wire   [   0:0] bram$clk;
  wire   [   4:0] bram$addrb;
  wire   [   4:0] bram$addra;
  wire   [   0:0] bram$wea;
  wire   [   0:0] bram$reset;
  wire   [  31:0] bram$dia;
  wire   [  31:0] bram$dob;
  wire   [  31:0] bram$doa;

  DPBramRTL_0x7d90234ced63a656 bram
  (
    .ena   ( bram$ena ),
    .enb   ( bram$enb ),
    .clk   ( bram$clk ),
    .addrb ( bram$addrb ),
    .addra ( bram$addra ),
    .wea   ( bram$wea ),
    .reset ( bram$reset ),
    .dia   ( bram$dia ),
    .dob   ( bram$dob ),
    .doa   ( bram$doa )
  );

  // signal connections
  assign bram$addra                     = bram_a_addr_M0;
  assign bram$addrb                     = bram_b_addr_M0;
  assign bram$clk                       = clk;
  assign bram$dia                       = bram_a_wdata_M0;
  assign bram$ena                       = bram_a_en_M0;
  assign bram$enb                       = bram_b_en_M0;
  assign bram$reset                     = reset;
  assign bram$wea                       = bram_a_wen_M0;
  assign bram_a_rdata_M1                = bram$doa;
  assign bram_b_rdata_M1                = bram$dob;
  assign memreq_a_msg_reg$clk           = clk;
  assign memreq_a_msg_reg$in_           = memreq_a_msg;
  assign memreq_a_msg_reg$reset         = reset;
  assign memreq_a_rdy                   = memresp_a_queue$empty;
  assign memreq_a_val_reg$clk           = clk;
  assign memreq_a_val_reg$in_           = memreq_a_go_M0;
  assign memreq_a_val_reg$reset         = reset;
  assign memreq_b_msg_reg$clk           = clk;
  assign memreq_b_msg_reg$in_           = memreq_b_msg;
  assign memreq_b_msg_reg$reset         = reset;
  assign memreq_b_rdy                   = memresp_b_queue$empty;
  assign memreq_b_val_reg$clk           = clk;
  assign memreq_b_val_reg$in_           = memreq_b_go_M0;
  assign memreq_b_val_reg$reset         = reset;
  assign memresp_a_msg                  = memresp_a_queue$deq_msg;
  assign memresp_a_queue$clk            = clk;
  assign memresp_a_queue$deq_rdy        = memresp_a_rdy;
  assign memresp_a_queue$enq_msg[31:0]  = memresp_a_msg_data_M1;
  assign memresp_a_queue$enq_msg[33:32] = memreq_a_msg_reg$out[33:32];
  assign memresp_a_queue$enq_msg[35:34] = 2'd0;
  assign memresp_a_queue$enq_msg[43:36] = memreq_a_msg_reg$out[73:66];
  assign memresp_a_queue$enq_msg[46:44] = memreq_a_msg_reg$out[76:74];
  assign memresp_a_queue$enq_val        = memreq_a_val_reg$out;
  assign memresp_a_queue$reset          = reset;
  assign memresp_a_queue_rdy            = memresp_a_queue$enq_rdy;
  assign memresp_a_val                  = memresp_a_queue$deq_val;
  assign memresp_b_msg                  = memresp_b_queue$deq_msg;
  assign memresp_b_queue$clk            = clk;
  assign memresp_b_queue$deq_rdy        = memresp_b_rdy;
  assign memresp_b_queue$enq_msg[31:0]  = memresp_b_msg_data_M1;
  assign memresp_b_queue$enq_msg[33:32] = memreq_b_msg_reg$out[33:32];
  assign memresp_b_queue$enq_msg[35:34] = 2'd0;
  assign memresp_b_queue$enq_msg[43:36] = memreq_b_msg_reg$out[73:66];
  assign memresp_b_queue$enq_msg[46:44] = memreq_b_msg_reg$out[76:74];
  assign memresp_b_queue$enq_val        = memreq_b_val_reg$out;
  assign memresp_b_queue$reset          = reset;
  assign memresp_b_queue_rdy            = memresp_b_queue$enq_rdy;
  assign memresp_b_val                  = memresp_b_queue$deq_val;


  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_M0():
  //       s.memreq_a_go_M0.value      = s.memreq_a.val & s.memreq_a.rdy
  //       s.memreq_b_go_M0.value      = s.memreq_b.val & s.memreq_b.rdy
  //       s.bram_a_addr_32_M0.value = s.memreq_a.msg.addr # make it translatable
  //       s.bram_b_addr_32_M0.value = s.memreq_b.msg.addr # make it translatable
  //       s.bram_a_addr_M0.value    = s.bram_a_addr_32_M0[addr_start:addr_end]
  //       s.bram_b_addr_M0.value    = s.bram_b_addr_32_M0[addr_start:addr_end]
  //       s.bram_a_wen_M0.value     = s.memreq_a.val & ( s.memreq_a.msg.type_ == 1 )
  //       s.bram_a_en_M0.value      = s.memreq_a_go_M0
  //       s.bram_b_en_M0.value      = s.memreq_b_go_M0
  //       s.bram_a_wdata_M0.value   = s.memreq_a.msg.data

  // logic for comb_M0()
  always @ (*) begin
    memreq_a_go_M0 = (memreq_a_val&memreq_a_rdy);
    memreq_b_go_M0 = (memreq_b_val&memreq_b_rdy);
    bram_a_addr_32_M0 = memreq_a_msg[(66)-1:34];
    bram_b_addr_32_M0 = memreq_b_msg[(66)-1:34];
    bram_a_addr_M0 = bram_a_addr_32_M0[(addr_end)-1:addr_start];
    bram_b_addr_M0 = bram_b_addr_32_M0[(addr_end)-1:addr_start];
    bram_a_wen_M0 = (memreq_a_val&(memreq_a_msg[(77)-1:74] == 1));
    bram_a_en_M0 = memreq_a_go_M0;
    bram_b_en_M0 = memreq_b_go_M0;
    bram_a_wdata_M0 = memreq_a_msg[(32)-1:0];
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_M1():
  //       # zero out data if request is a write
  //       if s.memreq_a_msg_reg.out.type_ == 0:
  //         s.memresp_a_msg_data_M1.value = s.bram_a_rdata_M1
  //       else:
  //         s.memresp_a_msg_data_M1.value = 0
  //       if s.memreq_b_msg_reg.out.type_ == 0:
  //         s.memresp_b_msg_data_M1.value = s.bram_b_rdata_M1
  //       else:
  //         s.memresp_b_msg_data_M1.value = 0

  // logic for comb_M1()
  always @ (*) begin
    if ((memreq_a_msg_reg$out[(77)-1:74] == 0)) begin
      memresp_a_msg_data_M1 = bram_a_rdata_M1;
    end
    else begin
      memresp_a_msg_data_M1 = 0;
    end
    if ((memreq_b_msg_reg$out[(77)-1:74] == 0)) begin
      memresp_b_msg_data_M1 = bram_b_rdata_M1;
    end
    else begin
      memresp_b_msg_data_M1 = 0;
    end
  end


endmodule // DPBramValRdyRTL
`default_nettype wire

//-----------------------------------------------------------------------------
// RegRst_0x2ce052f8c32c5c39
//-----------------------------------------------------------------------------
// dtype: 1
// reset_value: 0
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module RegRst_0x2ce052f8c32c5c39
(
  input  wire [   0:0] clk,
  input  wire [   0:0] in_,
  output reg  [   0:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam reset_value = 0;



  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def seq_logic():
  //       if s.reset:
  //         s.out.next = reset_value
  //       else:
  //         s.out.next = s.in_

  // logic for seq_logic()
  always @ (posedge clk) begin
    if (reset) begin
      out <= reset_value;
    end
    else begin
      out <= in_;
    end
  end


endmodule // RegRst_0x2ce052f8c32c5c39
`default_nettype wire

//-----------------------------------------------------------------------------
// TwoElementBypassQueue_0x54ac45b19128a76b
//-----------------------------------------------------------------------------
// dtype: 47
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module TwoElementBypassQueue_0x54ac45b19128a76b
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

  SingleElementBypassQueue_0x54ac45b19128a76b queue1
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

  SingleElementBypassQueue_0x54ac45b19128a76b queue0
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


  // PYMTL SOURCE:
  //
  // @s.combinational
  // def full_empty():
  //       s.full.value  = s.queue0.full & s.queue1.full
  //       s.empty.value = (~s.queue0.full) & (~s.queue1.full)

  // logic for full_empty()
  always @ (*) begin
    full = (queue0$full&queue1$full);
    empty = (~queue0$full&~queue1$full);
  end


endmodule // TwoElementBypassQueue_0x54ac45b19128a76b
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueue_0x54ac45b19128a76b
//-----------------------------------------------------------------------------
// dtype: 47
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueue_0x54ac45b19128a76b
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

  SingleElementBypassQueueCtrl_0x2a979dc5ff91cb88 ctrl
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

  SingleElementBypassQueueDpath_0x54ac45b19128a76b dpath
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



endmodule // SingleElementBypassQueue_0x54ac45b19128a76b
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueueCtrl_0x2a979dc5ff91cb88
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueueCtrl_0x2a979dc5ff91cb88
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


endmodule // SingleElementBypassQueueCtrl_0x2a979dc5ff91cb88
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueueDpath_0x54ac45b19128a76b
//-----------------------------------------------------------------------------
// dtype: 47
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueueDpath_0x54ac45b19128a76b
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

  Mux_0x7a68080d896dd935 bypass_mux
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

  RegEn_0x4f74ec4a24e8c2c0 queue
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



endmodule // SingleElementBypassQueueDpath_0x54ac45b19128a76b
`default_nettype wire

//-----------------------------------------------------------------------------
// Mux_0x7a68080d896dd935
//-----------------------------------------------------------------------------
// dtype: 47
// nports: 2
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module Mux_0x7a68080d896dd935
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

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       assert s.sel < nports
  //       s.out.v = s.in_[ s.sel ]

  // logic for comb_logic()
  always @ (*) begin
    out = in_[sel];
  end


endmodule // Mux_0x7a68080d896dd935
`default_nettype wire

//-----------------------------------------------------------------------------
// RegEn_0x4f74ec4a24e8c2c0
//-----------------------------------------------------------------------------
// dtype: 47
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module RegEn_0x4f74ec4a24e8c2c0
(
  input  wire [   0:0] clk,
  input  wire [   0:0] en,
  input  wire [  46:0] in_,
  output reg  [  46:0] out,
  input  wire [   0:0] reset
);



  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def seq_logic():
  //       if s.en:
  //         s.out.next = s.in_

  // logic for seq_logic()
  always @ (posedge clk) begin
    if (en) begin
      out <= in_;
    end
    else begin
    end
  end


endmodule // RegEn_0x4f74ec4a24e8c2c0
`default_nettype wire

//-----------------------------------------------------------------------------
// RegRst_0x252e3010658927d3
//-----------------------------------------------------------------------------
// dtype: 77
// reset_value: 0
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module RegRst_0x252e3010658927d3
(
  input  wire [   0:0] clk,
  input  wire [  76:0] in_,
  output reg  [  76:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam reset_value = 0;



  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def seq_logic():
  //       if s.reset:
  //         s.out.next = reset_value
  //       else:
  //         s.out.next = s.in_

  // logic for seq_logic()
  always @ (posedge clk) begin
    if (reset) begin
      out <= reset_value;
    end
    else begin
      out <= in_;
    end
  end


endmodule // RegRst_0x252e3010658927d3
`default_nettype wire

//-----------------------------------------------------------------------------
// DPBramRTL_0x7d90234ced63a656
//-----------------------------------------------------------------------------
// dump-vcd: True
// verilator-xinit: zeros
`default_nettype none
module DPBramRTL_0x7d90234ced63a656
(
  input  wire [   4:0] addra,
  input  wire [   4:0] addrb,
  input  wire [   0:0] clk,
  input  wire [  31:0] dia,
  output wire [  31:0] doa,
  output wire [  31:0] dob,
  input  wire [   0:0] ena,
  input  wire [   0:0] enb,
  input  wire [   0:0] reset,
  input  wire [   0:0] wea
);

  // Imported Verilog source from:
  // /home/dg524/RISC-V-CGRA/pymtl-rocket/sim/proc_teach/DPBramRTL.v

  DPBramRTL#(

  )  verilog_module
  (
    .addra ( addra ),
    .addrb ( addrb ),
    .clk   ( clk ),
    .dia   ( dia ),
    .doa   ( doa ),
    .dob   ( dob ),
    .ena   ( ena ),
    .enb   ( enb ),
    .wea   ( wea )
  );

endmodule // DPBramRTL_0x7d90234ced63a656
`default_nettype wire

//
// Dual Port RAM
//

module DPBramRTL
(
  input  logic        clk,
  input  logic        ena,
  input  logic        enb,
  input  logic        wea,
  input  logic [4:0]  addra,
  input  logic [4:0]  addrb,
  input  logic [31:0] dia,
  output  logic [31:0] doa,
  output  logic [31:0] dob
);

  logic  [31:0] ram [31:0];
  logic  [4:0] read_addra, read_addrb;;

  always @(posedge clk) begin
    if (ena) begin
      if (wea)  ram[addra] <= dia;
      read_addra <= addra;
    end
    if (enb) begin
      read_addrb <= addrb;
    end
  end

  assign doa = ram[read_addra];
  assign dob = ram[read_addrb];

endmodule

//-----------------------------------------------------------------------------
// ProcRTL
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros

`default_nettype none
module Verilog_ProcRTL
(
  input  wire [   0:0] clk,
  output wire [   0:0] commit_inst,
  output wire [  76:0] dmemreq_msg,
  input  wire [   0:0] dmemreq_rdy,
  output wire [   0:0] dmemreq_val,
  input  wire [  46:0] dmemresp_msg,
  output wire [   0:0] dmemresp_rdy,
  input  wire [   0:0] dmemresp_val,
  input  wire [   0:0] go,
  output wire [  76:0] imemreq_msg,
  input  wire [   0:0] imemreq_rdy,
  output wire [   0:0] imemreq_val,
  input  wire [  46:0] imemresp_msg,
  output wire [   0:0] imemresp_rdy,
  input  wire [   0:0] imemresp_val,
  input  wire [  31:0] mngr2proc_msg,
  output wire [   0:0] mngr2proc_rdy,
  input  wire [   0:0] mngr2proc_val,
  output wire [  31:0] proc2mngr_msg,
  input  wire [   0:0] proc2mngr_rdy,
  output wire [   0:0] proc2mngr_val,
  input  wire [   0:0] reset,
  output wire [   0:0] stats_en
);

  // wire declarations
  wire   [   0:0] imemresp_drop;

  assign dmemreq_msg[73:66] = 8'd0; //opaque
  assign dmemreq_msg[33:32] = 2'd0;  // len

  // ctrl temporaries
  wire   [   0:0] ctrl$go;
  wire   [   0:0] ctrl$clk;
  wire   [   0:0] ctrl$imemresp_val;
  wire   [   0:0] ctrl$proc2mngr_rdy;
  wire   [   0:0] ctrl$br_cond_ltu_X;
  wire   [   0:0] ctrl$mngr2proc_val;
  wire   [   0:0] ctrl$mul_resp_val_X;
  wire   [   0:0] ctrl$br_cond_neg_X;
  wire   [   0:0] ctrl$br_cond_zero_X;
  wire   [  31:0] ctrl$inst_D;
  wire   [   0:0] ctrl$dmemreq_rdy;
  wire   [   0:0] ctrl$imemreq_rdy;
  wire   [   0:0] ctrl$mul_req_rdy_D;
  wire   [   0:0] ctrl$reset;
  wire   [   0:0] ctrl$br_cond_lt_X;
  wire   [   0:0] ctrl$dmemresp_val;
  wire   [   0:0] ctrl$br_cond_eq_X;
  wire   [   0:0] ctrl$br_cond_ge_X;
  wire   [   0:0] ctrl$br_cond_geu_X;
  wire   [   0:0] ctrl$dmemreq_val;
  wire   [   0:0] ctrl$mngr2proc_rdy;
  wire   [   1:0] ctrl$op1_byp_sel_D;
  wire   [   0:0] ctrl$dmemresp_rdy;
  wire   [   0:0] ctrl$reg_en_X;
  wire   [   0:0] ctrl$mul_req_val_D;
  wire   [   0:0] ctrl$wb_result_sel_M;
  wire   [   0:0] ctrl$reg_en_D;
  wire   [   0:0] ctrl$reg_en_F;
  wire   [   0:0] ctrl$reg_en_M;
  wire   [   0:0] ctrl$reg_en_W;
  wire   [   3:0] ctrl$alu_fn_X;
  wire   [   0:0] ctrl$ex_result_sel_X;
  wire   [   1:0] ctrl$op0_sel_D;
  wire   [   0:0] ctrl$rf_wen_W;
  wire   [   2:0] ctrl$dmemreq_msg_type;
  wire   [   4:0] ctrl$rf_waddr_W;
  wire   [   0:0] ctrl$stats_en_wen_W;
  wire   [   1:0] ctrl$pc_sel_F;
  wire   [   0:0] ctrl$proc2mngr_val;
  wire   [   0:0] ctrl$commit_inst;
  wire   [   0:0] ctrl$imemreq_val;
  wire   [   3:0] ctrl$op1_sel_D;
  wire   [   1:0] ctrl$op0_byp_sel_D;
  wire   [   0:0] ctrl$imemresp_rdy;
  wire   [   0:0] ctrl$imemresp_drop;
  wire   [   0:0] ctrl$mul_resp_rdy_X;

  ProcCtrlRTL_0x7340ba89deb7e746 ctrl
  (
    .go               ( ctrl$go ),
    .clk              ( ctrl$clk ),
    .imemresp_val     ( ctrl$imemresp_val ),
    .proc2mngr_rdy    ( ctrl$proc2mngr_rdy ),
    .br_cond_ltu_X    ( ctrl$br_cond_ltu_X ),
    .mngr2proc_val    ( ctrl$mngr2proc_val ),
    .mul_resp_val_X   ( ctrl$mul_resp_val_X ),
    .br_cond_neg_X    ( ctrl$br_cond_neg_X ),
    .br_cond_zero_X   ( ctrl$br_cond_zero_X ),
    .inst_D           ( ctrl$inst_D ),
    .dmemreq_rdy      ( ctrl$dmemreq_rdy ),
    .imemreq_rdy      ( ctrl$imemreq_rdy ),
    .mul_req_rdy_D    ( ctrl$mul_req_rdy_D ),
    .reset            ( ctrl$reset ),
    .br_cond_lt_X     ( ctrl$br_cond_lt_X ),
    .dmemresp_val     ( ctrl$dmemresp_val ),
    .br_cond_eq_X     ( ctrl$br_cond_eq_X ),
    .br_cond_ge_X     ( ctrl$br_cond_ge_X ),
    .br_cond_geu_X    ( ctrl$br_cond_geu_X ),
    .dmemreq_val      ( ctrl$dmemreq_val ),
    .mngr2proc_rdy    ( ctrl$mngr2proc_rdy ),
    .op1_byp_sel_D    ( ctrl$op1_byp_sel_D ),
    .dmemresp_rdy     ( ctrl$dmemresp_rdy ),
    .reg_en_X         ( ctrl$reg_en_X ),
    .mul_req_val_D    ( ctrl$mul_req_val_D ),
    .wb_result_sel_M  ( ctrl$wb_result_sel_M ),
    .reg_en_D         ( ctrl$reg_en_D ),
    .reg_en_F         ( ctrl$reg_en_F ),
    .reg_en_M         ( ctrl$reg_en_M ),
    .reg_en_W         ( ctrl$reg_en_W ),
    .alu_fn_X         ( ctrl$alu_fn_X ),
    .ex_result_sel_X  ( ctrl$ex_result_sel_X ),
    .op0_sel_D        ( ctrl$op0_sel_D ),
    .rf_wen_W         ( ctrl$rf_wen_W ),
    .dmemreq_msg_type ( ctrl$dmemreq_msg_type ),
    .rf_waddr_W       ( ctrl$rf_waddr_W ),
    .stats_en_wen_W   ( ctrl$stats_en_wen_W ),
    .pc_sel_F         ( ctrl$pc_sel_F ),
    .proc2mngr_val    ( ctrl$proc2mngr_val ),
    .commit_inst      ( ctrl$commit_inst ),
    .imemreq_val      ( ctrl$imemreq_val ),
    .op1_sel_D        ( ctrl$op1_sel_D ),
    .op0_byp_sel_D    ( ctrl$op0_byp_sel_D ),
    .imemresp_rdy     ( ctrl$imemresp_rdy ),
    .imemresp_drop    ( ctrl$imemresp_drop ),
    .mul_resp_rdy_X   ( ctrl$mul_resp_rdy_X )
  );

  // imemresp_drop_unit temporaries
  wire   [   0:0] imemresp_drop_unit$reset;
  wire   [  31:0] imemresp_drop_unit$in__msg;
  wire   [   0:0] imemresp_drop_unit$in__val;
  wire   [   0:0] imemresp_drop_unit$clk;
  wire   [   0:0] imemresp_drop_unit$drop;
  wire   [   0:0] imemresp_drop_unit$out_rdy;
  wire   [   0:0] imemresp_drop_unit$in__rdy;
  wire   [  31:0] imemresp_drop_unit$out_msg;
  wire   [   0:0] imemresp_drop_unit$out_val;

  DropUnitRTL_0x56bc1346a6f0c780 imemresp_drop_unit
  (
    .reset   ( imemresp_drop_unit$reset ),
    .in__msg ( imemresp_drop_unit$in__msg ),
    .in__val ( imemresp_drop_unit$in__val ),
    .clk     ( imemresp_drop_unit$clk ),
    .drop    ( imemresp_drop_unit$drop ),
    .out_rdy ( imemresp_drop_unit$out_rdy ),
    .in__rdy ( imemresp_drop_unit$in__rdy ),
    .out_msg ( imemresp_drop_unit$out_msg ),
    .out_val ( imemresp_drop_unit$out_val )
  );

  // dmemresp_queue temporaries
  wire   [   0:0] dmemresp_queue$clk;
  wire   [  46:0] dmemresp_queue$enq_msg;
  wire   [   0:0] dmemresp_queue$enq_val;
  wire   [   0:0] dmemresp_queue$reset;
  wire   [   0:0] dmemresp_queue$deq_rdy;
  wire   [   0:0] dmemresp_queue$enq_rdy;
  wire   [   0:0] dmemresp_queue$full;
  wire   [  46:0] dmemresp_queue$deq_msg;
  wire   [   0:0] dmemresp_queue$deq_val;

  SingleElementBypassQueue dmemresp_queue
  (
    .clk     ( dmemresp_queue$clk ),
    .enq_msg ( dmemresp_queue$enq_msg ),
    .enq_val ( dmemresp_queue$enq_val ),
    .reset   ( dmemresp_queue$reset ),
    .deq_rdy ( dmemresp_queue$deq_rdy ),
    .enq_rdy ( dmemresp_queue$enq_rdy ),
    .full    ( dmemresp_queue$full ),
    .deq_msg ( dmemresp_queue$deq_msg ),
    .deq_val ( dmemresp_queue$deq_val )
  );

  // mngr2proc_queue temporaries
  wire   [   0:0] mngr2proc_queue$clk;
  wire   [  31:0] mngr2proc_queue$enq_msg;
  wire   [   0:0] mngr2proc_queue$enq_val;
  wire   [   0:0] mngr2proc_queue$reset;
  wire   [   0:0] mngr2proc_queue$deq_rdy;
  wire   [   0:0] mngr2proc_queue$enq_rdy;
  wire   [   0:0] mngr2proc_queue$full;
  wire   [  31:0] mngr2proc_queue$deq_msg;
  wire   [   0:0] mngr2proc_queue$deq_val;

  SingleElementBypassQueue_0x4c19e633b920d596 mngr2proc_queue
  (
    .clk     ( mngr2proc_queue$clk ),
    .enq_msg ( mngr2proc_queue$enq_msg ),
    .enq_val ( mngr2proc_queue$enq_val ),
    .reset   ( mngr2proc_queue$reset ),
    .deq_rdy ( mngr2proc_queue$deq_rdy ),
    .enq_rdy ( mngr2proc_queue$enq_rdy ),
    .full    ( mngr2proc_queue$full ),
    .deq_msg ( mngr2proc_queue$deq_msg ),
    .deq_val ( mngr2proc_queue$deq_val )
  );

  // dpath temporaries
  wire   [  31:0] dpath$mngr2proc_data;
  wire   [   0:0] dpath$clk;
  wire   [   0:0] dpath$reg_en_W;
  wire   [   1:0] dpath$op1_byp_sel_D;
  wire   [   0:0] dpath$reg_en_X;
  wire   [   0:0] dpath$mul_req_val_D;
  wire   [   0:0] dpath$wb_result_sel_M;
  wire   [   0:0] dpath$reg_en_D;
  wire   [   0:0] dpath$reg_en_F;
  wire   [   0:0] dpath$reg_en_M;
  wire   [   3:0] dpath$alu_fn_X;
  wire   [   0:0] dpath$ex_result_sel_X;
  wire   [   1:0] dpath$op0_sel_D;
  wire   [   0:0] dpath$rf_wen_W;
  wire   [   4:0] dpath$rf_waddr_W;
  wire   [   0:0] dpath$stats_en_wen_W;
  wire   [   0:0] dpath$reset;
  wire   [   1:0] dpath$pc_sel_F;
  wire   [   3:0] dpath$op1_sel_D;
  wire   [   1:0] dpath$op0_byp_sel_D;
  wire   [  31:0] dpath$imemresp_msg_data;
  wire   [  31:0] dpath$dmemresp_msg_data;
  wire   [   0:0] dpath$mul_resp_rdy_X;
  wire   [   0:0] dpath$stats_en;
  wire   [   0:0] dpath$br_cond_ltu_X;
  wire   [  31:0] dpath$dmemreq_msg_data;
  wire   [   0:0] dpath$mul_resp_val_X;
  wire   [  31:0] dpath$proc2mngr_data;
  wire   [   0:0] dpath$br_cond_neg_X;
  wire   [   0:0] dpath$br_cond_zero_X;
  wire   [  31:0] dpath$inst_D;
  wire   [   0:0] dpath$mul_req_rdy_D;
  wire   [  31:0] dpath$dmemreq_msg_addr;
  wire   [   0:0] dpath$br_cond_lt_X;
  wire   [   0:0] dpath$br_cond_eq_X;
  wire   [  76:0] dpath$imemreq_msg;

  ProcDpathRTL_0x5941394665257d0d dpath
  (
    .mngr2proc_data    ( dpath$mngr2proc_data ),
    .clk               ( dpath$clk ),
    .reg_en_W          ( dpath$reg_en_W ),
    .op1_byp_sel_D     ( dpath$op1_byp_sel_D ),
    .reg_en_X          ( dpath$reg_en_X ),
    .mul_req_val_D     ( dpath$mul_req_val_D ),
    .wb_result_sel_M   ( dpath$wb_result_sel_M ),
    .reg_en_D          ( dpath$reg_en_D ),
    .reg_en_F          ( dpath$reg_en_F ),
    .reg_en_M          ( dpath$reg_en_M ),
    .alu_fn_X          ( dpath$alu_fn_X ),
    .ex_result_sel_X   ( dpath$ex_result_sel_X ),
    .op0_sel_D         ( dpath$op0_sel_D ),
    .rf_wen_W          ( dpath$rf_wen_W ),
    .rf_waddr_W        ( dpath$rf_waddr_W ),
    .stats_en_wen_W    ( dpath$stats_en_wen_W ),
    .reset             ( dpath$reset ),
    .pc_sel_F          ( dpath$pc_sel_F ),
    .op1_sel_D         ( dpath$op1_sel_D ),
    .op0_byp_sel_D     ( dpath$op0_byp_sel_D ),
    .imemresp_msg_data ( dpath$imemresp_msg_data ),
    .dmemresp_msg_data ( dpath$dmemresp_msg_data ),
    .mul_resp_rdy_X    ( dpath$mul_resp_rdy_X ),
    .stats_en          ( dpath$stats_en ),
    .br_cond_ltu_X     ( dpath$br_cond_ltu_X ),
    .dmemreq_msg_data  ( dpath$dmemreq_msg_data ),
    .mul_resp_val_X    ( dpath$mul_resp_val_X ),
    .proc2mngr_data    ( dpath$proc2mngr_data ),
    .br_cond_neg_X     ( dpath$br_cond_neg_X ),
    .br_cond_zero_X    ( dpath$br_cond_zero_X ),
    .inst_D            ( dpath$inst_D ),
    .mul_req_rdy_D     ( dpath$mul_req_rdy_D ),
    .dmemreq_msg_addr  ( dpath$dmemreq_msg_addr ),
    .br_cond_lt_X      ( dpath$br_cond_lt_X ),
    .br_cond_eq_X      ( dpath$br_cond_eq_X ),
    .imemreq_msg       ( dpath$imemreq_msg )
  );

  // imemreq_queue temporaries
  wire   [   0:0] imemreq_queue$clk;
  wire   [  76:0] imemreq_queue$enq_msg;
  wire   [   0:0] imemreq_queue$enq_val;
  wire   [   0:0] imemreq_queue$reset;
  wire   [   0:0] imemreq_queue$deq_rdy;
  wire   [   0:0] imemreq_queue$enq_rdy;
  wire   [   0:0] imemreq_queue$empty;
  wire   [   0:0] imemreq_queue$full;
  wire   [  76:0] imemreq_queue$deq_msg;
  wire   [   0:0] imemreq_queue$deq_val;

  TwoElementBypassQueue_0x4b339266d56df875 imemreq_queue
  (
    .clk     ( imemreq_queue$clk ),
    .enq_msg ( imemreq_queue$enq_msg ),
    .enq_val ( imemreq_queue$enq_val ),
    .reset   ( imemreq_queue$reset ),
    .deq_rdy ( imemreq_queue$deq_rdy ),
    .enq_rdy ( imemreq_queue$enq_rdy ),
    .empty   ( imemreq_queue$empty ),
    .full    ( imemreq_queue$full ),
    .deq_msg ( imemreq_queue$deq_msg ),
    .deq_val ( imemreq_queue$deq_val )
  );

  // proc2mngr_queue temporaries
  wire   [   0:0] proc2mngr_queue$clk;
  wire   [  31:0] proc2mngr_queue$enq_msg;
  wire   [   0:0] proc2mngr_queue$enq_val;
  wire   [   0:0] proc2mngr_queue$reset;
  wire   [   0:0] proc2mngr_queue$deq_rdy;
  wire   [   0:0] proc2mngr_queue$enq_rdy;
  wire   [   0:0] proc2mngr_queue$full;
  wire   [  31:0] proc2mngr_queue$deq_msg;
  wire   [   0:0] proc2mngr_queue$deq_val;

  SingleElementBypassQueue_0x4c19e633b920d596 proc2mngr_queue
  (
    .clk     ( proc2mngr_queue$clk ),
    .enq_msg ( proc2mngr_queue$enq_msg ),
    .enq_val ( proc2mngr_queue$enq_val ),
    .reset   ( proc2mngr_queue$reset ),
    .deq_rdy ( proc2mngr_queue$deq_rdy ),
    .enq_rdy ( proc2mngr_queue$enq_rdy ),
    .full    ( proc2mngr_queue$full ),
    .deq_msg ( proc2mngr_queue$deq_msg ),
    .deq_val ( proc2mngr_queue$deq_val )
  );

  // imemresp_queue temporaries
  wire   [   0:0] imemresp_queue$clk;
  wire   [  46:0] imemresp_queue$enq_msg;
  wire   [   0:0] imemresp_queue$enq_val;
  wire   [   0:0] imemresp_queue$reset;
  wire   [   0:0] imemresp_queue$deq_rdy;
  wire   [   0:0] imemresp_queue$enq_rdy;
  wire   [   0:0] imemresp_queue$full;
  wire   [  46:0] imemresp_queue$deq_msg;
  wire   [   0:0] imemresp_queue$deq_val;

  SingleElementBypassQueue imemresp_queue
  (
    .clk     ( imemresp_queue$clk ),
    .enq_msg ( imemresp_queue$enq_msg ),
    .enq_val ( imemresp_queue$enq_val ),
    .reset   ( imemresp_queue$reset ),
    .deq_rdy ( imemresp_queue$deq_rdy ),
    .enq_rdy ( imemresp_queue$enq_rdy ),
    .full    ( imemresp_queue$full ),
    .deq_msg ( imemresp_queue$deq_msg ),
    .deq_val ( imemresp_queue$deq_val )
  );

  // dmemreq_queue temporaries
  wire   [   0:0] dmemreq_queue$clk;
  wire   [  76:0] dmemreq_queue$enq_msg;
  wire   [   0:0] dmemreq_queue$enq_val;
  wire   [   0:0] dmemreq_queue$reset;
  wire   [   0:0] dmemreq_queue$deq_rdy;
  wire   [   0:0] dmemreq_queue$enq_rdy;
  wire   [   0:0] dmemreq_queue$full;
  wire   [  76:0] dmemreq_queue$deq_msg;
  wire   [   0:0] dmemreq_queue$deq_val;

  SingleElementBypassQueue_0x4b339266d56df875 dmemreq_queue
  (
    .clk     ( dmemreq_queue$clk ),
    .enq_msg ( dmemreq_queue$enq_msg ),
    .enq_val ( dmemreq_queue$enq_val ),
    .reset   ( dmemreq_queue$reset ),
    .deq_rdy ( dmemreq_queue$deq_rdy ),
    .enq_rdy ( dmemreq_queue$enq_rdy ),
    .full    ( dmemreq_queue$full ),
    .deq_msg ( dmemreq_queue$deq_msg ),
    .deq_val ( dmemreq_queue$deq_val )
  );

  // signal connections
  assign commit_inst                  = ctrl$commit_inst;
  assign commit_inst                  = ctrl$commit_inst;
  assign ctrl$br_cond_eq_X            = dpath$br_cond_eq_X;
  assign ctrl$br_cond_lt_X            = dpath$br_cond_lt_X;
  assign ctrl$br_cond_ltu_X           = dpath$br_cond_ltu_X;
  assign ctrl$br_cond_neg_X           = dpath$br_cond_neg_X;
  assign ctrl$br_cond_zero_X          = dpath$br_cond_zero_X;
  assign ctrl$clk                     = clk;
  assign ctrl$dmemreq_rdy             = dmemreq_queue$enq_rdy;
  assign ctrl$dmemresp_val            = dmemresp_queue$deq_val;
  assign ctrl$go                      = go;
  assign ctrl$go                      = go;
  assign ctrl$imemreq_rdy             = imemreq_queue$enq_rdy;
  assign ctrl$imemresp_val            = imemresp_drop_unit$out_val;
  assign ctrl$inst_D                  = dpath$inst_D;
  assign ctrl$mngr2proc_val           = mngr2proc_queue$deq_val;
  assign ctrl$mul_req_rdy_D           = dpath$mul_req_rdy_D;
  assign ctrl$mul_resp_val_X          = dpath$mul_resp_val_X;
  assign ctrl$proc2mngr_rdy           = proc2mngr_queue$enq_rdy;
  assign ctrl$reset                   = reset;
  assign dmemreq_msg                  = dmemreq_queue$deq_msg;
  assign dmemreq_queue$clk            = clk;
  assign dmemreq_queue$deq_rdy        = dmemreq_rdy;
  assign dmemreq_queue$enq_msg[31:0]  = dpath$dmemreq_msg_data;
  assign dmemreq_queue$enq_msg[65:34] = dpath$dmemreq_msg_addr;
  assign dmemreq_queue$enq_msg[76:74] = ctrl$dmemreq_msg_type;
  assign dmemreq_queue$enq_val        = ctrl$dmemreq_val;
  assign dmemreq_queue$reset          = reset;
  assign dmemreq_val                  = dmemreq_queue$deq_val;
  assign dmemresp_queue$clk           = clk;
  assign dmemresp_queue$deq_rdy       = ctrl$dmemresp_rdy;
  assign dmemresp_queue$enq_msg       = dmemresp_msg;
  assign dmemresp_queue$enq_val       = dmemresp_val;
  assign dmemresp_queue$reset         = reset;
  assign dmemresp_rdy                 = dmemresp_queue$enq_rdy;
  assign dpath$alu_fn_X               = ctrl$alu_fn_X;
  assign dpath$clk                    = clk;
  assign dpath$dmemresp_msg_data      = dmemresp_queue$deq_msg[31:0];
  assign dpath$ex_result_sel_X        = ctrl$ex_result_sel_X;
  assign dpath$imemresp_msg_data      = imemresp_drop_unit$out_msg;
  assign dpath$mngr2proc_data         = mngr2proc_queue$deq_msg;
  assign dpath$mul_req_val_D          = ctrl$mul_req_val_D;
  assign dpath$mul_resp_rdy_X         = ctrl$mul_resp_rdy_X;
  assign dpath$op0_byp_sel_D          = ctrl$op0_byp_sel_D;
  assign dpath$op0_sel_D              = ctrl$op0_sel_D;
  assign dpath$op1_byp_sel_D          = ctrl$op1_byp_sel_D;
  assign dpath$op1_sel_D              = ctrl$op1_sel_D;
  assign dpath$pc_sel_F               = ctrl$pc_sel_F;
  assign dpath$reg_en_D               = ctrl$reg_en_D;
  assign dpath$reg_en_F               = ctrl$reg_en_F;
  assign dpath$reg_en_M               = ctrl$reg_en_M;
  assign dpath$reg_en_W               = ctrl$reg_en_W;
  assign dpath$reg_en_X               = ctrl$reg_en_X;
  assign dpath$reset                  = reset;
  assign dpath$rf_waddr_W             = ctrl$rf_waddr_W;
  assign dpath$rf_wen_W               = ctrl$rf_wen_W;
  assign dpath$stats_en_wen_W         = ctrl$stats_en_wen_W;
  assign dpath$wb_result_sel_M        = ctrl$wb_result_sel_M;
  assign imemreq_msg                  = imemreq_queue$deq_msg;
  assign imemreq_queue$clk            = clk;
  assign imemreq_queue$deq_rdy        = imemreq_rdy;
  assign imemreq_queue$enq_msg        = dpath$imemreq_msg;
  assign imemreq_queue$enq_val        = ctrl$imemreq_val;
  assign imemreq_queue$reset          = reset;
  assign imemreq_val                  = imemreq_queue$deq_val;
  assign imemresp_drop                = ctrl$imemresp_drop;
  assign imemresp_drop_unit$clk       = clk;
  assign imemresp_drop_unit$drop      = imemresp_drop;
  assign imemresp_drop_unit$in__msg   = imemresp_queue$deq_msg[31:0];
  assign imemresp_drop_unit$in__val   = imemresp_queue$deq_val;
  assign imemresp_drop_unit$out_rdy   = ctrl$imemresp_rdy;
  assign imemresp_drop_unit$reset     = reset;
  assign imemresp_queue$clk           = clk;
  assign imemresp_queue$deq_rdy       = imemresp_drop_unit$in__rdy;
  assign imemresp_queue$enq_msg       = imemresp_msg;
  assign imemresp_queue$enq_val       = imemresp_val;
  assign imemresp_queue$reset         = reset;
  assign imemresp_rdy                 = imemresp_queue$enq_rdy;
  assign mngr2proc_queue$clk          = clk;
  assign mngr2proc_queue$deq_rdy      = ctrl$mngr2proc_rdy;
  assign mngr2proc_queue$enq_msg      = mngr2proc_msg;
  assign mngr2proc_queue$enq_val      = mngr2proc_val;
  assign mngr2proc_queue$reset        = reset;
  assign mngr2proc_rdy                = mngr2proc_queue$enq_rdy;
  assign proc2mngr_msg                = proc2mngr_queue$deq_msg;
  assign proc2mngr_queue$clk          = clk;
  assign proc2mngr_queue$deq_rdy      = proc2mngr_rdy;
  assign proc2mngr_queue$enq_msg      = dpath$proc2mngr_data;
  assign proc2mngr_queue$enq_val      = ctrl$proc2mngr_val;
  assign proc2mngr_queue$reset        = reset;
  assign proc2mngr_val                = proc2mngr_queue$deq_val;
  assign stats_en                     = dpath$stats_en;
  assign stats_en                     = dpath$stats_en;

endmodule // ProcRTL
`default_nettype wire

//-----------------------------------------------------------------------------
// ProcCtrlRTL_0x7340ba89deb7e746
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module ProcCtrlRTL_0x7340ba89deb7e746
(
  output reg  [   3:0] alu_fn_X,
  input  wire [   0:0] br_cond_eq_X,
  input  wire [   0:0] br_cond_ge_X,
  input  wire [   0:0] br_cond_geu_X,
  input  wire [   0:0] br_cond_lt_X,
  input  wire [   0:0] br_cond_ltu_X,
  input  wire [   0:0] br_cond_neg_X,
  input  wire [   0:0] br_cond_zero_X,
  input  wire [   0:0] clk,
  output reg  [   0:0] commit_inst,
  output reg  [   2:0] dmemreq_msg_type,
  input  wire [   0:0] dmemreq_rdy,
  output reg  [   0:0] dmemreq_val,
  output reg  [   0:0] dmemresp_rdy,
  input  wire [   0:0] dmemresp_val,
  output reg  [   0:0] ex_result_sel_X,
  input  wire [   0:0] go,
  input  wire [   0:0] imemreq_rdy,
  output reg  [   0:0] imemreq_val,
  output reg  [   0:0] imemresp_drop,
  output reg  [   0:0] imemresp_rdy,
  input  wire [   0:0] imemresp_val,
  input  wire [  31:0] inst_D,
  output reg  [   0:0] mngr2proc_rdy,
  input  wire [   0:0] mngr2proc_val,
  input  wire [   0:0] mul_req_rdy_D,
  output reg  [   0:0] mul_req_val_D,
  output reg  [   0:0] mul_resp_rdy_X,
  input  wire [   0:0] mul_resp_val_X,
  output reg  [   1:0] op0_byp_sel_D,
  output reg  [   1:0] op0_sel_D,
  output reg  [   1:0] op1_byp_sel_D,
  output reg  [   3:0] op1_sel_D,
  output reg  [   1:0] pc_sel_F,
  input  wire [   0:0] proc2mngr_rdy,
  output reg  [   0:0] proc2mngr_val,
  output reg  [   0:0] reg_en_D,
  output reg  [   0:0] reg_en_F,
  output reg  [   0:0] reg_en_M,
  output reg  [   0:0] reg_en_W,
  output reg  [   0:0] reg_en_X,
  input  wire [   0:0] reset,
  output reg  [   4:0] rf_waddr_W,
  output reg  [   0:0] rf_wen_W,
  output reg  [   0:0] stats_en_wen_W,
  output reg  [   0:0] wb_result_sel_M
);

  // wire declarations
  wire   [   0:0] ostall_waddr_X_rt_D;
  wire   [   0:0] ostall_waddr_W_rs_D;
  wire   [   0:0] ostall_xcel_X;
  wire   [   0:0] ostall_proc2mngr_W;
  wire   [   2:0] rf_waddr_sel_D;
  wire   [   0:0] ostall_waddr_M_rt_D;
  wire   [   0:0] ostall_waddr_W_rt_D;
  wire   [   0:0] ostall_waddr_M_rs_D;
  wire   [   0:0] ostall_waddr_X_rs_D;


  // register declarations
  reg    [   3:0] alu_fn_D;
  reg    [   2:0] br_type_D;
  reg    [   2:0] br_type_X;
  reg    [  26:0] cs_D;
  reg    [   1:0] dmemreq_type_D;
  reg    [   1:0] dmemreq_type_M;
  reg    [   1:0] dmemreq_type_X;
  reg    [   0:0] ex_result_sel_D;
  reg    [   0:0] go_reg_F;
  reg    [  15:0] inst__10;
  reg    [  15:0] inst_type_M;
  reg    [  15:0] inst_type_W;
  reg    [  15:0] inst_type_X;
  reg    [   0:0] inst_val_D;
  reg    [   1:0] j_type_D;
  reg    [   0:0] mngr2proc_rdy_D;
  reg    [   0:0] mul_D;
  reg    [   0:0] mul_X;
  reg    [   0:0] next_val_D;
  reg    [   0:0] next_val_F;
  reg    [   0:0] next_val_M;
  reg    [   0:0] next_val_X;
  reg    [   0:0] osquash_D;
  reg    [   0:0] osquash_X;
  reg    [   0:0] ostall_D;
  reg    [   0:0] ostall_F;
  reg    [   0:0] ostall_M;
  reg    [   0:0] ostall_W;
  reg    [   0:0] ostall_X;
  reg    [   0:0] ostall_dmem_X;
  reg    [   0:0] ostall_hazard_D;
  reg    [   0:0] ostall_load_use_X_rs_D;
  reg    [   0:0] ostall_load_use_X_rt_D;
  reg    [   0:0] ostall_mfx_use_X_rs_D;
  reg    [   0:0] ostall_mfx_use_X_rt_D;
  reg    [   0:0] ostall_mngr_D;
  reg    [   0:0] ostall_mul_D;
  reg    [   0:0] ostall_mul_X;
  reg    [   0:0] pc_redirect_D;
  reg    [   0:0] pc_redirect_X;
  reg    [   1:0] pc_sel_D;
  reg    [   1:0] pc_sel_X;
  reg    [   0:0] proc2mngr_val_D;
  reg    [   0:0] proc2mngr_val_M;
  reg    [   0:0] proc2mngr_val_W;
  reg    [   0:0] proc2mngr_val_X;
  reg    [   4:0] rf_waddr_D;
  reg    [   4:0] rf_waddr_M;
  reg    [   4:0] rf_waddr_X;
  reg    [   0:0] rf_wen_pending_D;
  reg    [   0:0] rf_wen_pending_M;
  reg    [   0:0] rf_wen_pending_W;
  reg    [   0:0] rf_wen_pending_X;
  reg    [   0:0] rf_wen_pending_csr_D;
  reg    [   0:0] rs_en_D;
  reg    [   0:0] rt_en_D;
  reg    [   0:0] squash_D;
  reg    [   0:0] squash_F;
  reg    [   0:0] stall_D;
  reg    [   0:0] stall_F;
  reg    [   0:0] stall_M;
  reg    [   0:0] stall_W;
  reg    [   0:0] stall_X;
  reg    [   0:0] stats_en_wen_D;
  reg    [   0:0] stats_en_wen_M;
  reg    [   0:0] stats_en_wen_X;
  reg    [   0:0] stats_en_wen_pending_W;
  reg    [   0:0] val_D;
  reg    [   0:0] val_F;
  reg    [   0:0] val_M;
  reg    [   0:0] val_W;
  reg    [   0:0] val_X;
  reg    [   1:0] wb_result_sel_D;
  reg    [   1:0] wb_result_sel_X;

  // localparam declarations
  localparam ADD = 1;
  localparam ADDI = 2;
  localparam AND = 5;
  localparam ANDI = 6;
  localparam AUIPC = 7;
  localparam BEQ = 8;
  localparam BGE = 9;
  localparam BGEU = 10;
  localparam BLT = 11;
  localparam BLTU = 12;
  localparam BNE = 13;
  localparam CSRRS = 14;
  localparam CSRRW = 15;
  localparam CSR_MTOHOST = 1920;
  localparam CSR_STATS_EN = 1922;
  localparam CUSTOM0 = 16;
  localparam JAL = 21;
  localparam JALR = 22;
  localparam LUI = 28;
  localparam LW = 29;
  localparam MUL = 31;
  localparam NOP = 0;
  localparam OR = 36;
  localparam ORI = 37;
  localparam SLL = 45;
  localparam SLLI = 46;
  localparam SLT = 49;
  localparam SLTI = 50;
  localparam SLTIU = 51;
  localparam SLTU = 53;
  localparam SRA = 54;
  localparam SRAI = 55;
  localparam SRL = 58;
  localparam SRLI = 59;
  localparam SUB = 62;
  localparam SW = 64;
  localparam XOR = 65;
  localparam XORI = 66;
  localparam alu_add = 4'd0;
  localparam alu_and = 4'd6;
  localparam alu_aui = 4'd13;
  localparam alu_cp0 = 4'd11;
  localparam alu_cp1 = 4'd12;
  localparam alu_lt = 4'd4;
  localparam alu_ltu = 4'd5;
  localparam alu_or = 4'd3;
  localparam alu_sll = 4'd2;
  localparam alu_sra = 4'd10;
  localparam alu_srl = 4'd9;
  localparam alu_sub = 4'd1;
  localparam alu_x = 4'd0;
  localparam alu_xor = 4'd7;
  localparam am_rdat = 2'd0;
  localparam am_x = 2'd0;
  localparam am_zui = 2'd1;
  localparam bm_12 = 4'd6;
  localparam bm_fhst = 4'd4;
  localparam bm_pc = 4'd3;
  localparam bm_pc4 = 4'd8;
  localparam bm_rdat = 4'd0;
  localparam bm_samt = 4'd5;
  localparam bm_si = 4'd1;
  localparam bm_sw = 4'd7;
  localparam bm_x = 4'd0;
  localparam bm_zi = 4'd2;
  localparam br_beq = 3'd2;
  localparam br_bge = 3'd3;
  localparam br_bgeu = 3'd4;
  localparam br_blt = 3'd5;
  localparam br_bltu = 3'd6;
  localparam br_bne = 3'd1;
  localparam br_none = 3'd0;
  localparam br_x = 3'd0;
  localparam byp_m = 2'd2;
  localparam byp_r = 2'd0;
  localparam byp_w = 2'd3;
  localparam byp_x = 2'd1;
  localparam j_i = 2'd1;
  localparam j_n = 2'd0;
  localparam j_r = 2'd2;
  localparam j_x = 2'd0;
  localparam ld = 2'd1;
  localparam n = 1'd0;
  localparam nr = 2'd0;
  localparam st = 2'd2;
  localparam wm_a = 2'd0;
  localparam wm_m = 2'd1;
  localparam wm_o = 2'd2;
  localparam wm_x = 2'd0;
  localparam y = 1'd1;

  // inst_type_decoder_D temporaries
  wire   [   0:0] inst_type_decoder_D$reset;
  wire   [  31:0] inst_type_decoder_D$in_;
  wire   [   0:0] inst_type_decoder_D$clk;
  wire   [  15:0] inst_type_decoder_D$out;

  ProcDecodeRTL_0x417a6fa29460909d inst_type_decoder_D
  (
    .reset ( inst_type_decoder_D$reset ),
    .in_   ( inst_type_decoder_D$in_ ),
    .clk   ( inst_type_decoder_D$clk ),
    .out   ( inst_type_decoder_D$out )
  );

  // signal connections
  assign inst_type_decoder_D$clk   = clk;
  assign inst_type_decoder_D$in_   = inst_D;
  assign inst_type_decoder_D$reset = reset;


  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def go_reg():
  //       if s.reset:
  //         s.go_reg_F.next = 0
  //       elif s.go:
  //         s.go_reg_F.next = s.go
  //       else:
  //         s.go_reg_F.next = s.go_reg_F

  // logic for go_reg()
  always @ (posedge clk) begin
    if (reset) begin
      go_reg_F <= 0;
    end
    else begin
      if (go) begin
        go_reg_F <= go;
      end
      else begin
        go_reg_F <= go_reg_F;
      end
    end
  end

  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def reg_F():
  //       if s.reset:
  //         s.val_F.next = 0
  //       elif s.reg_en_F:
  //         s.val_F.next = 1

  // logic for reg_F()
  always @ (posedge clk) begin
    if (reset) begin
      val_F <= 0;
    end
    else begin
      if (reg_en_F) begin
        val_F <= 1;
      end
      else begin
      end
    end
  end

  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def reg_D():
  //       if s.reset:
  //         s.val_D.next = 0
  //       elif s.reg_en_D:
  //         s.val_D.next = s.next_val_F

  // logic for reg_D()
  always @ (posedge clk) begin
    if (reset) begin
      val_D <= 0;
    end
    else begin
      if (reg_en_D) begin
        val_D <= next_val_F;
      end
      else begin
      end
    end
  end

  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def reg_X():
  //       if s.reset:
  //         s.val_X.next            = 0
  //         s.stats_en_wen_X.next   = 0
  //       elif s.reg_en_X:
  //         s.val_X.next            = s.next_val_D
  //         s.rf_wen_pending_X.next = s.rf_wen_pending_D
  //         s.inst_type_X.next      = s.inst_type_decoder_D.out
  //         s.alu_fn_X.next         = s.alu_fn_D
  //         s.rf_waddr_X.next       = s.rf_waddr_D
  //         s.proc2mngr_val_X.next  = s.proc2mngr_val_D
  //         s.dmemreq_type_X.next   = s.dmemreq_type_D
  //         #s.xcelreq_X.next        = s.xcelreq_D
  //         s.wb_result_sel_X.next  = s.wb_result_sel_D
  //         s.br_type_X.next        = s.br_type_D
  //         s.mul_X.next            = s.mul_D
  //         s.ex_result_sel_X.next  = s.ex_result_sel_D
  //         s.stats_en_wen_X.next   = s.stats_en_wen_D

  // logic for reg_X()
  always @ (posedge clk) begin
    if (reset) begin
      val_X <= 0;
      stats_en_wen_X <= 0;
    end
    else begin
      if (reg_en_X) begin
        val_X <= next_val_D;
        rf_wen_pending_X <= rf_wen_pending_D;
        inst_type_X <= inst_type_decoder_D$out;
        alu_fn_X <= alu_fn_D;
        rf_waddr_X <= rf_waddr_D;
        proc2mngr_val_X <= proc2mngr_val_D;
        dmemreq_type_X <= dmemreq_type_D;
        wb_result_sel_X <= wb_result_sel_D;
        br_type_X <= br_type_D;
        mul_X <= mul_D;
        ex_result_sel_X <= ex_result_sel_D;
        stats_en_wen_X <= stats_en_wen_D;
      end
      else begin
      end
    end
  end

  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def reg_M():
  //       if s.reset:
  //         s.val_M.next            = 0
  //         s.stats_en_wen_M.next   = 0
  //       elif s.reg_en_M:
  //         s.val_M.next            = s.next_val_X
  //         s.rf_wen_pending_M.next = s.rf_wen_pending_X
  //         s.inst_type_M.next      = s.inst_type_X
  //         s.rf_waddr_M.next       = s.rf_waddr_X
  //         s.proc2mngr_val_M.next  = s.proc2mngr_val_X
  //         s.dmemreq_type_M.next   = s.dmemreq_type_X
  //         #s.xcelreq_M.next        = s.xcelreq_X
  //         s.wb_result_sel_M.next  = s.wb_result_sel_X
  //         s.stats_en_wen_M.next   = s.stats_en_wen_X

  // logic for reg_M()
  always @ (posedge clk) begin
    if (reset) begin
      val_M <= 0;
      stats_en_wen_M <= 0;
    end
    else begin
      if (reg_en_M) begin
        val_M <= next_val_X;
        rf_wen_pending_M <= rf_wen_pending_X;
        inst_type_M <= inst_type_X;
        rf_waddr_M <= rf_waddr_X;
        proc2mngr_val_M <= proc2mngr_val_X;
        dmemreq_type_M <= dmemreq_type_X;
        wb_result_sel_M <= wb_result_sel_X[0];
        stats_en_wen_M <= stats_en_wen_X;
      end
      else begin
      end
    end
  end

  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def reg_W():
  //
  //       if s.reset:
  //         s.val_W.next                  = 0
  //       elif s.reg_en_W:
  //         s.val_W.next                  = s.next_val_M
  //         s.rf_wen_pending_W.next       = s.rf_wen_pending_M
  //         s.inst_type_W.next            = s.inst_type_M
  //         s.rf_waddr_W.next             = s.rf_waddr_M
  //         s.proc2mngr_val_W.next        = s.proc2mngr_val_M
  //         s.stats_en_wen_pending_W.next = s.stats_en_wen_M

  // logic for reg_W()
  always @ (posedge clk) begin
    if (reset) begin
      val_W <= 0;
    end
    else begin
      if (reg_en_W) begin
        val_W <= next_val_M;
        rf_wen_pending_W <= rf_wen_pending_M;
        inst_type_W <= inst_type_M;
        rf_waddr_W <= rf_waddr_M;
        proc2mngr_val_W <= proc2mngr_val_M;
        stats_en_wen_pending_W <= stats_en_wen_M;
      end
      else begin
      end
    end
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_reg_en_F():
  //       s.reg_en_F.value = ( ~s.stall_F | s.squash_F )

  // logic for comb_reg_en_F()
  always @ (*) begin
    reg_en_F = (~stall_F|squash_F);
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_PC_sel_F():
  //       if not s.go_reg_F:
  //         s.pc_sel_F.value = 0           # go bit not valid
  //       elif s.pc_redirect_X:
  //         s.pc_sel_F.value = s.pc_sel_X  # use branch target (if taken)
  //       elif s.pc_redirect_D:
  //         s.pc_sel_F.value = s.pc_sel_D  # use jump target
  //       else:
  //         s.pc_sel_F.value = 0           # use pc+4

  // logic for comb_PC_sel_F()
  always @ (*) begin
    if (!go_reg_F) begin
      pc_sel_F = 0;
    end
    else begin
      if (pc_redirect_X) begin
        pc_sel_F = pc_sel_X;
      end
      else begin
        if (pc_redirect_D) begin
          pc_sel_F = pc_sel_D;
        end
        else begin
          pc_sel_F = 0;
        end
      end
    end
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_F():
  //       # ostall due to imemresp
  //
  //       s.ostall_F.value      = s.val_F & ~s.imemresp_val
  //
  //       # stall and squash in F stage
  //
  //       s.stall_F.value       = ~s.go_reg_F | ( s.val_F & ( s.ostall_F  | s.ostall_D | s.ostall_X | s.ostall_M | s.ostall_W ) )
  //       s.squash_F.value      = s.val_F & ( s.osquash_D | s.osquash_X )
  //
  //       # imem req is speical, it actually be sent out _before_ the F
  //       # stage, we need to send memreq everytime we are getting squashed
  //       # because we need to redirect the PC. We also need to factor in
  //       # reset. When we are resetting we shouldn't send out imem req.
  //
  //       s.imemreq_val.value   =  ~s.reset & (~s.stall_F | s.squash_F)
  //       s.imemresp_rdy.value  =  ~s.stall_F | s.squash_F
  //
  //       # We drop the mem response when we are getting squashed
  //
  //       s.imemresp_drop.value = s.squash_F
  //
  //       s.next_val_F.value    = s.val_F & ~s.stall_F & ~s.squash_F

  // logic for comb_F()
  always @ (*) begin
    ostall_F = (val_F&~imemresp_val);
    stall_F = (~go_reg_F|(val_F&((((ostall_F|ostall_D)|ostall_X)|ostall_M)|ostall_W)));
    squash_F = (val_F&(osquash_D|osquash_X));
    imemreq_val = (~reset&(~stall_F|squash_F));
    imemresp_rdy = (~stall_F|squash_F);
    imemresp_drop = squash_F;
    next_val_F = ((val_F&~stall_F)&~squash_F);
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_reg_en_D():
  //       s.reg_en_D.value = ~s.stall_D | s.squash_D

  // logic for comb_reg_en_D()
  always @ (*) begin
    reg_en_D = (~stall_D|squash_D);
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_control_table_D():
  //       inst = s.inst_type_decoder_D.out.value
  //       #                                                j    br       op0      rs op1      rt alu      xcel dmm wbmux rf      thst fhst
  //       #                                            val type type     muxsel   en muxsel   en fn       val  typ sel   wen mul val  rdy
  //       if   inst == NOP    : s.cs_D.value = concat( y,  j_n, br_none, am_x,    n, bm_x,    n, alu_x,   n,   nr, wm_a, n,  n,  n,   n    )
  //
  //       # reg-to-reg instruction
  //
  //       elif inst == MUL    : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_x,   n,   nr, wm_a, y,  y,  n,   n    )
  //
  //       elif inst == ADD    : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_add, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == SUB    : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_sub, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == SLT    : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_lt,  n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == SLTU   : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_ltu, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == AND    : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_and, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == OR     : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_or,  n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == XOR    : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_xor, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == SRA    : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_sra, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == SRL    : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_srl, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == SLL    : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_sll, n,   nr, wm_a, y,  n,  n,   n    )
  //
  //       # reg-to-imm instruction
  //
  //       elif inst == ADDI   : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_si,   n, alu_add, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == SLTI   : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_si,   n, alu_lt,  n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == SLTIU  : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_si,   n, alu_ltu, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == ORI    : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_zi,   n, alu_or,  n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == ANDI   : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_zi,   n, alu_and, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == XORI   : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_zi,   n, alu_xor, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == SRAI   : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_samt, n, alu_sra, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == SRLI   : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_samt, n, alu_srl, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == SLLI   : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_samt, n, alu_sll, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == LUI    : s.cs_D.value = concat( y,  j_n, br_none, am_zui,  n, bm_12,   n, alu_sll, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == AUIPC  : s.cs_D.value = concat( y,  j_n, br_none, am_zui,  n, bm_pc,   n, alu_aui, n,   nr, wm_a, y,  n,  n,   n    )
  //
  //       # dmem instruction
  //
  //       elif inst == LW     : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_si,   n, alu_add, n,   ld, wm_m, y,  n,  n,   n    )
  //       elif inst == SW     : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_sw,   y, alu_add, n,   st, wm_x, n,  n,  n,   n    )
  //
  //       elif inst == CUSTOM0: s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_rdat, y, alu_x,   y,   nr, wm_o, y,  n,  n,   n    )
  //
  //       # branch instruction
  //
  //       elif inst == BNE    : s.cs_D.value = concat( y,  j_n, br_bne,  am_rdat, y, bm_rdat, y, alu_x,   n,   nr, wm_a, n,  n,  n,   n    )
  //       elif inst == BEQ    : s.cs_D.value = concat( y,  j_n, br_beq,  am_rdat, y, bm_rdat, y, alu_x,   n,   nr, wm_a, n,  n,  n,   n    )
  //       elif inst == BGE    : s.cs_D.value = concat( y,  j_n, br_bge,  am_rdat, y, bm_rdat, y, alu_x,   n,   nr, wm_a, n,  n,  n,   n    )
  //       elif inst == BLT    : s.cs_D.value = concat( y,  j_n, br_blt,  am_rdat, y, bm_rdat, y, alu_x,   n,   nr, wm_a, n,  n,  n,   n    )
  //       elif inst == BLTU   : s.cs_D.value = concat( y,  j_n, br_bltu, am_rdat, y, bm_rdat, y, alu_x,   n,   nr, wm_a, n,  n,  n,   n    )
  //       elif inst == BGEU   : s.cs_D.value = concat( y,  j_n, br_bgeu, am_rdat, y, bm_rdat, y, alu_x,   n,   nr, wm_a, n,  n,  n,   n    )
  //
  //       # jump instruction
  //
  //       elif inst == JALR   : s.cs_D.value = concat( y,  j_r, br_none, am_rdat, y, bm_pc4,  n, alu_cp1, n,   nr, wm_a, y,  n,  n,   n    )
  //       elif inst == JAL    : s.cs_D.value = concat( y,  j_i, br_none, am_x,    n, bm_pc4,  n, alu_cp1, n,   nr, wm_a, y,  n,  n,   n    )
  //
  //       # CSR instruction
  //
  //       # Note that currently we do _not_ correctly implement CSRRW/CSRRS
  //       # semantics: for CSRRW only rd == x0 are supported. We just write
  //       # RF[rs1] to CSR, not swap old CSR value to RF[rd]; for CSRRS only
  //       # rs1 == x0 is supported. We just read CSR value to RF[rd], not set
  //       # CSR according to RF[rs1].
  //
  //       elif inst == CSRRS  : s.cs_D.value = concat( y,  j_n, br_none, am_x,    n, bm_fhst, n, alu_cp1, n,   nr, wm_a, y,  n,  n,   y    )
  //       elif inst == CSRRW  : s.cs_D.value = concat( y,  j_n, br_none, am_rdat, y, bm_x,    n, alu_cp0, n,   nr, wm_a, n,  n,  y,   n    )
  //
  //       else:                 s.cs_D.value = concat( n,  j_x, br_x,    am_x,    n, bm_x,    n, alu_x,   n,   nr, wm_x, n,  n,  n,   n    )
  //
  //       # Unpack control signals
  //
  //       s.inst_val_D.value           = s.cs_D[ CS_INST_VAL       ]
  //       s.j_type_D.value             = s.cs_D[ CS_J_TYPE         ]
  //       s.br_type_D.value            = s.cs_D[ CS_BR_TYPE        ]
  //       s.op0_sel_D.value            = s.cs_D[ CS_OP0_SEL        ]
  //       s.rs_en_D.value              = s.cs_D[ CS_RS_EN          ]
  //       s.op1_sel_D.value            = s.cs_D[ CS_OP1_SEL        ]
  //       s.rt_en_D.value              = s.cs_D[ CS_RT_EN          ]
  //       s.alu_fn_D.value             = s.cs_D[ CS_ALU_FN         ]
  //       #s.xcelreq_D.value            = s.cs_D[ CS_XCELREQ_VALUE  ]
  //       s.dmemreq_type_D.value       = s.cs_D[ CS_DMEMREQ_TYPE   ]
  //       s.wb_result_sel_D.value      = s.cs_D[ CS_WB_RESULT_SEL  ]
  //       s.rf_wen_pending_csr_D.value = s.cs_D[ CS_RF_WEN_PENDING ]
  //       s.mul_D.value                = s.cs_D[ CS_MUL            ]
  //       s.mngr2proc_rdy_D.value      = s.cs_D[ CS_MNGR2PROC_RDY  ]
  //
  //       # Handle mtc0, mfc0, stats_en
  //
  //       s.proc2mngr_val_D.value = 0
  //       s.stats_en_wen_D.value  = 0
  //
  //       if s.cs_D[ CS_PROC2MNGR_VAL ] == y:
  //         if   s.inst_D[ IMM12 ] == CSR_MTOHOST:
  //           s.proc2mngr_val_D.value = 1
  //         elif s.inst_D[ IMM12 ] == CSR_STATS_EN:
  //           s.stats_en_wen_D.value = 1
  //
  //       # setting the actual write address
  //
  //       s.rf_waddr_D.value = s.inst_D[ RD ]
  //
  //       # write enable of RF[RD]
  //
  //       if ( s.inst_type_decoder_D.out == CUSTOM0 ) and s.inst_D[ XD ] == 0:
  //         s.rf_wen_pending_D.value = 0
  //       else:
  //         s.rf_wen_pending_D.value = s.rf_wen_pending_csr_D
  //
  //       # jump logic
  //
  //       if s.val_D:
  //         if   s.j_type_D == j_i:
  //           s.pc_redirect_D.value = 1
  //           s.pc_sel_D.value      = 2
  //         elif s.j_type_D == j_r:
  //           s.pc_redirect_D.value = 1
  //           s.pc_sel_D.value      = 3
  //         else:
  //           s.pc_redirect_D.value = 0
  //           s.pc_sel_D.value      = 0
  //       else:
  //         s.pc_redirect_D.value   = 0
  //         s.pc_sel_D.value        = 0
  //
  //       # imul result sel
  //
  //       if s.mul_D : s.ex_result_sel_D.value = 1
  //       else       : s.ex_result_sel_D.value = 0

  // logic for comb_control_table_D()
  always @ (*) begin
    inst__10 = inst_type_decoder_D$out;
    if ((inst__10 == NOP)) begin
      cs_D = { y,j_n,br_none,am_x,n,bm_x,n,alu_x,n,nr,wm_a,n,n,n,n };
    end
    else begin
      if ((inst__10 == MUL)) begin
        cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_x,n,nr,wm_a,y,y,n,n };
      end
      else begin
        if ((inst__10 == ADD)) begin
          cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_add,n,nr,wm_a,y,n,n,n };
        end
        else begin
          if ((inst__10 == SUB)) begin
            cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_sub,n,nr,wm_a,y,n,n,n };
          end
          else begin
            if ((inst__10 == SLT)) begin
              cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_lt,n,nr,wm_a,y,n,n,n };
            end
            else begin
              if ((inst__10 == SLTU)) begin
                cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_ltu,n,nr,wm_a,y,n,n,n };
              end
              else begin
                if ((inst__10 == AND)) begin
                  cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_and,n,nr,wm_a,y,n,n,n };
                end
                else begin
                  if ((inst__10 == OR)) begin
                    cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_or,n,nr,wm_a,y,n,n,n };
                  end
                  else begin
                    if ((inst__10 == XOR)) begin
                      cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_xor,n,nr,wm_a,y,n,n,n };
                    end
                    else begin
                      if ((inst__10 == SRA)) begin
                        cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_sra,n,nr,wm_a,y,n,n,n };
                      end
                      else begin
                        if ((inst__10 == SRL)) begin
                          cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_srl,n,nr,wm_a,y,n,n,n };
                        end
                        else begin
                          if ((inst__10 == SLL)) begin
                            cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_sll,n,nr,wm_a,y,n,n,n };
                          end
                          else begin
                            if ((inst__10 == ADDI)) begin
                              cs_D = { y,j_n,br_none,am_rdat,y,bm_si,n,alu_add,n,nr,wm_a,y,n,n,n };
                            end
                            else begin
                              if ((inst__10 == SLTI)) begin
                                cs_D = { y,j_n,br_none,am_rdat,y,bm_si,n,alu_lt,n,nr,wm_a,y,n,n,n };
                              end
                              else begin
                                if ((inst__10 == SLTIU)) begin
                                  cs_D = { y,j_n,br_none,am_rdat,y,bm_si,n,alu_ltu,n,nr,wm_a,y,n,n,n };
                                end
                                else begin
                                  if ((inst__10 == ORI)) begin
                                    cs_D = { y,j_n,br_none,am_rdat,y,bm_zi,n,alu_or,n,nr,wm_a,y,n,n,n };
                                  end
                                  else begin
                                    if ((inst__10 == ANDI)) begin
                                      cs_D = { y,j_n,br_none,am_rdat,y,bm_zi,n,alu_and,n,nr,wm_a,y,n,n,n };
                                    end
                                    else begin
                                      if ((inst__10 == XORI)) begin
                                        cs_D = { y,j_n,br_none,am_rdat,y,bm_zi,n,alu_xor,n,nr,wm_a,y,n,n,n };
                                      end
                                      else begin
                                        if ((inst__10 == SRAI)) begin
                                          cs_D = { y,j_n,br_none,am_rdat,y,bm_samt,n,alu_sra,n,nr,wm_a,y,n,n,n };
                                        end
                                        else begin
                                          if ((inst__10 == SRLI)) begin
                                            cs_D = { y,j_n,br_none,am_rdat,y,bm_samt,n,alu_srl,n,nr,wm_a,y,n,n,n };
                                          end
                                          else begin
                                            if ((inst__10 == SLLI)) begin
                                              cs_D = { y,j_n,br_none,am_rdat,y,bm_samt,n,alu_sll,n,nr,wm_a,y,n,n,n };
                                            end
                                            else begin
                                              if ((inst__10 == LUI)) begin
                                                cs_D = { y,j_n,br_none,am_zui,n,bm_12,n,alu_sll,n,nr,wm_a,y,n,n,n };
                                              end
                                              else begin
                                                if ((inst__10 == AUIPC)) begin
                                                  cs_D = { y,j_n,br_none,am_zui,n,bm_pc,n,alu_aui,n,nr,wm_a,y,n,n,n };
                                                end
                                                else begin
                                                  if ((inst__10 == LW)) begin
                                                    cs_D = { y,j_n,br_none,am_rdat,y,bm_si,n,alu_add,n,ld,wm_m,y,n,n,n };
                                                  end
                                                  else begin
                                                    if ((inst__10 == SW)) begin
                                                      cs_D = { y,j_n,br_none,am_rdat,y,bm_sw,y,alu_add,n,st,wm_x,n,n,n,n };
                                                    end
                                                    else begin
                                                      if ((inst__10 == CUSTOM0)) begin
                                                        cs_D = { y,j_n,br_none,am_rdat,y,bm_rdat,y,alu_x,y,nr,wm_o,y,n,n,n };
                                                      end
                                                      else begin
                                                        if ((inst__10 == BNE)) begin
                                                          cs_D = { y,j_n,br_bne,am_rdat,y,bm_rdat,y,alu_x,n,nr,wm_a,n,n,n,n };
                                                        end
                                                        else begin
                                                          if ((inst__10 == BEQ)) begin
                                                            cs_D = { y,j_n,br_beq,am_rdat,y,bm_rdat,y,alu_x,n,nr,wm_a,n,n,n,n };
                                                          end
                                                          else begin
                                                            if ((inst__10 == BGE)) begin
                                                              cs_D = { y,j_n,br_bge,am_rdat,y,bm_rdat,y,alu_x,n,nr,wm_a,n,n,n,n };
                                                            end
                                                            else begin
                                                              if ((inst__10 == BLT)) begin
                                                                cs_D = { y,j_n,br_blt,am_rdat,y,bm_rdat,y,alu_x,n,nr,wm_a,n,n,n,n };
                                                              end
                                                              else begin
                                                                if ((inst__10 == BLTU)) begin
                                                                  cs_D = { y,j_n,br_bltu,am_rdat,y,bm_rdat,y,alu_x,n,nr,wm_a,n,n,n,n };
                                                                end
                                                                else begin
                                                                  if ((inst__10 == BGEU)) begin
                                                                    cs_D = { y,j_n,br_bgeu,am_rdat,y,bm_rdat,y,alu_x,n,nr,wm_a,n,n,n,n };
                                                                  end
                                                                  else begin
                                                                    if ((inst__10 == JALR)) begin
                                                                      cs_D = { y,j_r,br_none,am_rdat,y,bm_pc4,n,alu_cp1,n,nr,wm_a,y,n,n,n };
                                                                    end
                                                                    else begin
                                                                      if ((inst__10 == JAL)) begin
                                                                        cs_D = { y,j_i,br_none,am_x,n,bm_pc4,n,alu_cp1,n,nr,wm_a,y,n,n,n };
                                                                      end
                                                                      else begin
                                                                        if ((inst__10 == CSRRS)) begin
                                                                          cs_D = { y,j_n,br_none,am_x,n,bm_fhst,n,alu_cp1,n,nr,wm_a,y,n,n,y };
                                                                        end
                                                                        else begin
                                                                          if ((inst__10 == CSRRW)) begin
                                                                            cs_D = { y,j_n,br_none,am_rdat,y,bm_x,n,alu_cp0,n,nr,wm_a,n,n,y,n };
                                                                          end
                                                                          else begin
                                                                            cs_D = { n,j_x,br_x,am_x,n,bm_x,n,alu_x,n,nr,wm_x,n,n,n,n };
                                                                          end
                                                                        end
                                                                      end
                                                                    end
                                                                  end
                                                                end
                                                              end
                                                            end
                                                          end
                                                        end
                                                      end
                                                    end
                                                  end
                                                end
                                              end
                                            end
                                          end
                                        end
                                      end
                                    end
                                  end
                                end
                              end
                            end
                          end
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    inst_val_D = cs_D[(27)-1:26];
    j_type_D = cs_D[(26)-1:24];
    br_type_D = cs_D[(24)-1:21];
    op0_sel_D = cs_D[(21)-1:19];
    rs_en_D = cs_D[(19)-1:18];
    op1_sel_D = cs_D[(18)-1:14];
    rt_en_D = cs_D[(14)-1:13];
    alu_fn_D = cs_D[(13)-1:9];
    dmemreq_type_D = cs_D[(8)-1:6];
    wb_result_sel_D = cs_D[(6)-1:4];
    rf_wen_pending_csr_D = cs_D[(4)-1:3];
    mul_D = cs_D[(3)-1:2];
    mngr2proc_rdy_D = cs_D[(1)-1:0];
    proc2mngr_val_D = 0;
    stats_en_wen_D = 0;
    if ((cs_D[(2)-1:1] == y)) begin
      if ((inst_D[(32)-1:20] == CSR_MTOHOST)) begin
        proc2mngr_val_D = 1;
      end
      else begin
        if ((inst_D[(32)-1:20] == CSR_STATS_EN)) begin
          stats_en_wen_D = 1;
        end
        else begin
        end
      end
    end
    else begin
    end
    rf_waddr_D = inst_D[(12)-1:7];
    if (((inst_type_decoder_D$out == CUSTOM0)&&(inst_D[(15)-1:14] == 0))) begin
      rf_wen_pending_D = 0;
    end
    else begin
      rf_wen_pending_D = rf_wen_pending_csr_D;
    end
    if (val_D) begin
      if ((j_type_D == j_i)) begin
        pc_redirect_D = 1;
        pc_sel_D = 2;
      end
      else begin
        if ((j_type_D == j_r)) begin
          pc_redirect_D = 1;
          pc_sel_D = 3;
        end
        else begin
          pc_redirect_D = 0;
          pc_sel_D = 0;
        end
      end
    end
    else begin
      pc_redirect_D = 0;
      pc_sel_D = 0;
    end
    if (mul_D) begin
      ex_result_sel_D = 1;
    end
    else begin
      ex_result_sel_D = 0;
    end
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_bypass_D():
  //       if   s.rs_en_D & s.val_X & s.rf_wen_pending_X & ( s.inst_D[ RS1 ] == s.rf_waddr_X ) & ( s.rf_waddr_X != 0 ):
  //         s.op0_byp_sel_D.value = byp_x;
  //       elif s.rs_en_D & s.val_M & s.rf_wen_pending_M & ( s.inst_D[ RS1 ] == s.rf_waddr_M ) & ( s.rf_waddr_M != 0 ):
  //         s.op0_byp_sel_D.value = byp_m;
  //       elif s.rs_en_D & s.val_W & s.rf_wen_pending_W & ( s.inst_D[ RS1 ] == s.rf_waddr_W ) & ( s.rf_waddr_W != 0 ):
  //         s.op0_byp_sel_D.value = byp_w;
  //       else:
  //         s.op0_byp_sel_D.value = byp_r;
  //
  //       if   s.rt_en_D & s.val_X & s.rf_wen_pending_X & ( s.inst_D[ RS2 ] == s.rf_waddr_X ) & ( s.rf_waddr_X != 0 ):
  //         s.op1_byp_sel_D.value = byp_x;
  //       elif s.rt_en_D & s.val_M & s.rf_wen_pending_M & ( s.inst_D[ RS2 ] == s.rf_waddr_M ) & ( s.rf_waddr_M != 0 ):
  //         s.op1_byp_sel_D.value = byp_m;
  //       elif s.rt_en_D & s.val_W & s.rf_wen_pending_W & ( s.inst_D[ RS2 ] == s.rf_waddr_W ) & ( s.rf_waddr_W != 0 ):
  //         s.op1_byp_sel_D.value = byp_w;
  //       else:
  //         s.op1_byp_sel_D.value = byp_r;

  // logic for comb_bypass_D()
  always @ (*) begin
    if (((((rs_en_D&val_X)&rf_wen_pending_X)&(inst_D[(20)-1:15] == rf_waddr_X))&(rf_waddr_X != 0))) begin
      op0_byp_sel_D = byp_x;
    end
    else begin
      if (((((rs_en_D&val_M)&rf_wen_pending_M)&(inst_D[(20)-1:15] == rf_waddr_M))&(rf_waddr_M != 0))) begin
        op0_byp_sel_D = byp_m;
      end
      else begin
        if (((((rs_en_D&val_W)&rf_wen_pending_W)&(inst_D[(20)-1:15] == rf_waddr_W))&(rf_waddr_W != 0))) begin
          op0_byp_sel_D = byp_w;
        end
        else begin
          op0_byp_sel_D = byp_r;
        end
      end
    end
    if (((((rt_en_D&val_X)&rf_wen_pending_X)&(inst_D[(25)-1:20] == rf_waddr_X))&(rf_waddr_X != 0))) begin
      op1_byp_sel_D = byp_x;
    end
    else begin
      if (((((rt_en_D&val_M)&rf_wen_pending_M)&(inst_D[(25)-1:20] == rf_waddr_M))&(rf_waddr_M != 0))) begin
        op1_byp_sel_D = byp_m;
      end
      else begin
        if (((((rt_en_D&val_W)&rf_wen_pending_W)&(inst_D[(25)-1:20] == rf_waddr_W))&(rf_waddr_W != 0))) begin
          op1_byp_sel_D = byp_w;
        end
        else begin
          op1_byp_sel_D = byp_r;
        end
      end
    end
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_stall_D():
  //       s.ostall_load_use_X_rs_D.value = ( s.rs_en_D & s.val_X & s.rf_wen_pending_X &
  //                                        ( s.inst_D[ RS1 ] == s.rf_waddr_X ) &
  //                                        ( s.rf_waddr_X != 0 ) & ( s.dmemreq_type_X == ld ) )
  //       s.ostall_load_use_X_rt_D.value = ( s.rt_en_D & s.val_X & s.rf_wen_pending_X &
  //                                        ( s.inst_D[ RS2 ] == s.rf_waddr_X ) &
  //                                        ( s.rf_waddr_X != 0 ) & ( s.dmemreq_type_X == ld ) )
  //       s.ostall_mfx_use_X_rs_D.value =  ( s.rs_en_D & s.val_X & s.rf_wen_pending_X &
  //                                        ( s.inst_D[ RS1 ] == s.rf_waddr_X ) &
  //                                        ( s.rf_waddr_X != 0 ) & ( s.inst_type_X == CUSTOM0 ) )
  //       s.ostall_mfx_use_X_rt_D.value =  ( s.rt_en_D & s.val_X & s.rf_wen_pending_X &
  //                                        ( s.inst_D[ RS2 ] == s.rf_waddr_X ) &
  //                                        ( s.rf_waddr_X != 0 ) & ( s.inst_type_X == CUSTOM0 ) )
  //
  //       s.ostall_hazard_D.value        = s.ostall_load_use_X_rs_D | s.ostall_load_use_X_rt_D | \
  //                                        s.ostall_mfx_use_X_rs_D  | s.ostall_mfx_use_X_rt_D

  // logic for comb_stall_D()
  always @ (*) begin
    ostall_load_use_X_rs_D = (((((rs_en_D&val_X)&rf_wen_pending_X)&(inst_D[(20)-1:15] == rf_waddr_X))&(rf_waddr_X != 0))&(dmemreq_type_X == ld));
    ostall_load_use_X_rt_D = (((((rt_en_D&val_X)&rf_wen_pending_X)&(inst_D[(25)-1:20] == rf_waddr_X))&(rf_waddr_X != 0))&(dmemreq_type_X == ld));
    ostall_mfx_use_X_rs_D = (((((rs_en_D&val_X)&rf_wen_pending_X)&(inst_D[(20)-1:15] == rf_waddr_X))&(rf_waddr_X != 0))&(inst_type_X == CUSTOM0));
    ostall_mfx_use_X_rt_D = (((((rt_en_D&val_X)&rf_wen_pending_X)&(inst_D[(25)-1:20] == rf_waddr_X))&(rf_waddr_X != 0))&(inst_type_X == CUSTOM0));
    ostall_hazard_D = (((ostall_load_use_X_rs_D|ostall_load_use_X_rt_D)|ostall_mfx_use_X_rs_D)|ostall_mfx_use_X_rt_D);
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_D():
  //       # ostall due to mngr2proc
  //
  //       s.ostall_mngr_D.value = s.mngr2proc_rdy_D & ~s.mngr2proc_val
  //
  //       # ostall due to mul
  //
  //       s.ostall_mul_D.value  = s.val_D & ( s.mul_D == y ) & ~s.mul_req_rdy_D
  //
  //       # put together all ostall conditions
  //
  //       s.ostall_D.value      = s.val_D & ( s.ostall_mngr_D | s.ostall_hazard_D | s.ostall_mul_D );
  //
  //       # stall in D stage
  //
  //       s.stall_D.value       = s.val_D & ( s.ostall_D | s.ostall_X | s.ostall_M | s.ostall_W )
  //
  //       # osquash due to jumps, not implemented yet
  //
  //       s.osquash_D.value     = s.val_D & ~s.stall_D & s.pc_redirect_D
  //
  //       # squash in D stage
  //
  //       s.squash_D.value      = s.val_D & s.osquash_X
  //
  //       # mngr2proc port
  //
  //       s.mngr2proc_rdy.value = s.val_D & ~s.stall_D & s.mngr2proc_rdy_D
  //
  //       # mul req
  //
  //       s.mul_req_val_D.value = s.val_D & ~s.stall_D & ~s.squash_D & ( s.mul_D == y )
  //
  //       # next valid bit
  //
  //       s.next_val_D.value    = s.val_D & ~s.stall_D & ~s.squash_D

  // logic for comb_D()
  always @ (*) begin
    ostall_mngr_D = (mngr2proc_rdy_D&~mngr2proc_val);
    ostall_mul_D = ((val_D&(mul_D == y))&~mul_req_rdy_D);
    ostall_D = (val_D&((ostall_mngr_D|ostall_hazard_D)|ostall_mul_D));
    stall_D = (val_D&(((ostall_D|ostall_X)|ostall_M)|ostall_W));
    osquash_D = ((val_D&~stall_D)&pc_redirect_D);
    squash_D = (val_D&osquash_X);
    mngr2proc_rdy = ((val_D&~stall_D)&mngr2proc_rdy_D);
    mul_req_val_D = (((val_D&~stall_D)&~squash_D)&(mul_D == y));
    next_val_D = ((val_D&~stall_D)&~squash_D);
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_reg_en_X():
  //       s.reg_en_X.value  = ~s.stall_X

  // logic for comb_reg_en_X()
  always @ (*) begin
    reg_en_X = ~stall_X;
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_br_X():
  //       if s.val_X:
  //         if   ( s.br_type_X == br_bne  ): s.pc_redirect_X.value = ~s.br_cond_eq_X
  //         elif ( s.br_type_X == br_beq  ): s.pc_redirect_X.value =  s.br_cond_eq_X
  //         elif ( s.br_type_X == br_bge  ): s.pc_redirect_X.value = ~s.br_cond_lt_X
  //         elif ( s.br_type_X == br_blt  ): s.pc_redirect_X.value =  s.br_cond_lt_X
  //         elif ( s.br_type_X == br_bgeu ): s.pc_redirect_X.value = ~s.br_cond_ltu_X
  //         elif ( s.br_type_X == br_bltu ): s.pc_redirect_X.value =  s.br_cond_ltu_X
  //         else:                            s.pc_redirect_X.value =  0
  //
  //         s.pc_sel_X.value        = 1
  //       else:
  //         s.pc_redirect_X.value   = 0
  //         s.pc_sel_X.value        = 0

  // logic for comb_br_X()
  always @ (*) begin
    if (val_X) begin
      if ((br_type_X == br_bne)) begin
        pc_redirect_X = ~br_cond_eq_X;
      end
      else begin
        if ((br_type_X == br_beq)) begin
          pc_redirect_X = br_cond_eq_X;
        end
        else begin
          if ((br_type_X == br_bge)) begin
            pc_redirect_X = ~br_cond_lt_X;
          end
          else begin
            if ((br_type_X == br_blt)) begin
              pc_redirect_X = br_cond_lt_X;
            end
            else begin
              if ((br_type_X == br_bgeu)) begin
                pc_redirect_X = ~br_cond_ltu_X;
              end
              else begin
                if ((br_type_X == br_bltu)) begin
                  pc_redirect_X = br_cond_ltu_X;
                end
                else begin
                  pc_redirect_X = 0;
                end
              end
            end
          end
        end
      end
      pc_sel_X = 1;
    end
    else begin
      pc_redirect_X = 0;
      pc_sel_X = 0;
    end
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_X():
  //       # ostall due to dmemreq or imul or xcelreq
  //
  //       s.ostall_dmem_X.value    = s.val_X & ( s.dmemreq_type_X != nr ) & ~s.dmemreq_rdy
  //       s.ostall_mul_X.value     = s.val_X & ( s.mul_X == y ) & ~s.mul_resp_val_X
  //       #s.stall_xcel_X.value    = s.val_X & ~s.xcelreq_rdy & s.xcelreq_X
  //
  //       s.ostall_X.value         = s.val_X & ( s.ostall_dmem_X | s.ostall_mul_X.value | s.ostall_xcel_X )
  //
  //       # stall in X stage
  //
  //       s.stall_X.value     = s.val_X & ( s.ostall_X | s.ostall_M | s.ostall_W )
  //
  //       # osquash due to taken branches
  //
  //       s.osquash_X.value   = s.val_X & ~s.stall_X & s.pc_redirect_X
  //
  //       # send dmemreq if not stalling
  //
  //       s.dmemreq_val.value = s.val_X & ~s.stall_X & ( s.dmemreq_type_X != nr )
  //
  //       if   s.dmemreq_type_X == ld:
  //         s.dmemreq_msg_type.value = 0  # read
  //       elif s.dmemreq_type_X == st:
  //         s.dmemreq_msg_type.value = 1  # write
  //       else:
  //         s.dmemreq_msg_type.value = 0
  //
  //       #s.xcelreq_val.value = s.val_X & ~s.stall_X & ( s.xcelreq_X == y )
  //
  //       # mul resp
  //
  //       s.mul_resp_rdy_X.value = s.val_X & ~s.stall_X & ( s.mul_X == y )
  //
  //       # next valid bit
  //
  //       s.next_val_X.value  = s.val_X & ~s.stall_X

  // logic for comb_X()
  always @ (*) begin
    ostall_dmem_X = ((val_X&(dmemreq_type_X != nr))&~dmemreq_rdy);
    ostall_mul_X = ((val_X&(mul_X == y))&~mul_resp_val_X);
    ostall_X = (val_X&((ostall_dmem_X|ostall_mul_X))); //|ostall_xcel_X));
    stall_X = (val_X&((ostall_X|ostall_M)|ostall_W));
    osquash_X = ((val_X&~stall_X)&pc_redirect_X);
    dmemreq_val = ((val_X&~stall_X)&(dmemreq_type_X != nr));
    if ((dmemreq_type_X == ld)) begin
      dmemreq_msg_type = 0;
    end
    else begin
      if ((dmemreq_type_X == st)) begin
        dmemreq_msg_type = 1;
      end
      else begin
        dmemreq_msg_type = 0;
      end
    end
    mul_resp_rdy_X = ((val_X&~stall_X)&(mul_X == y));
    next_val_X = (val_X&~stall_X);
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_reg_en_M():
  //       s.reg_en_M.value = ~s.stall_M

  // logic for comb_reg_en_M()
  always @ (*) begin
    reg_en_M = ~stall_M;
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_M():
  //
  //       #s.ostall_M.value      = s.val_M & ((( s.dmemreq_type_M != nr ) & ~s.dmemresp_val) | (s.xcelreq_M & ~s.xcelresp_val))
  //       s.ostall_M.value      = s.val_M & ((( s.dmemreq_type_M != nr ) & ~s.dmemresp_val))
  //       
  //       # stall in M stage
  //
  //       s.stall_M.value       = s.val_M & ( s.ostall_M | s.ostall_W )
  //
  //       # set dmemresp ready if not stalling
  //
  //       s.dmemresp_rdy.value  = s.val_M & ~s.stall_M & ( s.dmemreq_type_M != nr )
  //       #s.xcelresp_rdy.value  = s.val_M & ~s.stall_M & s.xcelreq_M
  //
  //       # next valid bit
  //
  //       s.next_val_M.value    = s.val_M & ~s.stall_M

  // logic for comb_M()
  always @ (*) begin
    ostall_M = (val_M&((dmemreq_type_M != nr)&~dmemresp_val));
    stall_M = (val_M&(ostall_M|ostall_W));
    dmemresp_rdy = ((val_M&~stall_M)&(dmemreq_type_M != nr));
    next_val_M = (val_M&~stall_M);
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_W():
  //       s.reg_en_W.value = ~s.stall_W

  // logic for comb_W()
  always @ (*) begin
    reg_en_W = ~stall_W;
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_W():
  //
  //       # set write enables if valid
  //
  //       s.rf_wen_W.value       = s.val_W & s.rf_wen_pending_W
  //       s.stats_en_wen_W.value = s.val_W & s.stats_en_wen_pending_W
  //
  //       # ostall due to proc2mngr
  //
  //       s.ostall_W.value       = s.val_W & s.proc2mngr_val_W & ~s.proc2mngr_rdy
  //
  //       # stall in W stage
  //
  //       s.stall_W.value        = s.val_W & s.ostall_W
  //
  //       # set proc2mngr val if not stalling
  //
  //       s.proc2mngr_val.value  = s.val_W & ~s.stall_W & s.proc2mngr_val_W
  //
  //       s.commit_inst.value    = s.val_W

  // logic for comb_W()
  always @ (*) begin
    rf_wen_W = (val_W&rf_wen_pending_W);
    stats_en_wen_W = (val_W&stats_en_wen_pending_W);
    ostall_W = ((val_W&proc2mngr_val_W)&~proc2mngr_rdy);
    stall_W = (val_W&ostall_W);
    proc2mngr_val = ((val_W&~stall_W)&proc2mngr_val_W);
    commit_inst = val_W;
  end


endmodule // ProcCtrlRTL_0x7340ba89deb7e746
`default_nettype wire

//-----------------------------------------------------------------------------
// ProcDecodeRTL_0x417a6fa29460909d
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module ProcDecodeRTL_0x417a6fa29460909d
(
  input  wire [   0:0] clk,
  input  wire [  31:0] in_,
  output reg  [  15:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam ADD = 1;
  localparam ADDI = 2;
  localparam ADDIW = 3;
  localparam ADDW = 4;
  localparam AND = 5;
  localparam ANDI = 6;
  localparam AUIPC = 7;
  localparam BEQ = 8;
  localparam BGE = 9;
  localparam BGEU = 10;
  localparam BLT = 11;
  localparam BLTU = 12;
  localparam BNE = 13;
  localparam CSRRS = 14;
  localparam CSRRW = 15;
  localparam CUSTOM0 = 16;
  localparam DIV = 17;
  localparam DIVU = 18;
  localparam DIVUW = 19;
  localparam DIVW = 20;
  localparam JAL = 21;
  localparam JALR = 22;
  localparam LB = 23;
  localparam LBU = 24;
  localparam LD = 25;
  localparam LH = 26;
  localparam LHU = 27;
  localparam LUI = 28;
  localparam LW = 29;
  localparam LWU = 30;
  localparam MUL = 31;
  localparam MULH = 32;
  localparam MULHSU = 33;
  localparam MULHU = 34;
  localparam MULW = 35;
  localparam NOP = 0;
  localparam OR = 36;
  localparam ORI = 37;
  localparam REM = 38;
  localparam REMU = 39;
  localparam REMUW = 40;
  localparam REMW = 41;
  localparam SB = 42;
  localparam SD = 43;
  localparam SH = 44;
  localparam SLL = 45;
  localparam SLLI = 46;
  localparam SLLIW = 47;
  localparam SLLW = 48;
  localparam SLT = 49;
  localparam SLTI = 50;
  localparam SLTIU = 51;
  localparam SLTIW = 52;
  localparam SLTU = 53;
  localparam SRA = 54;
  localparam SRAI = 55;
  localparam SRAIW = 56;
  localparam SRAW = 57;
  localparam SRL = 58;
  localparam SRLI = 59;
  localparam SRLIW = 60;
  localparam SRLW = 61;
  localparam SUB = 62;
  localparam SUBW = 63;
  localparam SW = 64;
  localparam XOR = 65;
  localparam XORI = 66;



  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       if         s.in_           == 0:       s.out.value = NOP
  //       # OP
  //       elif       s.in_[OP]       == 0b0110011:
  //         if       s.in_[FUNCT7]   == 0b0000000:
  //           if     s.in_[FUNCT3]   == 0b000:   s.out.value = ADD
  //           elif   s.in_[FUNCT3]   == 0b001:   s.out.value = SLL
  //           elif   s.in_[FUNCT3]   == 0b010:   s.out.value = SLT
  //           elif   s.in_[FUNCT3]   == 0b011:   s.out.value = SLTU
  //           elif   s.in_[FUNCT3]   == 0b100:   s.out.value = XOR
  //           elif   s.in_[FUNCT3]   == 0b101:   s.out.value = SRL
  //           elif   s.in_[FUNCT3]   == 0b110:   s.out.value = OR
  //           elif   s.in_[FUNCT3]   == 0b111:   s.out.value = AND
  //         elif     s.in_[FUNCT7]   == 0b0100000:
  //           if     s.in_[FUNCT3]   == 0b000:   s.out.value = SUB
  //           elif   s.in_[FUNCT3]   == 0b101:   s.out.value = SRA
  //         elif     s.in_[FUNCT7]   == 0b0000001:
  //           if     s.in_[FUNCT3]   == 0b000:   s.out.value = MUL
  //           elif   s.in_[FUNCT3]   == 0b001:   s.out.value = MULH
  //           elif   s.in_[FUNCT3]   == 0b010:   s.out.value = MULHSU
  //           elif   s.in_[FUNCT3]   == 0b011:   s.out.value = MULHU
  //           elif   s.in_[FUNCT3]   == 0b100:   s.out.value = DIV
  //           elif   s.in_[FUNCT3]   == 0b101:   s.out.value = DIVU
  //           elif   s.in_[FUNCT3]   == 0b110:   s.out.value = REM
  //           elif   s.in_[FUNCT3]   == 0b111:   s.out.value = REMU
  //       # OP (RV64I)
  //       elif       s.in_[OP]       == 0b0111011:
  //         if       s.in_[FUNCT7]   == 0b0000000:
  //           if     s.in_[FUNCT3]   == 0b000:   s.out.value = ADDW
  //           elif   s.in_[FUNCT3]   == 0b001:   s.out.value = SLLW
  //           elif   s.in_[FUNCT3]   == 0b011:   s.out.value = SLTU
  //           elif   s.in_[FUNCT3]   == 0b101:   s.out.value = SRLW
  //         elif     s.in_[FUNCT7]   == 0b0100000:
  //           if     s.in_[FUNCT3]   == 0b000:   s.out.value = SUBW
  //           elif   s.in_[FUNCT3]   == 0b101:   s.out.value = SRAW
  //         elif     s.in_[FUNCT7]   == 0b0000001:
  //           if     s.in_[FUNCT3]   == 0b000:   s.out.value = MULW
  //           elif   s.in_[FUNCT3]   == 0b100:   s.out.value = DIVW
  //           elif   s.in_[FUNCT3]   == 0b101:   s.out.value = DIVUW
  //           elif   s.in_[FUNCT3]   == 0b110:   s.out.value = REMW
  //           elif   s.in_[FUNCT3]   == 0b111:   s.out.value = REMUW
  //       # OP-IMM
  //       elif     s.in_[OP]   == 0b0010011:
  //         if     s.in_[FUNCT3]   == 0b000:     s.out.value = ADDI
  //         elif   s.in_[FUNCT3]   == 0b010:     s.out.value = SLTI
  //         elif   s.in_[FUNCT3]   == 0b011:     s.out.value = SLTIU
  //         elif   s.in_[FUNCT3]   == 0b100:     s.out.value = XORI
  //         elif   s.in_[FUNCT3]   == 0b110:     s.out.value = ORI
  //         elif   s.in_[FUNCT3]   == 0b111:     s.out.value = ANDI
  //         elif   s.in_[FUNCT3]   == 0b001:     s.out.value = SLLI
  //         elif   s.in_[FUNCT3]   == 0b101:
  //           if   s.in_[FUNCT7]   == 0b0000000: s.out.value = SRLI
  //           elif s.in_[FUNCT7]   == 0b0100000: s.out.value = SRAI
  //       # OP-IMM (RV64I)
  //       elif     s.in_[OP]   == 0b0011011:
  //         if     s.in_[FUNCT3]   == 0b000:     s.out.value = ADDIW
  //         elif   s.in_[FUNCT3]   == 0b010:     s.out.value = SLTIW
  //         elif   s.in_[FUNCT3]   == 0b001:     s.out.value = SLLIW
  //         elif   s.in_[FUNCT3]   == 0b101:
  //           if   s.in_[FUNCT7]   == 0b0000000: s.out.value = SRLIW
  //           elif s.in_[FUNCT7]   == 0b0100000: s.out.value = SRAIW
  //       # OP-STORE
  //       elif     s.in_[OP]   == 0b0100011:
  //         if     s.in_[FUNCT3] == 0b000:       s.out.value = SB
  //         elif   s.in_[FUNCT3] == 0b001:       s.out.value = SH
  //         elif   s.in_[FUNCT3] == 0b010:       s.out.value = SW
  //         elif   s.in_[FUNCT3] == 0b011:       s.out.value = SD
  //       # OP-LOAD
  //       elif     s.in_[OP]   == 0b0000011:
  //         if     s.in_[FUNCT3] == 0b000:       s.out.value = LB
  //         elif   s.in_[FUNCT3] == 0b001:       s.out.value = LH
  //         elif   s.in_[FUNCT3] == 0b010:       s.out.value = LW
  //         elif   s.in_[FUNCT3] == 0b100:       s.out.value = LBU
  //         elif   s.in_[FUNCT3] == 0b101:       s.out.value = LHU
  //         elif   s.in_[FUNCT3] == 0b110:       s.out.value = LWU
  //         elif   s.in_[FUNCT3] == 0b011:       s.out.value = LD
  //       # OP-BRANCH
  //       elif     s.in_[OP]   == 0b1100011:
  //         if     s.in_[FUNCT3]   == 0b000:     s.out.value = BEQ
  //         elif   s.in_[FUNCT3]   == 0b001:     s.out.value = BNE
  //         elif   s.in_[FUNCT3]   == 0b100:     s.out.value = BLT
  //         elif   s.in_[FUNCT3]   == 0b101:     s.out.value = BGE
  //         elif   s.in_[FUNCT3]   == 0b110:     s.out.value = BLTU
  //         elif   s.in_[FUNCT3]   == 0b111:     s.out.value = BGEU
  //       # OP-LUI
  //       elif     s.in_[OP]   == 0b0110111:     s.out.value = LUI
  //       # OP-AUIPC
  //       elif     s.in_[OP]   == 0b0010111:     s.out.value = AUIPC
  //       # OP-JAL
  //       elif     s.in_[OP]   == 0b1101111:     s.out.value = JAL
  //       # OP-JALR
  //       elif     s.in_[OP]   == 0b1100111:     s.out.value = JALR
  //       # CSR
  //       elif     s.in_[OP]   == 0b1110011:
  //         if     s.in_[FUNCT3]   == 0b001:     s.out.value = CSRRW
  //         elif   s.in_[FUNCT3]   == 0b010:     s.out.value = CSRRS
  //       # custom
  //       elif     s.in_[OP]   == 0b0001011:     s.out.value = CUSTOM0
  //       else:                                  s.out.value = NOP

  // logic for comb_logic()
  always @ (*) begin
    if ((in_ == 0)) begin
      out = NOP;
    end
    else begin
      if ((in_[(7)-1:0] == 51)) begin
        if ((in_[(32)-1:25] == 0)) begin
          if ((in_[(15)-1:12] == 0)) begin
            out = ADD;
          end
          else begin
            if ((in_[(15)-1:12] == 1)) begin
              out = SLL;
            end
            else begin
              if ((in_[(15)-1:12] == 2)) begin
                out = SLT;
              end
              else begin
                if ((in_[(15)-1:12] == 3)) begin
                  out = SLTU;
                end
                else begin
                  if ((in_[(15)-1:12] == 4)) begin
                    out = XOR;
                  end
                  else begin
                    if ((in_[(15)-1:12] == 5)) begin
                      out = SRL;
                    end
                    else begin
                      if ((in_[(15)-1:12] == 6)) begin
                        out = OR;
                      end
                      else begin
                        if ((in_[(15)-1:12] == 7)) begin
                          out = AND;
                        end
                        else begin
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end
        else begin
          if ((in_[(32)-1:25] == 32)) begin
            if ((in_[(15)-1:12] == 0)) begin
              out = SUB;
            end
            else begin
              if ((in_[(15)-1:12] == 5)) begin
                out = SRA;
              end
              else begin
              end
            end
          end
          else begin
            if ((in_[(32)-1:25] == 1)) begin
              if ((in_[(15)-1:12] == 0)) begin
                out = MUL;
              end
              else begin
                if ((in_[(15)-1:12] == 1)) begin
                  out = MULH;
                end
                else begin
                  if ((in_[(15)-1:12] == 2)) begin
                    out = MULHSU;
                  end
                  else begin
                    if ((in_[(15)-1:12] == 3)) begin
                      out = MULHU;
                    end
                    else begin
                      if ((in_[(15)-1:12] == 4)) begin
                        out = DIV;
                      end
                      else begin
                        if ((in_[(15)-1:12] == 5)) begin
                          out = DIVU;
                        end
                        else begin
                          if ((in_[(15)-1:12] == 6)) begin
                            out = REM;
                          end
                          else begin
                            if ((in_[(15)-1:12] == 7)) begin
                              out = REMU;
                            end
                            else begin
                            end
                          end
                        end
                      end
                    end
                  end
                end
              end
            end
            else begin
            end
          end
        end
      end
      else begin
        if ((in_[(7)-1:0] == 59)) begin
          if ((in_[(32)-1:25] == 0)) begin
            if ((in_[(15)-1:12] == 0)) begin
              out = ADDW;
            end
            else begin
              if ((in_[(15)-1:12] == 1)) begin
                out = SLLW;
              end
              else begin
                if ((in_[(15)-1:12] == 3)) begin
                  out = SLTU;
                end
                else begin
                  if ((in_[(15)-1:12] == 5)) begin
                    out = SRLW;
                  end
                  else begin
                  end
                end
              end
            end
          end
          else begin
            if ((in_[(32)-1:25] == 32)) begin
              if ((in_[(15)-1:12] == 0)) begin
                out = SUBW;
              end
              else begin
                if ((in_[(15)-1:12] == 5)) begin
                  out = SRAW;
                end
                else begin
                end
              end
            end
            else begin
              if ((in_[(32)-1:25] == 1)) begin
                if ((in_[(15)-1:12] == 0)) begin
                  out = MULW;
                end
                else begin
                  if ((in_[(15)-1:12] == 4)) begin
                    out = DIVW;
                  end
                  else begin
                    if ((in_[(15)-1:12] == 5)) begin
                      out = DIVUW;
                    end
                    else begin
                      if ((in_[(15)-1:12] == 6)) begin
                        out = REMW;
                      end
                      else begin
                        if ((in_[(15)-1:12] == 7)) begin
                          out = REMUW;
                        end
                        else begin
                        end
                      end
                    end
                  end
                end
              end
              else begin
              end
            end
          end
        end
        else begin
          if ((in_[(7)-1:0] == 19)) begin
            if ((in_[(15)-1:12] == 0)) begin
              out = ADDI;
            end
            else begin
              if ((in_[(15)-1:12] == 2)) begin
                out = SLTI;
              end
              else begin
                if ((in_[(15)-1:12] == 3)) begin
                  out = SLTIU;
                end
                else begin
                  if ((in_[(15)-1:12] == 4)) begin
                    out = XORI;
                  end
                  else begin
                    if ((in_[(15)-1:12] == 6)) begin
                      out = ORI;
                    end
                    else begin
                      if ((in_[(15)-1:12] == 7)) begin
                        out = ANDI;
                      end
                      else begin
                        if ((in_[(15)-1:12] == 1)) begin
                          out = SLLI;
                        end
                        else begin
                          if ((in_[(15)-1:12] == 5)) begin
                            if ((in_[(32)-1:25] == 0)) begin
                              out = SRLI;
                            end
                            else begin
                              if ((in_[(32)-1:25] == 32)) begin
                                out = SRAI;
                              end
                              else begin
                              end
                            end
                          end
                          else begin
                          end
                        end
                      end
                    end
                  end
                end
              end
            end
          end
          else begin
            if ((in_[(7)-1:0] == 27)) begin
              if ((in_[(15)-1:12] == 0)) begin
                out = ADDIW;
              end
              else begin
                if ((in_[(15)-1:12] == 2)) begin
                  out = SLTIW;
                end
                else begin
                  if ((in_[(15)-1:12] == 1)) begin
                    out = SLLIW;
                  end
                  else begin
                    if ((in_[(15)-1:12] == 5)) begin
                      if ((in_[(32)-1:25] == 0)) begin
                        out = SRLIW;
                      end
                      else begin
                        if ((in_[(32)-1:25] == 32)) begin
                          out = SRAIW;
                        end
                        else begin
                        end
                      end
                    end
                    else begin
                    end
                  end
                end
              end
            end
            else begin
              if ((in_[(7)-1:0] == 35)) begin
                if ((in_[(15)-1:12] == 0)) begin
                  out = SB;
                end
                else begin
                  if ((in_[(15)-1:12] == 1)) begin
                    out = SH;
                  end
                  else begin
                    if ((in_[(15)-1:12] == 2)) begin
                      out = SW;
                    end
                    else begin
                      if ((in_[(15)-1:12] == 3)) begin
                        out = SD;
                      end
                      else begin
                      end
                    end
                  end
                end
              end
              else begin
                if ((in_[(7)-1:0] == 3)) begin
                  if ((in_[(15)-1:12] == 0)) begin
                    out = LB;
                  end
                  else begin
                    if ((in_[(15)-1:12] == 1)) begin
                      out = LH;
                    end
                    else begin
                      if ((in_[(15)-1:12] == 2)) begin
                        out = LW;
                      end
                      else begin
                        if ((in_[(15)-1:12] == 4)) begin
                          out = LBU;
                        end
                        else begin
                          if ((in_[(15)-1:12] == 5)) begin
                            out = LHU;
                          end
                          else begin
                            if ((in_[(15)-1:12] == 6)) begin
                              out = LWU;
                            end
                            else begin
                              if ((in_[(15)-1:12] == 3)) begin
                                out = LD;
                              end
                              else begin
                              end
                            end
                          end
                        end
                      end
                    end
                  end
                end
                else begin
                  if ((in_[(7)-1:0] == 99)) begin
                    if ((in_[(15)-1:12] == 0)) begin
                      out = BEQ;
                    end
                    else begin
                      if ((in_[(15)-1:12] == 1)) begin
                        out = BNE;
                      end
                      else begin
                        if ((in_[(15)-1:12] == 4)) begin
                          out = BLT;
                        end
                        else begin
                          if ((in_[(15)-1:12] == 5)) begin
                            out = BGE;
                          end
                          else begin
                            if ((in_[(15)-1:12] == 6)) begin
                              out = BLTU;
                            end
                            else begin
                              if ((in_[(15)-1:12] == 7)) begin
                                out = BGEU;
                              end
                              else begin
                              end
                            end
                          end
                        end
                      end
                    end
                  end
                  else begin
                    if ((in_[(7)-1:0] == 55)) begin
                      out = LUI;
                    end
                    else begin
                      if ((in_[(7)-1:0] == 23)) begin
                        out = AUIPC;
                      end
                      else begin
                        if ((in_[(7)-1:0] == 111)) begin
                          out = JAL;
                        end
                        else begin
                          if ((in_[(7)-1:0] == 103)) begin
                            out = JALR;
                          end
                          else begin
                            if ((in_[(7)-1:0] == 115)) begin
                              if ((in_[(15)-1:12] == 1)) begin
                                out = CSRRW;
                              end
                              else begin
                                if ((in_[(15)-1:12] == 2)) begin
                                  out = CSRRS;
                                end
                                else begin
                                end
                              end
                            end
                            else begin
                              if ((in_[(7)-1:0] == 11)) begin
                                out = CUSTOM0;
                              end
                              else begin
                                out = NOP;
                              end
                            end
                          end
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
  end


endmodule // ProcDecodeRTL_0x417a6fa29460909d
`default_nettype wire

//-----------------------------------------------------------------------------
// DropUnitRTL_0x56bc1346a6f0c780
//-----------------------------------------------------------------------------
// nbits: 32
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module DropUnitRTL_0x56bc1346a6f0c780
(
  input  wire [   0:0] clk,
  input  wire [   0:0] drop,
  input  wire [  31:0] in__msg,
  output reg  [   0:0] in__rdy,
  input  wire [   0:0] in__val,
  output wire [  31:0] out_msg,
  input  wire [   0:0] out_rdy,
  output reg  [   0:0] out_val,
  input  wire [   0:0] reset
);

  // register declarations
  reg    [   0:0] do_wait__0;
  reg    [   0:0] in_go__0;
  reg    [   0:0] snoop_state$in_;

  // localparam declarations
  localparam SNOOP = 0;
  localparam WAIT = 1;

  // snoop_state temporaries
  wire   [   0:0] snoop_state$reset;
  wire   [   0:0] snoop_state$clk;
  wire   [   0:0] snoop_state$out;

  RegRst snoop_state
  (
    .reset ( snoop_state$reset ),
    .in_   ( snoop_state$in_ ),
    .clk   ( snoop_state$clk ),
    .out   ( snoop_state$out )
  );

  // signal connections
  assign out_msg           = in__msg;
  assign snoop_state$clk   = clk;
  assign snoop_state$reset = reset;


  // PYMTL SOURCE:
  //
  // @s.combinational
  // def state_transitions():
  //
  //       in_go   = s.in_.rdy and s.in_.val
  //       do_wait = s.drop    and not in_go
  //
  //       if s.snoop_state.out.value == SNOOP:
  //         if do_wait: s.snoop_state.in_.value = WAIT
  //         else:       s.snoop_state.in_.value = SNOOP
  //
  //       elif s.snoop_state.out == WAIT:
  //         if in_go: s.snoop_state.in_.value = SNOOP
  //         else:     s.snoop_state.in_.value = WAIT

  // logic for state_transitions()
  always @ (*) begin
    in_go__0 = (in__rdy&&in__val);
    do_wait__0 = (drop&&!in_go__0);
    if ((snoop_state$out == SNOOP)) begin
      if (do_wait__0) begin
        snoop_state$in_ = WAIT;
      end
      else begin
        snoop_state$in_ = SNOOP;
      end
    end
    else begin
      if ((snoop_state$out == WAIT)) begin
        if (in_go__0) begin
          snoop_state$in_ = SNOOP;
        end
        else begin
          snoop_state$in_ = WAIT;
        end
      end
      else begin
      end
    end
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def set_outputs():
  //
  //       if   s.snoop_state.out == SNOOP:
  //         s.out.val.value = s.in_.val and not s.drop
  //         s.in_.rdy.value = s.out.rdy
  //
  //       elif s.snoop_state.out == WAIT:
  //         s.out.val.value = 0
  //         s.in_.rdy.value = 1

  // logic for set_outputs()
  always @ (*) begin
    if ((snoop_state$out == SNOOP)) begin
      out_val = (in__val&&!drop);
      in__rdy = out_rdy;
    end
    else begin
      if ((snoop_state$out == WAIT)) begin
        out_val = 0;
        in__rdy = 1;
      end
      else begin
      end
    end
  end


endmodule // DropUnitRTL_0x56bc1346a6f0c780
`default_nettype wire

//-----------------------------------------------------------------------------
// RegRst
//-----------------------------------------------------------------------------
// dtype: 1
// reset_value: 0
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module RegRst
(
  input  wire [   0:0] clk,
  input  wire [   0:0] in_,
  output reg  [   0:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam reset_value = 0;



  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def seq_logic():
  //       if s.reset:
  //         s.out.next = reset_value
  //       else:
  //         s.out.next = s.in_

  // logic for seq_logic()
  always @ (posedge clk) begin
    if (reset) begin
      out <= reset_value;
    end
    else begin
      out <= in_;
    end
  end


endmodule // RegRst
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueue
//-----------------------------------------------------------------------------
// dtype: 47
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueue
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

  SingleElementBypassQueueCtrl ctrl
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

  SingleElementBypassQueueDpath dpath
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



endmodule // SingleElementBypassQueue
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueueCtrl
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueueCtrl
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


endmodule // SingleElementBypassQueueCtrl
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueueDpath
//-----------------------------------------------------------------------------
// dtype: 47
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueueDpath
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

  Mux bypass_mux
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

  RegEn queue
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



endmodule // SingleElementBypassQueueDpath
`default_nettype wire

//-----------------------------------------------------------------------------
// Mux
//-----------------------------------------------------------------------------
// dtype: 47
// nports: 2
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module Mux
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

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       assert s.sel < nports
  //       s.out.v = s.in_[ s.sel ]

  // logic for comb_logic()
  always @ (*) begin
    out = in_[sel];
  end


endmodule // Mux
`default_nettype wire

//-----------------------------------------------------------------------------
// RegEn
//-----------------------------------------------------------------------------
// dtype: 47
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module RegEn
(
  input  wire [   0:0] clk,
  input  wire [   0:0] en,
  input  wire [  46:0] in_,
  output reg  [  46:0] out,
  input  wire [   0:0] reset
);



  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def seq_logic():
  //       if s.en:
  //         s.out.next = s.in_

  // logic for seq_logic()
  always @ (posedge clk) begin
    if (en) begin
      out <= in_;
    end
    else begin
    end
  end


endmodule // RegEn
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueue_0x4c19e633b920d596
//-----------------------------------------------------------------------------
// dtype: 32
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueue_0x4c19e633b920d596
(
  input  wire [   0:0] clk,
  output wire [  31:0] deq_msg,
  input  wire [   0:0] deq_rdy,
  output wire [   0:0] deq_val,
  input  wire [  31:0] enq_msg,
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

  SingleElementBypassQueueCtrl ctrl
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
  wire   [  31:0] dpath$enq_bits;
  wire   [  31:0] dpath$deq_bits;

  SingleElementBypassQueueDpath_0x4c19e633b920d596 dpath
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



endmodule // SingleElementBypassQueue_0x4c19e633b920d596
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueueDpath_0x4c19e633b920d596
//-----------------------------------------------------------------------------
// dtype: 32
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueueDpath_0x4c19e633b920d596
(
  input  wire [   0:0] bypass_mux_sel,
  input  wire [   0:0] clk,
  output wire [  31:0] deq_bits,
  input  wire [  31:0] enq_bits,
  input  wire [   0:0] reset,
  input  wire [   0:0] wen
);

  // bypass_mux temporaries
  wire   [   0:0] bypass_mux$reset;
  wire   [  31:0] bypass_mux$in_$000;
  wire   [  31:0] bypass_mux$in_$001;
  wire   [   0:0] bypass_mux$clk;
  wire   [   0:0] bypass_mux$sel;
  wire   [  31:0] bypass_mux$out;

  Mux_0x7e8c65f0610ab9ca bypass_mux
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
  wire   [  31:0] queue$in_;
  wire   [   0:0] queue$clk;
  wire   [   0:0] queue$en;
  wire   [  31:0] queue$out;

  RegEn_0x1eed677bd3b5c175 queue
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



endmodule // SingleElementBypassQueueDpath_0x4c19e633b920d596
`default_nettype wire

//-----------------------------------------------------------------------------
// Mux_0x7e8c65f0610ab9ca
//-----------------------------------------------------------------------------
// dtype: 32
// nports: 2
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module Mux_0x7e8c65f0610ab9ca
(
  input  wire [   0:0] clk,
  input  wire [  31:0] in_$000,
  input  wire [  31:0] in_$001,
  output reg  [  31:0] out,
  input  wire [   0:0] reset,
  input  wire [   0:0] sel
);

  // localparam declarations
  localparam nports = 2;


  // array declarations
  wire   [  31:0] in_[0:1];
  assign in_[  0] = in_$000;
  assign in_[  1] = in_$001;

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       assert s.sel < nports
  //       s.out.v = s.in_[ s.sel ]

  // logic for comb_logic()
  always @ (*) begin
    out = in_[sel];
  end


endmodule // Mux_0x7e8c65f0610ab9ca
`default_nettype wire

//-----------------------------------------------------------------------------
// RegEn_0x1eed677bd3b5c175
//-----------------------------------------------------------------------------
// dtype: 32
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module RegEn_0x1eed677bd3b5c175
(
  input  wire [   0:0] clk,
  input  wire [   0:0] en,
  input  wire [  31:0] in_,
  output reg  [  31:0] out,
  input  wire [   0:0] reset
);



  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def seq_logic():
  //       if s.en:
  //         s.out.next = s.in_

  // logic for seq_logic()
  always @ (posedge clk) begin
    if (en) begin
      out <= in_;
    end
    else begin
    end
  end


endmodule // RegEn_0x1eed677bd3b5c175
`default_nettype wire

//-----------------------------------------------------------------------------
// ProcDpathRTL_0x5941394665257d0d
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module ProcDpathRTL_0x5941394665257d0d
(
  input  wire [   3:0] alu_fn_X,
  output wire [   0:0] br_cond_eq_X,
  output wire [   0:0] br_cond_lt_X,
  output wire [   0:0] br_cond_ltu_X,
  output wire [   0:0] br_cond_neg_X,
  output wire [   0:0] br_cond_zero_X,
  input  wire [   0:0] clk,
  output wire [  31:0] dmemreq_msg_addr,
  output wire [  31:0] dmemreq_msg_data,
  input  wire [  31:0] dmemresp_msg_data,
  input  wire [   0:0] ex_result_sel_X,
  output reg  [  76:0] imemreq_msg,
  input  wire [  31:0] imemresp_msg_data,
  output wire [  31:0] inst_D,
  input  wire [  31:0] mngr2proc_data,
  output wire [   0:0] mul_req_rdy_D,
  input  wire [   0:0] mul_req_val_D,
  input  wire [   0:0] mul_resp_rdy_X,
  output wire [   0:0] mul_resp_val_X,
  input  wire [   1:0] op0_byp_sel_D,
  input  wire [   1:0] op0_sel_D,
  input  wire [   1:0] op1_byp_sel_D,
  input  wire [   3:0] op1_sel_D,
  input  wire [   1:0] pc_sel_F,
  output wire [  31:0] proc2mngr_data,
  input  wire [   0:0] reg_en_D,
  input  wire [   0:0] reg_en_F,
  input  wire [   0:0] reg_en_M,
  input  wire [   0:0] reg_en_W,
  input  wire [   0:0] reg_en_X,
  input  wire [   0:0] reset,
  input  wire [   4:0] rf_waddr_W,
  input  wire [   0:0] rf_wen_W,
  output reg  [   0:0] stats_en,
  input  wire [   0:0] stats_en_wen_W,
  input  wire [   0:0] wb_result_sel_M
);

  // wire declarations
  wire   [  31:0] rf_rdata1_D;
  wire   [  31:0] rf_wdata_W;
  wire   [  31:0] byp_data_M;
  wire   [  31:0] byp_data_W;
  wire   [  31:0] byp_data_X;
  wire   [  31:0] pc_F;
  wire   [  63:0] mul_req_msg_D;
  wire   [  31:0] j_target_D;
  wire   [  31:0] inst_X;
  wire   [  31:0] pc_plus4_F;
  wire   [  31:0] br_target_X;
  wire   [  31:0] mul_resp_msg_X;
  wire   [  31:0] rf_rdata0_D;
  wire   [  31:0] jr_target_D;
  reg    [  31:0] inst_M;
  reg    [  31:0] inst_W;

  // register declarations
  reg    [  11:0] s_imm_D;

  // imm_sext_D temporaries
  wire   [   0:0] imm_sext_D$reset;
  wire   [  11:0] imm_sext_D$in_;
  wire   [   0:0] imm_sext_D$clk;
  wire   [  31:0] imm_sext_D$out;

  SignExtender_0x323011a57c347b27 imm_sext_D
  (
    .reset ( imm_sext_D$reset ),
    .in_   ( imm_sext_D$in_ ),
    .clk   ( imm_sext_D$clk ),
    .out   ( imm_sext_D$out )
  );

  // imul temporaries
  wire   [   0:0] imul$resp_rdy;
  wire   [   0:0] imul$clk;
  wire   [  63:0] imul$req_msg;
  wire   [   0:0] imul$req_val;
  wire   [   0:0] imul$reset;
  wire   [  31:0] imul$resp_msg;
  wire   [   0:0] imul$resp_val;
  wire   [   0:0] imul$req_rdy;
  
  // Weier: multiplier instantiation
  IntMul imul
  (
    .clk ( imul$clk ),
    .A   ( imul$req_msg[31:0] ),
    .B   ( imul$req_msg[63:32] ),
    .MULT ( imul$resp_msg ),
    .req_rdy ( imul$req_rdy ),
    .req_val ( imul$req_val ),
    .resp_rdy ( imul$resp_rdy ),
    .resp_val ( imul$resp_val )
  );
  
  /*
  IntMulVarLatPRTL_0x2b2428df45bfba3f imul
  (
    .resp_rdy ( imul$resp_rdy ),
    .clk      ( imul$clk ),
    .req_msg  ( imul$req_msg ),
    .req_val  ( imul$req_val ),
    .reset    ( imul$reset ),
    .resp_msg ( imul$resp_msg ),
    .resp_val ( imul$resp_val ),
    .req_rdy  ( imul$req_rdy )
  );
  */

  // uimm_zext_D temporaries
  wire   [   0:0] uimm_zext_D$reset;
  wire   [  19:0] uimm_zext_D$in_;
  wire   [   0:0] uimm_zext_D$clk;
  wire   [  31:0] uimm_zext_D$out;

  ZeroExtender_0x252563c6b652c06f uimm_zext_D
  (
    .reset ( uimm_zext_D$reset ),
    .in_   ( uimm_zext_D$in_ ),
    .clk   ( uimm_zext_D$clk ),
    .out   ( uimm_zext_D$out )
  );

  // pc_reg_D temporaries
  wire   [   0:0] pc_reg_D$reset;
  wire   [   0:0] pc_reg_D$en;
  wire   [   0:0] pc_reg_D$clk;
  wire   [  31:0] pc_reg_D$in_;
  wire   [  31:0] pc_reg_D$out;

  RegEnRst_0x3857337130dc0828 pc_reg_D
  (
    .reset ( pc_reg_D$reset ),
    .en    ( pc_reg_D$en ),
    .clk   ( pc_reg_D$clk ),
    .in_   ( pc_reg_D$in_ ),
    .out   ( pc_reg_D$out )
  );

  // pc_reg_F temporaries
  wire   [   0:0] pc_reg_F$reset;
  wire   [   0:0] pc_reg_F$en;
  wire   [   0:0] pc_reg_F$clk;
  wire   [  31:0] pc_reg_F$in_;
  wire   [  31:0] pc_reg_F$out;

  RegEnRst_0x6c5cfbd4c2d1e32c pc_reg_F
  (
    .reset ( pc_reg_F$reset ),
    .en    ( pc_reg_F$en ),
    .clk   ( pc_reg_F$clk ),
    .in_   ( pc_reg_F$in_ ),
    .out   ( pc_reg_F$out )
  );

  // op1_reg_X temporaries
  wire   [   0:0] op1_reg_X$reset;
  wire   [   0:0] op1_reg_X$en;
  wire   [   0:0] op1_reg_X$clk;
  wire   [  31:0] op1_reg_X$in_;
  wire   [  31:0] op1_reg_X$out;

  RegEnRst_0x3857337130dc0828 op1_reg_X
  (
    .reset ( op1_reg_X$reset ),
    .en    ( op1_reg_X$en ),
    .clk   ( op1_reg_X$clk ),
    .in_   ( op1_reg_X$in_ ),
    .out   ( op1_reg_X$out )
  );

  // j_target_clac_D temporaries
  wire   [   0:0] j_target_clac_D$clk;
  wire   [  31:0] j_target_clac_D$pc;
  wire   [  31:0] j_target_clac_D$inst;
  wire   [   0:0] j_target_clac_D$reset;
  wire   [  31:0] j_target_clac_D$j_target;

  JumpTargetCalcRTL_0x655d63971647128d j_target_clac_D
  (
    .clk      ( j_target_clac_D$clk ),
    .pc       ( j_target_clac_D$pc ),
    .inst     ( j_target_clac_D$inst ),
    .reset    ( j_target_clac_D$reset ),
    .j_target ( j_target_clac_D$j_target )
  );

  // alu_X temporaries
  wire   [   0:0] alu_X$clk;
  wire   [  31:0] alu_X$in0;
  wire   [  31:0] alu_X$in1;
  wire   [   3:0] alu_X$fn;
  wire   [   0:0] alu_X$reset;
  wire   [   0:0] alu_X$ops_lt;
  wire   [   0:0] alu_X$ops_ltu;
  wire   [   0:0] alu_X$op0_zero;
  wire   [  31:0] alu_X$out;
  wire   [   0:0] alu_X$ops_eq;
  wire   [   0:0] alu_X$op0_neg;

  AluRTL_0x655d63971647128d alu_X
  (
    .clk      ( alu_X$clk ),
    .in0      ( alu_X$in0 ),
    .in1      ( alu_X$in1 ),
    .fn       ( alu_X$fn ),
    .reset    ( alu_X$reset ),
    .ops_lt   ( alu_X$ops_lt ),
    .ops_ltu  ( alu_X$ops_ltu ),
    .op0_zero ( alu_X$op0_zero ),
    .out      ( alu_X$out ),
    .ops_eq   ( alu_X$ops_eq ),
    .op0_neg  ( alu_X$op0_neg )
  );

  // shamt_zext_D temporaries
  wire   [   0:0] shamt_zext_D$reset;
  wire   [   4:0] shamt_zext_D$in_;
  wire   [   0:0] shamt_zext_D$clk;
  wire   [  31:0] shamt_zext_D$out;

  ZeroExtender_0x23eb7c2c2c335618 shamt_zext_D
  (
    .reset ( shamt_zext_D$reset ),
    .in_   ( shamt_zext_D$in_ ),
    .clk   ( shamt_zext_D$clk ),
    .out   ( shamt_zext_D$out )
  );

  // ex_result_sel_mux_X temporaries
  wire   [   0:0] ex_result_sel_mux_X$reset;
  wire   [  31:0] ex_result_sel_mux_X$in_$000;
  wire   [  31:0] ex_result_sel_mux_X$in_$001;
  wire   [   0:0] ex_result_sel_mux_X$clk;
  wire   [   0:0] ex_result_sel_mux_X$sel;
  wire   [  31:0] ex_result_sel_mux_X$out;

  Mux_0x7e8c65f0610ab9ca ex_result_sel_mux_X
  (
    .reset   ( ex_result_sel_mux_X$reset ),
    .in_$000 ( ex_result_sel_mux_X$in_$000 ),
    .in_$001 ( ex_result_sel_mux_X$in_$001 ),
    .clk     ( ex_result_sel_mux_X$clk ),
    .sel     ( ex_result_sel_mux_X$sel ),
    .out     ( ex_result_sel_mux_X$out )
  );

  // stats_en_reg_W temporaries
  wire   [   0:0] stats_en_reg_W$reset;
  wire   [   0:0] stats_en_reg_W$en;
  wire   [   0:0] stats_en_reg_W$clk;
  wire   [  31:0] stats_en_reg_W$in_;
  wire   [  31:0] stats_en_reg_W$out;

  RegEnRst_0x3857337130dc0828 stats_en_reg_W
  (
    .reset ( stats_en_reg_W$reset ),
    .en    ( stats_en_reg_W$en ),
    .clk   ( stats_en_reg_W$clk ),
    .in_   ( stats_en_reg_W$in_ ),
    .out   ( stats_en_reg_W$out )
  );

  // rf temporaries
  wire   [   0:0] rf$write_en;
  wire   [   4:0] rf$read_addr0;
  wire   [   4:0] rf$read_addr1;
  wire   [   0:0] rf$clk;
  wire   [   4:0] rf$write_addr;
  wire   [  31:0] rf$write_data;
  wire   [   0:0] rf$reset;
  wire   [  31:0] rf$read_data1;
  wire   [  31:0] rf$read_data0;

  Regfile_0x2934358c954f09d3 rf
  (
    .write_en   ( rf$write_en ),
    .read_addr0 ( rf$read_addr0 ),
    .read_addr1 ( rf$read_addr1 ),
    .clk        ( rf$clk ),
    .write_addr ( rf$write_addr ),
    .write_data ( rf$write_data ),
    .reset      ( rf$reset ),
    .read_data1 ( rf$read_data1 ),
    .read_data0 ( rf$read_data0 )
  );

  // op0_sel_mux_D temporaries
  wire   [   0:0] op0_sel_mux_D$reset;
  wire   [  31:0] op0_sel_mux_D$in_$000;
  wire   [  31:0] op0_sel_mux_D$in_$001;
  wire   [  31:0] op0_sel_mux_D$in_$002;
  wire   [   0:0] op0_sel_mux_D$clk;
  wire   [   1:0] op0_sel_mux_D$sel;
  wire   [  31:0] op0_sel_mux_D$out;

  Mux_0x32d1d735b6f2dcf9 op0_sel_mux_D
  (
    .reset   ( op0_sel_mux_D$reset ),
    .in_$000 ( op0_sel_mux_D$in_$000 ),
    .in_$001 ( op0_sel_mux_D$in_$001 ),
    .in_$002 ( op0_sel_mux_D$in_$002 ),
    .clk     ( op0_sel_mux_D$clk ),
    .sel     ( op0_sel_mux_D$sel ),
    .out     ( op0_sel_mux_D$out )
  );

  // pc_sel_mux_F temporaries
  wire   [   0:0] pc_sel_mux_F$reset;
  wire   [  31:0] pc_sel_mux_F$in_$000;
  wire   [  31:0] pc_sel_mux_F$in_$001;
  wire   [  31:0] pc_sel_mux_F$in_$002;
  wire   [  31:0] pc_sel_mux_F$in_$003;
  wire   [   0:0] pc_sel_mux_F$clk;
  wire   [   1:0] pc_sel_mux_F$sel;
  wire   [  31:0] pc_sel_mux_F$out;

  Mux_0x7be03e4007003adc pc_sel_mux_F
  (
    .reset   ( pc_sel_mux_F$reset ),
    .in_$000 ( pc_sel_mux_F$in_$000 ),
    .in_$001 ( pc_sel_mux_F$in_$001 ),
    .in_$002 ( pc_sel_mux_F$in_$002 ),
    .in_$003 ( pc_sel_mux_F$in_$003 ),
    .clk     ( pc_sel_mux_F$clk ),
    .sel     ( pc_sel_mux_F$sel ),
    .out     ( pc_sel_mux_F$out )
  );

  // op1_byp_mux_D temporaries
  wire   [   0:0] op1_byp_mux_D$reset;
  wire   [  31:0] op1_byp_mux_D$in_$000;
  wire   [  31:0] op1_byp_mux_D$in_$001;
  wire   [  31:0] op1_byp_mux_D$in_$002;
  wire   [  31:0] op1_byp_mux_D$in_$003;
  wire   [   0:0] op1_byp_mux_D$clk;
  wire   [   1:0] op1_byp_mux_D$sel;
  wire   [  31:0] op1_byp_mux_D$out;

  Mux_0x7be03e4007003adc op1_byp_mux_D
  (
    .reset   ( op1_byp_mux_D$reset ),
    .in_$000 ( op1_byp_mux_D$in_$000 ),
    .in_$001 ( op1_byp_mux_D$in_$001 ),
    .in_$002 ( op1_byp_mux_D$in_$002 ),
    .in_$003 ( op1_byp_mux_D$in_$003 ),
    .clk     ( op1_byp_mux_D$clk ),
    .sel     ( op1_byp_mux_D$sel ),
    .out     ( op1_byp_mux_D$out )
  );

  // op0_byp_mux_D temporaries
  wire   [   0:0] op0_byp_mux_D$reset;
  wire   [  31:0] op0_byp_mux_D$in_$000;
  wire   [  31:0] op0_byp_mux_D$in_$001;
  wire   [  31:0] op0_byp_mux_D$in_$002;
  wire   [  31:0] op0_byp_mux_D$in_$003;
  wire   [   0:0] op0_byp_mux_D$clk;
  wire   [   1:0] op0_byp_mux_D$sel;
  wire   [  31:0] op0_byp_mux_D$out;

  Mux_0x7be03e4007003adc op0_byp_mux_D
  (
    .reset   ( op0_byp_mux_D$reset ),
    .in_$000 ( op0_byp_mux_D$in_$000 ),
    .in_$001 ( op0_byp_mux_D$in_$001 ),
    .in_$002 ( op0_byp_mux_D$in_$002 ),
    .in_$003 ( op0_byp_mux_D$in_$003 ),
    .clk     ( op0_byp_mux_D$clk ),
    .sel     ( op0_byp_mux_D$sel ),
    .out     ( op0_byp_mux_D$out )
  );

  // pc_incr_F temporaries
  wire   [   0:0] pc_incr_F$reset;
  wire   [  31:0] pc_incr_F$in_;
  wire   [   0:0] pc_incr_F$clk;
  wire   [  31:0] pc_incr_F$out;

  Incrementer_0x17b02585ea4ef9aa pc_incr_F
  (
    .reset ( pc_incr_F$reset ),
    .in_   ( pc_incr_F$in_ ),
    .clk   ( pc_incr_F$clk ),
    .out   ( pc_incr_F$out )
  );

  // s_imm_sext_D temporaries
  wire   [   0:0] s_imm_sext_D$reset;
  wire   [  11:0] s_imm_sext_D$in_;
  wire   [   0:0] s_imm_sext_D$clk;
  wire   [  31:0] s_imm_sext_D$out;

  SignExtender_0x323011a57c347b27 s_imm_sext_D
  (
    .reset ( s_imm_sext_D$reset ),
    .in_   ( s_imm_sext_D$in_ ),
    .clk   ( s_imm_sext_D$clk ),
    .out   ( s_imm_sext_D$out )
  );

  // inst_X_reg temporaries
  wire   [   0:0] inst_X_reg$reset;
  wire   [   0:0] inst_X_reg$en;
  wire   [   0:0] inst_X_reg$clk;
  wire   [  31:0] inst_X_reg$in_;
  wire   [  31:0] inst_X_reg$out;

  RegEnRst_0x3857337130dc0828 inst_X_reg
  (
    .reset ( inst_X_reg$reset ),
    .en    ( inst_X_reg$en ),
    .clk   ( inst_X_reg$clk ),
    .in_   ( inst_X_reg$in_ ),
    .out   ( inst_X_reg$out )
  );

  // br_target_calc_D temporaries
  wire   [   0:0] br_target_calc_D$clk;
  wire   [  31:0] br_target_calc_D$pc;
  wire   [  31:0] br_target_calc_D$inst;
  wire   [   0:0] br_target_calc_D$reset;
  wire   [  31:0] br_target_calc_D$br_target;

  BranchTargetCalcRTL_0x655d63971647128d br_target_calc_D
  (
    .clk       ( br_target_calc_D$clk ),
    .pc        ( br_target_calc_D$pc ),
    .inst      ( br_target_calc_D$inst ),
    .reset     ( br_target_calc_D$reset ),
    .br_target ( br_target_calc_D$br_target )
  );

  // op1_sel_mux_D temporaries
  wire   [   0:0] op1_sel_mux_D$reset;
  wire   [  31:0] op1_sel_mux_D$in_$000;
  wire   [  31:0] op1_sel_mux_D$in_$001;
  wire   [  31:0] op1_sel_mux_D$in_$002;
  wire   [  31:0] op1_sel_mux_D$in_$003;
  wire   [  31:0] op1_sel_mux_D$in_$004;
  wire   [  31:0] op1_sel_mux_D$in_$005;
  wire   [  31:0] op1_sel_mux_D$in_$006;
  wire   [  31:0] op1_sel_mux_D$in_$007;
  wire   [  31:0] op1_sel_mux_D$in_$008;
  wire   [   0:0] op1_sel_mux_D$clk;
  wire   [   3:0] op1_sel_mux_D$sel;
  wire   [  31:0] op1_sel_mux_D$out;

  Mux_0x1256f43349e9a82f op1_sel_mux_D
  (
    .reset   ( op1_sel_mux_D$reset ),
    .in_$000 ( op1_sel_mux_D$in_$000 ),
    .in_$001 ( op1_sel_mux_D$in_$001 ),
    .in_$002 ( op1_sel_mux_D$in_$002 ),
    .in_$003 ( op1_sel_mux_D$in_$003 ),
    .in_$004 ( op1_sel_mux_D$in_$004 ),
    .in_$005 ( op1_sel_mux_D$in_$005 ),
    .in_$006 ( op1_sel_mux_D$in_$006 ),
    .in_$007 ( op1_sel_mux_D$in_$007 ),
    .in_$008 ( op1_sel_mux_D$in_$008 ),
    .clk     ( op1_sel_mux_D$clk ),
    .sel     ( op1_sel_mux_D$sel ),
    .out     ( op1_sel_mux_D$out )
  );

  // imm_zext_D temporaries
  wire   [   0:0] imm_zext_D$reset;
  wire   [  11:0] imm_zext_D$in_;
  wire   [   0:0] imm_zext_D$clk;
  wire   [  31:0] imm_zext_D$out;

  ZeroExtender_0x323011a57c347b27 imm_zext_D
  (
    .reset ( imm_zext_D$reset ),
    .in_   ( imm_zext_D$in_ ),
    .clk   ( imm_zext_D$clk ),
    .out   ( imm_zext_D$out )
  );

  // pc_plus4_reg_D temporaries
  wire   [   0:0] pc_plus4_reg_D$reset;
  wire   [   0:0] pc_plus4_reg_D$en;
  wire   [   0:0] pc_plus4_reg_D$clk;
  wire   [  31:0] pc_plus4_reg_D$in_;
  wire   [  31:0] pc_plus4_reg_D$out;

  RegEnRst_0x3857337130dc0828 pc_plus4_reg_D
  (
    .reset ( pc_plus4_reg_D$reset ),
    .en    ( pc_plus4_reg_D$en ),
    .clk   ( pc_plus4_reg_D$clk ),
    .in_   ( pc_plus4_reg_D$in_ ),
    .out   ( pc_plus4_reg_D$out )
  );

  // jr_target_calc_D temporaries
  wire   [   0:0] jr_target_calc_D$reset;
  wire   [   0:0] jr_target_calc_D$clk;
  wire   [  31:0] jr_target_calc_D$rd;
  wire   [  31:0] jr_target_calc_D$i_imm_sext;
  wire   [  31:0] jr_target_calc_D$jr_target;

  JumpRegTargetCalcRTL_0x655d63971647128d jr_target_calc_D
  (
    .reset      ( jr_target_calc_D$reset ),
    .clk        ( jr_target_calc_D$clk ),
    .rd         ( jr_target_calc_D$rd ),
    .i_imm_sext ( jr_target_calc_D$i_imm_sext ),
    .jr_target  ( jr_target_calc_D$jr_target )
  );

  // wb_result_reg_W temporaries
  wire   [   0:0] wb_result_reg_W$reset;
  wire   [   0:0] wb_result_reg_W$en;
  wire   [   0:0] wb_result_reg_W$clk;
  wire   [  31:0] wb_result_reg_W$in_;
  wire   [  31:0] wb_result_reg_W$out;

  RegEnRst_0x3857337130dc0828 wb_result_reg_W
  (
    .reset ( wb_result_reg_W$reset ),
    .en    ( wb_result_reg_W$en ),
    .clk   ( wb_result_reg_W$clk ),
    .in_   ( wb_result_reg_W$in_ ),
    .out   ( wb_result_reg_W$out )
  );

  // wb_result_sel_mux_M temporaries
  wire   [   0:0] wb_result_sel_mux_M$reset;
  wire   [  31:0] wb_result_sel_mux_M$in_$000;
  wire   [  31:0] wb_result_sel_mux_M$in_$001;
  wire   [   0:0] wb_result_sel_mux_M$clk;
  wire   [   0:0] wb_result_sel_mux_M$sel;
  wire   [  31:0] wb_result_sel_mux_M$out;

  Mux_0x7e8c65f0610ab9ca wb_result_sel_mux_M
  (
    .reset   ( wb_result_sel_mux_M$reset ),
    .in_$000 ( wb_result_sel_mux_M$in_$000 ),
    .in_$001 ( wb_result_sel_mux_M$in_$001 ),
    .clk     ( wb_result_sel_mux_M$clk ),
    .sel     ( wb_result_sel_mux_M$sel ),
    .out     ( wb_result_sel_mux_M$out )
  );

  // dmem_wdata_reg_X temporaries
  wire   [   0:0] dmem_wdata_reg_X$reset;
  wire   [   0:0] dmem_wdata_reg_X$en;
  wire   [   0:0] dmem_wdata_reg_X$clk;
  wire   [  31:0] dmem_wdata_reg_X$in_;
  wire   [  31:0] dmem_wdata_reg_X$out;

  RegEnRst_0x3857337130dc0828 dmem_wdata_reg_X
  (
    .reset ( dmem_wdata_reg_X$reset ),
    .en    ( dmem_wdata_reg_X$en ),
    .clk   ( dmem_wdata_reg_X$clk ),
    .in_   ( dmem_wdata_reg_X$in_ ),
    .out   ( dmem_wdata_reg_X$out )
  );

  // ex_result_reg_M temporaries
  wire   [   0:0] ex_result_reg_M$reset;
  wire   [   0:0] ex_result_reg_M$en;
  wire   [   0:0] ex_result_reg_M$clk;
  wire   [  31:0] ex_result_reg_M$in_;
  wire   [  31:0] ex_result_reg_M$out;

  RegEnRst_0x3857337130dc0828 ex_result_reg_M
  (
    .reset ( ex_result_reg_M$reset ),
    .en    ( ex_result_reg_M$en ),
    .clk   ( ex_result_reg_M$clk ),
    .in_   ( ex_result_reg_M$in_ ),
    .out   ( ex_result_reg_M$out )
  );

  // br_target_reg_X temporaries
  wire   [   0:0] br_target_reg_X$reset;
  wire   [   0:0] br_target_reg_X$en;
  wire   [   0:0] br_target_reg_X$clk;
  wire   [  31:0] br_target_reg_X$in_;
  wire   [  31:0] br_target_reg_X$out;

  RegEnRst_0x3857337130dc0828 br_target_reg_X
  (
    .reset ( br_target_reg_X$reset ),
    .en    ( br_target_reg_X$en ),
    .clk   ( br_target_reg_X$clk ),
    .in_   ( br_target_reg_X$in_ ),
    .out   ( br_target_reg_X$out )
  );

  // inst_D_reg temporaries
  wire   [   0:0] inst_D_reg$reset;
  wire   [   0:0] inst_D_reg$en;
  wire   [   0:0] inst_D_reg$clk;
  wire   [  31:0] inst_D_reg$in_;
  wire   [  31:0] inst_D_reg$out;

  RegEnRst_0x3857337130dc0828 inst_D_reg
  (
    .reset ( inst_D_reg$reset ),
    .en    ( inst_D_reg$en ),
    .clk   ( inst_D_reg$clk ),
    .in_   ( inst_D_reg$in_ ),
    .out   ( inst_D_reg$out )
  );

  // op0_reg_X temporaries
  wire   [   0:0] op0_reg_X$reset;
  wire   [   0:0] op0_reg_X$en;
  wire   [   0:0] op0_reg_X$clk;
  wire   [  31:0] op0_reg_X$in_;
  wire   [  31:0] op0_reg_X$out;

  RegEnRst_0x3857337130dc0828 op0_reg_X
  (
    .reset ( op0_reg_X$reset ),
    .en    ( op0_reg_X$en ),
    .clk   ( op0_reg_X$clk ),
    .in_   ( op0_reg_X$in_ ),
    .out   ( op0_reg_X$out )
  );

  // signal connections
  assign alu_X$clk                   = clk;
  assign alu_X$fn                    = alu_fn_X;
  assign alu_X$in0                   = op0_reg_X$out;
  assign alu_X$in1                   = op1_reg_X$out;
  assign alu_X$reset                 = reset;
  assign br_cond_eq_X                = alu_X$ops_eq;
  assign br_cond_lt_X                = alu_X$ops_lt;
  assign br_cond_ltu_X               = alu_X$ops_ltu;
  assign br_cond_neg_X               = alu_X$op0_neg;
  assign br_cond_zero_X              = alu_X$op0_zero;
  assign br_target_X                 = br_target_reg_X$out;
  assign br_target_calc_D$clk        = clk;
  assign br_target_calc_D$inst       = inst_D;
  assign br_target_calc_D$pc         = pc_reg_D$out;
  assign br_target_calc_D$reset      = reset;
  assign br_target_reg_X$clk         = clk;
  assign br_target_reg_X$en          = reg_en_X;
  assign br_target_reg_X$in_         = br_target_calc_D$br_target;
  assign br_target_reg_X$reset       = reset;
  assign byp_data_M                  = wb_result_sel_mux_M$out;
  assign byp_data_W                  = wb_result_reg_W$out;
  assign byp_data_X                  = ex_result_sel_mux_X$out;
  assign dmem_wdata_reg_X$clk        = clk;
  assign dmem_wdata_reg_X$en         = reg_en_X;
  assign dmem_wdata_reg_X$in_        = op1_byp_mux_D$out;
  assign dmem_wdata_reg_X$reset      = reset;
  assign dmemreq_msg_addr            = alu_X$out;
  assign dmemreq_msg_data            = dmem_wdata_reg_X$out;
  assign ex_result_reg_M$clk         = clk;
  assign ex_result_reg_M$en          = reg_en_M;
  assign ex_result_reg_M$in_         = ex_result_sel_mux_X$out;
  assign ex_result_reg_M$reset       = reset;
  assign ex_result_sel_mux_X$clk     = clk;
  assign ex_result_sel_mux_X$in_$000 = alu_X$out;
  assign ex_result_sel_mux_X$in_$001 = mul_resp_msg_X;
  assign ex_result_sel_mux_X$reset   = reset;
  assign ex_result_sel_mux_X$sel     = ex_result_sel_X;
  assign imm_sext_D$clk              = clk;
  assign imm_sext_D$in_              = inst_D[31:20];
  assign imm_sext_D$reset            = reset;
  assign imm_zext_D$clk              = clk;
  assign imm_zext_D$in_              = inst_D[31:20];
  assign imm_zext_D$reset            = reset;
  assign imul$clk                    = clk;
  assign imul$req_msg[31:0]          = op1_sel_mux_D$out;
  assign imul$req_msg[63:32]         = op0_sel_mux_D$out;
  assign imul$req_val                = mul_req_val_D;
  assign imul$reset                  = reset;
  assign imul$resp_rdy               = mul_resp_rdy_X;
  assign inst_D                      = inst_D_reg$out;
  assign inst_D_reg$clk              = clk;
  assign inst_D_reg$en               = reg_en_D;
  assign inst_D_reg$in_              = imemresp_msg_data;
  assign inst_D_reg$reset            = reset;
  assign inst_X                      = inst_X_reg$out;
  assign inst_X_reg$clk              = clk;
  assign inst_X_reg$en               = reg_en_X;
  assign inst_X_reg$in_              = inst_D_reg$out;
  assign inst_X_reg$reset            = reset;
  assign j_target_D                  = j_target_clac_D$j_target;
  assign j_target_clac_D$clk         = clk;
  assign j_target_clac_D$inst        = inst_D;
  assign j_target_clac_D$pc          = pc_reg_D$out;
  assign j_target_clac_D$reset       = reset;
  assign jr_target_D                 = jr_target_calc_D$jr_target;
  assign jr_target_calc_D$clk        = clk;
  assign jr_target_calc_D$i_imm_sext = imm_sext_D$out;
  assign jr_target_calc_D$rd         = op0_byp_mux_D$out;
  assign jr_target_calc_D$reset      = reset;
  assign mul_req_rdy_D               = imul$req_rdy;
  assign mul_resp_msg_X              = imul$resp_msg;
  assign mul_resp_val_X              = imul$resp_val;
  assign op0_byp_mux_D$clk           = clk;
  assign op0_byp_mux_D$in_$000       = rf_rdata0_D;
  assign op0_byp_mux_D$in_$001       = byp_data_X;
  assign op0_byp_mux_D$in_$002       = byp_data_M;
  assign op0_byp_mux_D$in_$003       = byp_data_W;
  assign op0_byp_mux_D$reset         = reset;
  assign op0_byp_mux_D$sel           = op0_byp_sel_D;
  assign op0_reg_X$clk               = clk;
  assign op0_reg_X$en                = reg_en_X;
  assign op0_reg_X$in_               = op0_sel_mux_D$out;
  assign op0_reg_X$reset             = reset;
  assign op0_sel_mux_D$clk           = clk;
  assign op0_sel_mux_D$in_$000       = op0_byp_mux_D$out;
  assign op0_sel_mux_D$in_$001       = uimm_zext_D$out;
  assign op0_sel_mux_D$in_$002       = 32'd4;
  assign op0_sel_mux_D$reset         = reset;
  assign op0_sel_mux_D$sel           = op0_sel_D;
  assign op1_byp_mux_D$clk           = clk;
  assign op1_byp_mux_D$in_$000       = rf_rdata1_D;
  assign op1_byp_mux_D$in_$001       = byp_data_X;
  assign op1_byp_mux_D$in_$002       = byp_data_M;
  assign op1_byp_mux_D$in_$003       = byp_data_W;
  assign op1_byp_mux_D$reset         = reset;
  assign op1_byp_mux_D$sel           = op1_byp_sel_D;
  assign op1_reg_X$clk               = clk;
  assign op1_reg_X$en                = reg_en_X;
  assign op1_reg_X$in_               = op1_sel_mux_D$out;
  assign op1_reg_X$reset             = reset;
  assign op1_sel_mux_D$clk           = clk;
  assign op1_sel_mux_D$in_$000       = op1_byp_mux_D$out;
  assign op1_sel_mux_D$in_$001       = imm_sext_D$out;
  assign op1_sel_mux_D$in_$002       = imm_zext_D$out;
  assign op1_sel_mux_D$in_$003       = pc_reg_D$out;
  assign op1_sel_mux_D$in_$004       = mngr2proc_data;
  assign op1_sel_mux_D$in_$005       = shamt_zext_D$out;
  assign op1_sel_mux_D$in_$006       = 32'd12;
  assign op1_sel_mux_D$in_$007       = s_imm_sext_D$out;
  assign op1_sel_mux_D$in_$008       = pc_plus4_reg_D$out;
  assign op1_sel_mux_D$reset         = reset;
  assign op1_sel_mux_D$sel           = op1_sel_D;
  assign pc_F                        = pc_reg_F$out;
  assign pc_incr_F$clk               = clk;
  assign pc_incr_F$in_               = pc_F;
  assign pc_incr_F$reset             = reset;
  assign pc_plus4_F                  = pc_incr_F$out;
  assign pc_plus4_reg_D$clk          = clk;
  assign pc_plus4_reg_D$en           = reg_en_D;
  assign pc_plus4_reg_D$in_          = pc_plus4_F;
  assign pc_plus4_reg_D$reset        = reset;
  assign pc_reg_D$clk                = clk;
  assign pc_reg_D$en                 = reg_en_D;
  assign pc_reg_D$in_                = pc_F;
  assign pc_reg_D$reset              = reset;
  assign pc_reg_F$clk                = clk;
  assign pc_reg_F$en                 = reg_en_F;
  assign pc_reg_F$in_                = pc_sel_mux_F$out;
  assign pc_reg_F$reset              = reset;
  assign pc_sel_mux_F$clk            = clk;
  assign pc_sel_mux_F$in_$000        = pc_plus4_F;
  assign pc_sel_mux_F$in_$001        = br_target_X;
  assign pc_sel_mux_F$in_$002        = j_target_D;
  assign pc_sel_mux_F$in_$003        = jr_target_D;
  assign pc_sel_mux_F$reset          = reset;
  assign pc_sel_mux_F$sel            = pc_sel_F;
  assign proc2mngr_data              = wb_result_reg_W$out;
  assign rf$clk                      = clk;
  assign rf$read_addr0               = inst_D[19:15];
  assign rf$read_addr1               = inst_D[24:20];
  assign rf$reset                    = reset;
  assign rf$write_addr               = rf_waddr_W;
  assign rf$write_data               = rf_wdata_W;
  assign rf$write_en                 = rf_wen_W;
  assign rf_rdata0_D                 = rf$read_data0;
  assign rf_rdata1_D                 = rf$read_data1;
  assign rf_wdata_W                  = wb_result_reg_W$out;
  assign s_imm_sext_D$clk            = clk;
  assign s_imm_sext_D$in_            = s_imm_D;
  assign s_imm_sext_D$reset          = reset;
  assign shamt_zext_D$clk            = clk;
  assign shamt_zext_D$in_            = inst_D[24:20];
  assign shamt_zext_D$reset          = reset;
  assign stats_en_reg_W$clk          = clk;
  assign stats_en_reg_W$en           = stats_en_wen_W;
  assign stats_en_reg_W$in_          = wb_result_reg_W$out;
  assign stats_en_reg_W$reset        = reset;
  assign uimm_zext_D$clk             = clk;
  assign uimm_zext_D$in_             = inst_D[31:12];
  assign uimm_zext_D$reset           = reset;
  assign wb_result_reg_W$clk         = clk;
  assign wb_result_reg_W$en          = reg_en_W;
  assign wb_result_reg_W$in_         = wb_result_sel_mux_M$out;
  assign wb_result_reg_W$reset       = reset;
  assign wb_result_sel_mux_M$clk     = clk;
  assign wb_result_sel_mux_M$in_$000 = ex_result_reg_M$out;
  assign wb_result_sel_mux_M$in_$001 = dmemresp_msg_data;
  assign wb_result_sel_mux_M$reset   = reset;
  assign wb_result_sel_mux_M$sel     = wb_result_sel_M;


  // PYMTL SOURCE:
  //
  // @s.combinational
  // def imem_req_F():
  //       s.imemreq_msg.addr.value  = s.pc_sel_mux_F.out

  // Start dg524
  assign imemreq_msg[33:0] = 34'd0; //{len,data}
  assign imemreq_msg[73:66] = 8'd0; //opaque
  assign imemreq_msg[76:74] = 3'd0; //type
  // End dg524
  
  // logic for imem_req_F()
  always @ (*) begin
    imemreq_msg[(66)-1:34] = pc_sel_mux_F$out;
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_s_imm_D():
  //       s.s_imm_D.value = concat( s.inst_D[ FUNCT7 ], s.inst_D[ RD ] )

  // logic for comb_s_imm_D()
  always @ (*) begin
    s_imm_D = { inst_D[(32)-1:25],inst_D[(12)-1:7] };
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def stats_en_logic_W():
  //       s.stats_en.value = reduce_or( s.stats_en_reg_W.out ) # reduction with bitwise OR

  // logic for stats_en_logic_W()
  always @ (*) begin
    stats_en = (|stats_en_reg_W$out);
  end

  // add registers for linetracing 
  always@(posedge clk) begin
    if(reset) begin
      inst_M <= 0;
      inst_W <= 0;
    end else begin
      inst_M <= inst_X;
      inst_W <= inst_M;
    end
  end

endmodule // ProcDpathRTL_0x5941394665257d0d
`default_nettype wire

//-----------------------------------------------------------------------------
// SignExtender_0x323011a57c347b27
//-----------------------------------------------------------------------------
// out_nbits: 32
// in_nbits: 12
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SignExtender_0x323011a57c347b27
(
  input  wire [   0:0] clk,
  input  wire [  11:0] in_,
  output reg  [  31:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam out_nbits = 32;



  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       s.out.value = sext( s.in_, out_nbits )

  // logic for comb_logic()
  always @ (*) begin
    out = { { out_nbits-12 { in_[11] } }, in_[11:0] };
  end


endmodule // SignExtender_0x323011a57c347b27
`default_nettype wire

// Weier: 3-stage pipelined multiplier

(*mult_style="pipe_block"*)
module IntMul(clk, A, B, MULT, req_rdy, req_val, resp_rdy, resp_val);

  input clk;
  input [31:0] A;
  input [31:0] B;
  output [31:0] MULT;
  output req_rdy;
  input req_val;
  input resp_rdy;
  output resp_val;
  
  reg [63:0] MULT_reg;
  reg [31:0] a_in, b_in;
  wire [63:0] mult_res;
  reg [63:0] pipe_1, pipe_2, pipe_3;
  
  reg req_rdy, resp_val;
  
  assign mult_res = a_in * b_in;
  
  reg go;
  reg [2:0] count;
  
  always @(posedge clk) begin
    a_in <= A; 
    b_in <= B;
    pipe_1 <= mult_res;
    pipe_2 <= pipe_1;
    pipe_3 <= pipe_2;
    MULT_reg <= pipe_3;
    
    if (req_val) begin
      req_rdy <= 0;
      resp_val <= 0;
      go <= 1;
      count <= 0;
    end
    if (go) begin
      count <= count + 1;
    end
    if (count == 3'b100) begin
      req_rdy <= 1;
      resp_val <= 1;
      count <= 0;
      go <= 0;
    end
  end
  
  assign MULT = MULT_reg[31:0];
  
endmodule

//-----------------------------------------------------------------------------
// ZeroExtender_0x252563c6b652c06f
//-----------------------------------------------------------------------------
// out_nbits: 32
// in_nbits: 20
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module ZeroExtender_0x252563c6b652c06f
(
  input  wire [   0:0] clk,
  input  wire [  19:0] in_,
  output reg  [  31:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam out_nbits = 32;



  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       s.out.value = zext( s.in_, out_nbits )

  // logic for comb_logic()
  always @ (*) begin
    out = { { out_nbits-20 { 1'b0 } }, in_[19:0] };
  end


endmodule // ZeroExtender_0x252563c6b652c06f
`default_nettype wire

//-----------------------------------------------------------------------------
// RegEnRst_0x3857337130dc0828
//-----------------------------------------------------------------------------
// reset_value: 0
// dtype: 32
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module RegEnRst_0x3857337130dc0828
(
  input  wire [   0:0] clk,
  input  wire [   0:0] en,
  input  wire [  31:0] in_,
  output reg  [  31:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam reset_value = 0;



  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def seq_logic():
  //       if s.reset:
  //         s.out.next = reset_value
  //       elif s.en:
  //         s.out.next = s.in_

  // logic for seq_logic()
  always @ (posedge clk) begin
    if (reset) begin
      out <= reset_value;
    end
    else begin
      if (en) begin
        out <= in_;
      end
      else begin
      end
    end
  end


endmodule // RegEnRst_0x3857337130dc0828
`default_nettype wire

//-----------------------------------------------------------------------------
// RegEnRst_0x6c5cfbd4c2d1e32c
//-----------------------------------------------------------------------------
// reset_value: 508
// dtype: 32
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module RegEnRst_0x6c5cfbd4c2d1e32c
(
  input  wire [   0:0] clk,
  input  wire [   0:0] en,
  input  wire [  31:0] in_,
  output reg  [  31:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  // dg524
  // localparam reset_value = 508;
  localparam reset_value = 0;


  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def seq_logic():
  //       if s.reset:
  //         s.out.next = reset_value
  //       elif s.en:
  //         s.out.next = s.in_

  // logic for seq_logic()
  always @ (posedge clk) begin
    if (reset) begin
      out <= reset_value;
    end
    else begin
      if (en) begin
        out <= in_;
      end
      else begin
      end
    end
  end


endmodule // RegEnRst_0x6c5cfbd4c2d1e32c
`default_nettype wire

//-----------------------------------------------------------------------------
// JumpTargetCalcRTL_0x655d63971647128d
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module JumpTargetCalcRTL_0x655d63971647128d
(
  input  wire [   0:0] clk,
  input  wire [  31:0] inst,
  output reg  [  31:0] j_target,
  input  wire [  31:0] pc,
  input  wire [   0:0] reset
);

  // register declarations
  reg    [  20:0] uj_imm;

  // uj_imm_sext temporaries
  wire   [   0:0] uj_imm_sext$reset;
  wire   [  20:0] uj_imm_sext$in_;
  wire   [   0:0] uj_imm_sext$clk;
  wire   [  31:0] uj_imm_sext$out;

  SignExtender_0x3f9cb03298f3f758 uj_imm_sext
  (
    .reset ( uj_imm_sext$reset ),
    .in_   ( uj_imm_sext$in_ ),
    .clk   ( uj_imm_sext$clk ),
    .out   ( uj_imm_sext$out )
  );

  // signal connections
  assign uj_imm_sext$clk   = clk;
  assign uj_imm_sext$in_   = uj_imm;
  assign uj_imm_sext$reset = reset;


  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic_uj_imm():
  //       s.uj_imm.value = concat( s.inst[31], s.inst[12:20], s.inst[20], s.inst[21:31], Bits(1, 0) )

  // logic for comb_logic_uj_imm()
  always @ (*) begin
    uj_imm = { inst[31],inst[(20)-1:12],inst[20],inst[(31)-1:21],1'd0 };
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       s.j_target.value = s.uj_imm_sext.out + s.pc

  // logic for comb_logic()
  always @ (*) begin
    j_target = (uj_imm_sext$out+pc);
  end


endmodule // JumpTargetCalcRTL_0x655d63971647128d
`default_nettype wire

//-----------------------------------------------------------------------------
// SignExtender_0x3f9cb03298f3f758
//-----------------------------------------------------------------------------
// out_nbits: 32
// in_nbits: 21
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SignExtender_0x3f9cb03298f3f758
(
  input  wire [   0:0] clk,
  input  wire [  20:0] in_,
  output reg  [  31:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam out_nbits = 32;



  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       s.out.value = sext( s.in_, out_nbits )

  // logic for comb_logic()
  always @ (*) begin
    out = { { out_nbits-21 { in_[20] } }, in_[20:0] };
  end


endmodule // SignExtender_0x3f9cb03298f3f758
`default_nettype wire

//-----------------------------------------------------------------------------
// AluRTL_0x655d63971647128d
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module AluRTL_0x655d63971647128d
(
  input  wire [   0:0] clk,
  input  wire [   3:0] fn,
  input  wire [  31:0] in0,
  input  wire [  31:0] in1,
  output reg  [   0:0] op0_neg,
  output reg  [   0:0] op0_zero,
  output reg  [   0:0] ops_eq,
  output reg  [   0:0] ops_lt,
  output reg  [   0:0] ops_ltu,
  output reg  [  31:0] out,
  input  wire [   0:0] reset
);

  // wire declarations
  wire   [   0:0] sign;
  wire   [  31:0] diff;
  wire   [  31:0] ones;
  wire   [  31:0] mask;


  // register declarations
  reg    [   0:0] lt;
  reg    [   0:0] ltu;
  reg    [  32:0] tmp_a;
  reg    [  63:0] tmp_b;



  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //
  //       s.ltu.value = s.in0 < s.in1
  //
  //       s.tmp_a.value = sext( s.in0, 33 ) - sext( s.in1, 33 )
  //       s.lt.value    = s.tmp_a[32]
  //
  //       if   s.fn ==  0: s.out.value = s.in0 + s.in1       # ADD
  //       elif s.fn == 11: s.out.value = s.in0               # CP OP0
  //       elif s.fn == 12: s.out.value = s.in1               # CP OP1
  //
  //       elif s.fn ==  1: s.out.value = s.in0 - s.in1       # SUB
  //       elif s.fn ==  2: s.out.value = s.in0 << s.in1[0:4] # SLL
  //       elif s.fn ==  3: s.out.value = s.in0 | s.in1       # OR
  //
  //       elif s.fn ==  4: s.out.value = s.lt                # SLT
  //
  //       elif s.fn ==  5: s.out.value = s.ltu               # SLTU
  //       elif s.fn ==  6: s.out.value = s.in0 & s.in1       # AND
  //       elif s.fn ==  7: s.out.value = s.in0 ^ s.in1       # XOR
  //       elif s.fn ==  8: s.out.value = ~( s.in0 | s.in1 )  # NOR
  //       elif s.fn ==  9: s.out.value = s.in0 >> s.in1[0:4] # SRL
  //
  //       elif s.fn == 10:                                   # SRA
  //         s.tmp_b.value = sext( s.in0, 64 ) >> s.in1[0:4]
  //         s.out.value   = s.tmp_b[0:32]
  //
  //       elif s.fn == 13:                                   # AUI
  //         s.out.value = ( s.in0 << 12 ) + s.in1
  //
  //       else:            s.out.value = 0                   # Unknown
  //
  //       s.ops_eq.value   = ( s.in0 == s.in1 )
  //       s.op0_zero.value = ( s.in0 == 0 )
  //       s.op0_neg.value  = ( s.in0[31] == 1 )
  //       s.ops_lt.value   = s.lt
  //       s.ops_ltu.value  = s.ltu

  // logic for comb_logic()
  always @ (*) begin
    ltu = (in0 < in1);
    tmp_a = ({ { 33-32 { in0[31] } }, in0[31:0] }-{ { 33-32 { in1[31] } }, in1[31:0] });
    lt = tmp_a[32];
    if ((fn == 0)) begin
      out = (in0+in1);
    end
    else begin
      if ((fn == 11)) begin
        out = in0;
      end
      else begin
        if ((fn == 12)) begin
          out = in1;
        end
        else begin
          if ((fn == 1)) begin
            out = (in0-in1);
          end
          else begin
            if ((fn == 2)) begin
              out = (in0<<in1[(4)-1:0]);
            end
            else begin
              if ((fn == 3)) begin
                out = (in0|in1);
              end
              else begin
                if ((fn == 4)) begin
                  out = { 31'b0, lt };
                end
                else begin
                  if ((fn == 5)) begin
                    out = { 31'b0, ltu };
                  end
                  else begin
                    if ((fn == 6)) begin
                      out = (in0&in1);
                    end
                    else begin
                      if ((fn == 7)) begin
                        out = (in0^in1);
                      end
                      else begin
                        if ((fn == 8)) begin
                          out = ~(in0|in1);
                        end
                        else begin
                          if ((fn == 9)) begin
                            out = (in0>>in1[(4)-1:0]);
                          end
                          else begin
                            if ((fn == 10)) begin
                              tmp_b = ({ { 64-32 { in0[31] } }, in0[31:0] }>>in1[(4)-1:0]);
                              out = tmp_b[(32)-1:0];
                            end
                            else begin
                              if ((fn == 13)) begin
                                out = ((in0<<12)+in1);
                              end
                              else begin
                                out = 0;
                              end
                            end
                          end
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    ops_eq = (in0 == in1);
    op0_zero = (in0 == 0);
    op0_neg = (in0[31] == 1);
    ops_lt = lt;
    ops_ltu = ltu;
  end


endmodule // AluRTL_0x655d63971647128d
`default_nettype wire

//-----------------------------------------------------------------------------
// ZeroExtender_0x23eb7c2c2c335618
//-----------------------------------------------------------------------------
// out_nbits: 32
// in_nbits: 5
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module ZeroExtender_0x23eb7c2c2c335618
(
  input  wire [   0:0] clk,
  input  wire [   4:0] in_,
  output reg  [  31:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam out_nbits = 32;



  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       s.out.value = zext( s.in_, out_nbits )

  // logic for comb_logic()
  always @ (*) begin
    out = { { out_nbits-5 { 1'b0 } }, in_[4:0] };
  end


endmodule // ZeroExtender_0x23eb7c2c2c335618
`default_nettype wire

//-----------------------------------------------------------------------------
// Regfile_0x2934358c954f09d3
//-----------------------------------------------------------------------------
// dump-vcd: True
// verilator-xinit: zeros
`default_nettype none
module Regfile_0x2934358c954f09d3
(
  input  wire [   0:0] clk,
  input  wire [   4:0] read_addr0,
  input  wire [   4:0] read_addr1,
  output wire [  31:0] read_data0,
  output wire [  31:0] read_data1,
  input  wire [   0:0] reset,
  input  wire [   4:0] write_addr,
  input  wire [  31:0] write_data,
  input  wire [   0:0] write_en
);

  // Imported Verilog source from:
  // /work/zhang/users/wm226/RISC-V-CGRA/pymtl-rocket/sim/proc_teach/RegfileVRTL.v

  Regfile#(

  )  verilog_module
  (
    .clk        ( clk ),
    .read_addr0 ( read_addr0 ),
    .read_addr1 ( read_addr1 ),
    .read_data0 ( read_data0 ),
    .read_data1 ( read_data1 ),
    .reset      ( reset ),
    .write_addr ( write_addr ),
    .write_data ( write_data ),
    .write_en   ( write_en )
  );

endmodule // Regfile_0x2934358c954f09d3
`default_nettype wire

//-----------------------------------------------------------------------------
// Mux_0x32d1d735b6f2dcf9
//-----------------------------------------------------------------------------
// nports: 3
// dtype: 32
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module Mux_0x32d1d735b6f2dcf9
(
  input  wire [   0:0] clk,
  input  wire [  31:0] in_$000,
  input  wire [  31:0] in_$001,
  input  wire [  31:0] in_$002,
  output reg  [  31:0] out,
  input  wire [   0:0] reset,
  input  wire [   1:0] sel
);

  // localparam declarations
  localparam nports = 3;


  // array declarations
  wire   [  31:0] in_[0:2];
  assign in_[  0] = in_$000;
  assign in_[  1] = in_$001;
  assign in_[  2] = in_$002;

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       assert s.sel < nports
  //       s.out.v = s.in_[ s.sel ]

  // logic for comb_logic()
  always @ (*) begin
    out = in_[sel];
  end


endmodule // Mux_0x32d1d735b6f2dcf9
`default_nettype wire

//-----------------------------------------------------------------------------
// Mux_0x7be03e4007003adc
//-----------------------------------------------------------------------------
// nports: 4
// dtype: 32
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module Mux_0x7be03e4007003adc
(
  input  wire [   0:0] clk,
  input  wire [  31:0] in_$000,
  input  wire [  31:0] in_$001,
  input  wire [  31:0] in_$002,
  input  wire [  31:0] in_$003,
  output reg  [  31:0] out,
  input  wire [   0:0] reset,
  input  wire [   1:0] sel
);

  // localparam declarations
  localparam nports = 4;


  // array declarations
  wire   [  31:0] in_[0:3];
  assign in_[  0] = in_$000;
  assign in_[  1] = in_$001;
  assign in_[  2] = in_$002;
  assign in_[  3] = in_$003;

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       assert s.sel < nports
  //       s.out.v = s.in_[ s.sel ]

  // logic for comb_logic()
  always @ (*) begin
    out = in_[sel];
  end


endmodule // Mux_0x7be03e4007003adc
`default_nettype wire

//-----------------------------------------------------------------------------
// Incrementer_0x17b02585ea4ef9aa
//-----------------------------------------------------------------------------
// nbits: 32
// increment_amount: 4
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module Incrementer_0x17b02585ea4ef9aa
(
  input  wire [   0:0] clk,
  input  wire [  31:0] in_,
  output reg  [  31:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam increment_amount = 4;



  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       s.out.value = s.in_ + increment_amount

  // logic for comb_logic()
  always @ (*) begin
    out = (in_+increment_amount);
  end


endmodule // Incrementer_0x17b02585ea4ef9aa
`default_nettype wire

//-----------------------------------------------------------------------------
// BranchTargetCalcRTL_0x655d63971647128d
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module BranchTargetCalcRTL_0x655d63971647128d
(
  output reg  [  31:0] br_target,
  input  wire [   0:0] clk,
  input  wire [  31:0] inst,
  input  wire [  31:0] pc,
  input  wire [   0:0] reset
);

  // register declarations
  reg    [  12:0] sb_imm;

  // sb_imm_sext temporaries
  wire   [   0:0] sb_imm_sext$reset;
  wire   [  12:0] sb_imm_sext$in_;
  wire   [   0:0] sb_imm_sext$clk;
  wire   [  31:0] sb_imm_sext$out;

  SignExtender_0x2b427ae37fcf14d0 sb_imm_sext
  (
    .reset ( sb_imm_sext$reset ),
    .in_   ( sb_imm_sext$in_ ),
    .clk   ( sb_imm_sext$clk ),
    .out   ( sb_imm_sext$out )
  );

  // signal connections
  assign sb_imm_sext$clk   = clk;
  assign sb_imm_sext$in_   = sb_imm;
  assign sb_imm_sext$reset = reset;


  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic_sb_imm():
  //       s.sb_imm.value = concat( s.inst[31], s.inst[7], s.inst[25:31], s.inst[8:12], Bits(1, 0) )

  // logic for comb_logic_sb_imm()
  always @ (*) begin
    sb_imm = { inst[31],inst[7],inst[(31)-1:25],inst[(12)-1:8],1'd0 };
  end

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       s.br_target.value = s.pc + s.sb_imm_sext.out

  // logic for comb_logic()
  always @ (*) begin
    br_target = (pc+sb_imm_sext$out);
  end


endmodule // BranchTargetCalcRTL_0x655d63971647128d
`default_nettype wire

//-----------------------------------------------------------------------------
// SignExtender_0x2b427ae37fcf14d0
//-----------------------------------------------------------------------------
// out_nbits: 32
// in_nbits: 13
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SignExtender_0x2b427ae37fcf14d0
(
  input  wire [   0:0] clk,
  input  wire [  12:0] in_,
  output reg  [  31:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam out_nbits = 32;



  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       s.out.value = sext( s.in_, out_nbits )

  // logic for comb_logic()
  always @ (*) begin
    out = { { out_nbits-13 { in_[12] } }, in_[12:0] };
  end


endmodule // SignExtender_0x2b427ae37fcf14d0
`default_nettype wire

//-----------------------------------------------------------------------------
// Mux_0x1256f43349e9a82f
//-----------------------------------------------------------------------------
// nports: 9
// dtype: 32
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module Mux_0x1256f43349e9a82f
(
  input  wire [   0:0] clk,
  input  wire [  31:0] in_$000,
  input  wire [  31:0] in_$001,
  input  wire [  31:0] in_$002,
  input  wire [  31:0] in_$003,
  input  wire [  31:0] in_$004,
  input  wire [  31:0] in_$005,
  input  wire [  31:0] in_$006,
  input  wire [  31:0] in_$007,
  input  wire [  31:0] in_$008,
  output reg  [  31:0] out,
  input  wire [   0:0] reset,
  input  wire [   3:0] sel
);

  // localparam declarations
  localparam nports = 9;


  // array declarations
  wire   [  31:0] in_[0:8];
  assign in_[  0] = in_$000;
  assign in_[  1] = in_$001;
  assign in_[  2] = in_$002;
  assign in_[  3] = in_$003;
  assign in_[  4] = in_$004;
  assign in_[  5] = in_$005;
  assign in_[  6] = in_$006;
  assign in_[  7] = in_$007;
  assign in_[  8] = in_$008;

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       assert s.sel < nports
  //       s.out.v = s.in_[ s.sel ]

  // logic for comb_logic()
  always @ (*) begin
    out = in_[sel];
  end


endmodule // Mux_0x1256f43349e9a82f
`default_nettype wire

//-----------------------------------------------------------------------------
// ZeroExtender_0x323011a57c347b27
//-----------------------------------------------------------------------------
// out_nbits: 32
// in_nbits: 12
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module ZeroExtender_0x323011a57c347b27
(
  input  wire [   0:0] clk,
  input  wire [  11:0] in_,
  output reg  [  31:0] out,
  input  wire [   0:0] reset
);

  // localparam declarations
  localparam out_nbits = 32;



  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       s.out.value = zext( s.in_, out_nbits )

  // logic for comb_logic()
  always @ (*) begin
    out = { { out_nbits-12 { 1'b0 } }, in_[11:0] };
  end


endmodule // ZeroExtender_0x323011a57c347b27
`default_nettype wire

//-----------------------------------------------------------------------------
// JumpRegTargetCalcRTL_0x655d63971647128d
//-----------------------------------------------------------------------------
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module JumpRegTargetCalcRTL_0x655d63971647128d
(
  input  wire [   0:0] clk,
  input  wire [  31:0] i_imm_sext,
  output reg  [  31:0] jr_target,
  input  wire [  31:0] rd,
  input  wire [   0:0] reset
);



  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       s.jr_target.value = ( s.rd + s.i_imm_sext ) & Bits( 32, 0xFFFFFFFE )

  // logic for comb_logic()
  always @ (*) begin
    jr_target = ((rd+i_imm_sext)&32'd4294967294);
  end


endmodule // JumpRegTargetCalcRTL_0x655d63971647128d
`default_nettype wire

//-----------------------------------------------------------------------------
// TwoElementBypassQueue_0x4b339266d56df875
//-----------------------------------------------------------------------------
// dtype: 77
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module TwoElementBypassQueue_0x4b339266d56df875
(
  input  wire [   0:0] clk,
  output wire [  76:0] deq_msg,
  input  wire [   0:0] deq_rdy,
  output wire [   0:0] deq_val,
  output reg  [   0:0] empty,
  input  wire [  76:0] enq_msg,
  output wire [   0:0] enq_rdy,
  input  wire [   0:0] enq_val,
  output reg  [   0:0] full,
  input  wire [   0:0] reset
);

  // queue1 temporaries
  wire   [   0:0] queue1$clk;
  wire   [  76:0] queue1$enq_msg;
  wire   [   0:0] queue1$enq_val;
  wire   [   0:0] queue1$reset;
  wire   [   0:0] queue1$deq_rdy;
  wire   [   0:0] queue1$enq_rdy;
  wire   [   0:0] queue1$full;
  wire   [  76:0] queue1$deq_msg;
  wire   [   0:0] queue1$deq_val;

  SingleElementBypassQueue_0x4b339266d56df875 queue1
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
  wire   [  76:0] queue0$enq_msg;
  wire   [   0:0] queue0$enq_val;
  wire   [   0:0] queue0$reset;
  wire   [   0:0] queue0$deq_rdy;
  wire   [   0:0] queue0$enq_rdy;
  wire   [   0:0] queue0$full;
  wire   [  76:0] queue0$deq_msg;
  wire   [   0:0] queue0$deq_val;

  SingleElementBypassQueue_0x4b339266d56df875 queue0
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


  // PYMTL SOURCE:
  //
  // @s.combinational
  // def full_empty():
  //       s.full.value  = s.queue0.full & s.queue1.full
  //       s.empty.value = (~s.queue0.full) & (~s.queue1.full)

  // logic for full_empty()
  always @ (*) begin
    full = (queue0$full&queue1$full);
    empty = (~queue0$full&~queue1$full);
  end


endmodule // TwoElementBypassQueue_0x4b339266d56df875
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueue_0x4b339266d56df875
//-----------------------------------------------------------------------------
// dtype: 77
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueue_0x4b339266d56df875
(
  input  wire [   0:0] clk,
  output wire [  76:0] deq_msg,
  input  wire [   0:0] deq_rdy,
  output wire [   0:0] deq_val,
  input  wire [  76:0] enq_msg,
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

  SingleElementBypassQueueCtrl ctrl
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
  wire   [  76:0] dpath$enq_bits;
  wire   [  76:0] dpath$deq_bits;

  SingleElementBypassQueueDpath_0x4b339266d56df875 dpath
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



endmodule // SingleElementBypassQueue_0x4b339266d56df875
`default_nettype wire

//-----------------------------------------------------------------------------
// SingleElementBypassQueueDpath_0x4b339266d56df875
//-----------------------------------------------------------------------------
// dtype: 77
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module SingleElementBypassQueueDpath_0x4b339266d56df875
(
  input  wire [   0:0] bypass_mux_sel,
  input  wire [   0:0] clk,
  output wire [  76:0] deq_bits,
  input  wire [  76:0] enq_bits,
  input  wire [   0:0] reset,
  input  wire [   0:0] wen
);

  // bypass_mux temporaries
  wire   [   0:0] bypass_mux$reset;
  wire   [  76:0] bypass_mux$in_$000;
  wire   [  76:0] bypass_mux$in_$001;
  wire   [   0:0] bypass_mux$clk;
  wire   [   0:0] bypass_mux$sel;
  wire   [  76:0] bypass_mux$out;

  Mux_0x53660fd12c3652eb bypass_mux
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
  wire   [  76:0] queue$in_;
  wire   [   0:0] queue$clk;
  wire   [   0:0] queue$en;
  wire   [  76:0] queue$out;

  RegEn_0x3ccbb0ba79955e96 queue
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



endmodule // SingleElementBypassQueueDpath_0x4b339266d56df875
`default_nettype wire

//-----------------------------------------------------------------------------
// Mux_0x53660fd12c3652eb
//-----------------------------------------------------------------------------
// dtype: 77
// nports: 2
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module Mux_0x53660fd12c3652eb
(
  input  wire [   0:0] clk,
  input  wire [  76:0] in_$000,
  input  wire [  76:0] in_$001,
  output reg  [  76:0] out,
  input  wire [   0:0] reset,
  input  wire [   0:0] sel
);

  // localparam declarations
  localparam nports = 2;


  // array declarations
  wire   [  76:0] in_[0:1];
  assign in_[  0] = in_$000;
  assign in_[  1] = in_$001;

  // PYMTL SOURCE:
  //
  // @s.combinational
  // def comb_logic():
  //       assert s.sel < nports
  //       s.out.v = s.in_[ s.sel ]

  // logic for comb_logic()
  always @ (*) begin
    out = in_[sel];
  end


endmodule // Mux_0x53660fd12c3652eb
`default_nettype wire

//-----------------------------------------------------------------------------
// RegEn_0x3ccbb0ba79955e96
//-----------------------------------------------------------------------------
// dtype: 77
// dump-vcd: False
// verilator-xinit: zeros
`default_nettype none
module RegEn_0x3ccbb0ba79955e96
(
  input  wire [   0:0] clk,
  input  wire [   0:0] en,
  input  wire [  76:0] in_,
  output reg  [  76:0] out,
  input  wire [   0:0] reset
);



  // PYMTL SOURCE:
  //
  // @s.posedge_clk
  // def seq_logic():
  //       if s.en:
  //         s.out.next = s.in_

  // logic for seq_logic()
  always @ (posedge clk) begin
    if (en) begin
      out <= in_;
    end
    else begin
    end
  end


endmodule // RegEn_0x3ccbb0ba79955e96
`default_nettype wire

//------------------------------------------------------------------------
// RegfileVRTL.v
// A register file module with two read ports and one write port. It
// utilizes BRAM when synthesized.
// Author: Tianjie Sun
//------------------------------------------------------------------------

module Regfile
#(
  parameter p_data_nbits  = 32,
  parameter p_num_entries = 32,

  // Local constants not meant to be set from outside the module
  parameter c_addr_nbits  = $clog2(p_num_entries)
)(
  input                     clk,   // primary clock
  input                     reset,

  // Read port 0 (reading on the rising clockb edge)

  input  wire [c_addr_nbits-1:0] read_addr0,
  output      [p_data_nbits-1:0] read_data0,

  // Read port 1 (reading on the rising clockb edge)

  input  wire [c_addr_nbits-1:0] read_addr1,
  output      [p_data_nbits-1:0] read_data1,

  // Write port (writing on the rising clocka edge)

  input                          write_en,
  input  wire [c_addr_nbits-1:0] write_addr,
  input  wire [p_data_nbits-1:0] write_data
);
  
  wire clk_bar;
  assign clk_bar = ~clk;
  (* ram_style = "block", DONT_TOUCH = "true" *) reg [p_data_nbits-1:0] ram1[p_num_entries-1:0];
  //(* ram_style = "block" *)reg[p_data_nbits-1:0]ram2[p_num_entries-1:0];
  
  always @( posedge clk )
    if ( write_en ) begin
      ram1[write_addr] <= write_data;
      //ram2[write_addr] <= write_data;
    end
    
  assign read_data0 = ( read_addr0 == 5'd0 ) ? 32'd0 : ram1[read_addr0];
  assign read_data1 = ( read_addr1 == 5'd0 ) ? 32'd0 : ram1[read_addr1];


endmodule
