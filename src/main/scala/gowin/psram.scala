package gowin

import chisel3._
import chisel3.util._
import chisel3.experimental._

import cpu.DmemPortIo

// GOWIN PSRAM Memory Interface IP User Guide (IPUG525)
// https://www.gowinsemi.com/upload/database_doc/2051/document/626a1159cde03.pdf
//
/** PSRAM instatiation template
	PSRAM_Memory_Interface_HS_Top your_instance_name(
		.clk(clk_i), //input clk
		.memory_clk(memory_clk_i), //input memory_clk
		.pll_lock(pll_lock_i), //input pll_lock
		.rst_n(rst_n_i), //input rst_n
		.O_psram_ck(O_psram_ck_o), //output [1:0] O_psram_ck
		.O_psram_ck_n(O_psram_ck_n_o), //output [1:0] O_psram_ck_n
		.IO_psram_dq(IO_psram_dq_io), //inout [15:0] IO_psram_dq
		.IO_psram_rwds(IO_psram_rwds_io), //inout [1:0] IO_psram_rwds
		.O_psram_cs_n(O_psram_cs_n_o), //output [1:0] O_psram_cs_n
		.O_psram_reset_n(O_psram_reset_n_o), //output [1:0] O_psram_reset_n
		.wr_data(wr_data_i), //input [63:0] wr_data
		.rd_data(rd_data_o), //output [63:0] rd_data
		.rd_data_valid(rd_data_valid_o), //output rd_data_valid
		.addr(addr_i), //input [20:0] addr
		.cmd(cmd_i), //input cmd
		.cmd_en(cmd_en_i), //input cmd_en
		.init_calib(init_calib_o), //output init_calib
		.clk_out(clk_out_o), //output clk_out
		.data_mask(data_mask_i) //input [7:0] data_mask
	);
*/

class PSRAMMemoryInterface(ipName: String = "PSRAM_Memory_Interface_HS_Top") extends BlackBox {
  override val desiredName = ipName
  
  val io = IO(new Bundle{
    val clk = Input(Clock())
    val memory_clk = Input(Clock())
    val pll_lock = Input(Bool())
    val rst_n = Input(Bool())
    val O_psram_ck = Output(UInt(2.W))
    val O_psram_ck_n = Output(UInt(2.W))
    val IO_psram_dq = Analog(16.W)
    val IO_psram_rwds = Analog(2.W)
    val O_psram_cs_n = Output(UInt(2.W))
    val O_psram_reset_n = Output(UInt(2.W))
    val wr_data = Input(UInt(64.W))
    val rd_data = Output(UInt(64.W))
    val rd_data_valid = Output(Bool())
    val addr = Input(UInt(21.W))
    val cmd = Input(Bool())
    val cmd_en = Input(Bool())
    val init_calib = Output(Bool())
    val clk_out = Output(Clock())
    val data_mask = Input(UInt(8.W))
  })
}

class PSRAMMemoryInterfacePort(addressBits: Int = 21, dataBits: Int = 64) extends Bundle {
  val init_calib = Input(Bool())
  val addr = Output(UInt(addressBits.W))
  val cmd = Output(Bool())
  val cmd_en = Output(Bool())
  val rd_data = Input(UInt(dataBits.W))
  val rd_data_valid = Input(Bool())
  val wr_data = Output(UInt(64.W))
  val data_mask = Output(UInt((dataBits/8).W))
}
object PSRAMMemoryInterfacePort {
  def apply(addressBits: Int = 21, dataBits: Int = 64): PSRAMMemoryInterfacePort = {
    new PSRAMMemoryInterfacePort(addressBits, dataBits)
  }
}

class DmemPortIoPSRAMBridge(addressBits: Int = 21, dataBits: Int = 64, psramBurstCount: Int = 16) extends Module {
  val io = IO(new Bundle {
    val psram = PSRAMMemoryInterfacePort(addressBits, dataBits)
    val mem = new DmemPortIo
  })

  val commandInterval = dataBits match {
    case 16 => 15
    case 32 => 19
    case 64 => 27
    case 128 => 43
    case _ => { assert(false, "dataBits must be 16, 32, 64 or 128."); 0 /* dummy */  }
  }

  val addr = RegInit(0.U(addressBits.W))
  val cmd = RegInit(false.B)
  val cmdEn = RegInit(false.B)
  val wrData = RegInit(0.U(dataBits.W))
  val wrDataMask = RegInit(0.U((dataBits/8).W))

  val controllerBurstCount = psramBurstCount / 4 // The controller reads/writes 4 words per cycle, thus the number of burst cycles in the controller is a quarter of PSRAM burst cycles.

  io.psram.addr := addr
  io.psram.cmd := cmd
  io.psram.cmd_en := cmdEn
  io.psram.wr_data := wrData
  io.psram.data_mask := wrDataMask
  cmdEn := false.B  // cmd_en must be deasserted after every asserted cycle.

  object State extends ChiselEnum {
    val Calibrating, Idle, StartFill, Filling, StartFlush, Flushing = Value
  }

  val cmdRead = false.B
  val cmdWrite = true.B
  val state = RegInit(State.Calibrating)
  val cmdIntervalCount = RegInit(0.U(log2Ceil(commandInterval + 1).W))
  
  when(cmdIntervalCount > 0.U) {
    cmdIntervalCount := cmdIntervalCount - 1.U
  }
  
  def memWordToPsramAddress(memWordAddress: UInt): UInt = {
    memWordAddress << 1
  }
  def psramToMemWordAddress(psramAddress: UInt): UInt = {
    psramAddress >> 1
  }

  val burstBufferLengthInMemWords = psramBurstCount/2
  val burstBufferOffsetMask = (burstBufferLengthInMemWords - 1).U(addressBits.W)
  val burstBufferAddressMask = ~burstBufferOffsetMask
  val burstBaseMemAddressStart = RegInit(0.U(addressBits.W))
  val burstBaseMemAddressEnd = RegInit(0.U(addressBits.W))
  val burstBuffer = Mem(psramBurstCount/2, UInt(32.W))
  val burstBufferValid = RegInit(0.U((burstBufferLengthInMemWords).W))        // valid bits for every words
  val burstBufferDirtyBytes = RegInit(0.U((burstBufferLengthInMemWords*4).W)) // dirty bits for every bytes

  val memRValid = RegInit(false.B)
  val memRData = RegInit(0.U(32.W))
  io.mem.rvalid := memRValid
  io.mem.rdata := memRData

  val memWordAddress = io.mem.addr >> 2
  val memBurstBufferOffset = memWordAddress & burstBufferOffsetMask
  val memIsPartialWrite = !io.mem.wstrb.andR
  val isMemAddressBurstBufferRange = burstBaseMemAddressStart <= memWordAddress && memWordAddress <= burstBaseMemAddressEnd
  // The burst buffer is valid and the address range currently loaded into the buffer is in range.
  val isMemReadFromBuffer = burstBufferValid(memBurstBufferOffset) && isMemAddressBurstBufferRange
  // For write access, we have to check the burst buffer is not dirty. If the buffer is dirty and the address to write is not within the buffer range, the buffer must be flushed.
  val isMemWriteToBuffer = burstBufferDirtyBytes === 0.U || (burstBufferDirtyBytes =/= 0.U && isMemAddressBurstBufferRange)
  val memByteMask = Cat((0 to 3).map(byte => Fill(8, io.mem.wstrb(byte))).reverse)
  val canAccessToBuffer = state === State.Idle

  memRValid := false.B
  when(io.mem.ren && isMemReadFromBuffer && canAccessToBuffer ) {
    // The burst buffer contains the target address data.
    memRValid := true.B
    memRData := burstBuffer(memBurstBufferOffset)
  }
  when(io.mem.wen && isMemWriteToBuffer && canAccessToBuffer ) {
    // The burst buffer is empty or contains the target address data.
    burstBufferValid := (burstBufferDirtyBytes >> (memBurstBufferOffset * 4.U)) === "b1111".U // If the whole word is filled. this word must be marked as "valid"
    burstBufferDirtyBytes := burstBufferDirtyBytes | (io.mem.wstrb << (memBurstBufferOffset * 4.U))
    burstBaseMemAddressStart := memWordAddress & burstBufferAddressMask
    burstBaseMemAddressEnd := (memWordAddress & burstBufferAddressMask) | burstBufferOffsetMask
    // Update the target bytes in the buffer.
    burstBuffer(memBurstBufferOffset) := (burstBuffer(memBurstBufferOffset) & ~memByteMask) | (io.mem.wdata & memByteMask)
  }
  
  val burstCount = RegInit(0.U(log2Ceil(controllerBurstCount).W))

  switch( state ) {
    is(State.Calibrating) {
      when( io.psram.init_calib ) {  // Wait until calibration completes.
        state := State.Idle
      }
    }
    is(State.Idle) {
      when( cmdIntervalCount > 0.U ) {
        when( io.mem.ren && !isMemReadFromBuffer ) {
          when( burstBufferDirtyBytes === 0.U )  { // The burst buffer is not dirty. We can load the target burst range to the buffer.
            state := State.StartFill
          } .otherwise {  // We must flush the buffer.
            state := State.StartFlush
          }
        } .elsewhen( io.mem.wen && !isMemWriteToBuffer ) {  // Cannot write to the buffer because the buffer is occupied by the another address range.
          state := State.StartFlush
        }
      }
    }
    is(State.StartFill) {
      burstCount := 0.U
      cmd := cmdRead
      cmdEn := true.B
      addr := memWordToPsramAddress(memWordAddress)
      cmdIntervalCount := commandInterval.U
      state := State.Filling
    }
    is(State.Filling) {
      when(io.psram.rd_data_valid) {
        burstCount := burstCount + 1.U
        val burstIndexBase = burstCount << 1
        val burstUpperWordIndex = burstIndexBase | 1.U
        val burstLowerWordIndex = burstIndexBase | 0.U
        burstBuffer(burstUpperWordIndex) := io.psram.rd_data(63, 32)
        burstBuffer(burstLowerWordIndex) := io.psram.rd_data(31, 0)
        when(burstCount === (controllerBurstCount - 1).U ) {
          burstBufferValid := Fill(burstBufferLengthInMemWords, 1.U(1.W))
          burstBufferDirtyBytes := 0.U
          state := State.Idle
        }
      }
    }
    is(State.StartFlush) {
      burstCount := 1.U // The first burst is written within this cycle.
      cmd := cmdWrite
      cmdEn := true.B
      addr := memWordToPsramAddress(memWordAddress)
      wrData := Cat(burstBuffer(1), burstBuffer(0))
      wrDataMask := burstBufferDirtyBytes(7, 0)
      cmdIntervalCount := commandInterval.U
      state := State.Flushing
    }
    is(State.Flushing) {
      val burstIndexBase = burstCount << 1
      val burstUpperWordIndex = burstIndexBase | 1.U
      val burstLowerWordIndex = burstIndexBase | 0.U
      wrData := Cat(burstBuffer(burstUpperWordIndex), burstBuffer(burstLowerWordIndex))
      wrDataMask := (burstBufferDirtyBytes >> (burstCount * 8.U)) & "b11111111".U
      burstCount := burstCount + 1.U
      when(burstCount === (controllerBurstCount - 1).U) {
        burstBufferDirtyBytes := 0.U // Now the burst buffer is flushed, clear the dirty bits.
        state := State.Idle
      }
    }
  }
}