package cpu

import chiseltest._
import scala.util.control.Breaks
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._

import common.Consts.WORD_LEN
import gowin._
import java.nio.file.Path


class MemTestSystem(memoryPathGen: Int => String = i => f"sw/memtest_${i}.hex", suppressDebugMessage: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val success = Output(Bool())
    val exit = Output(Bool())
  })
  val baseAddress = BigInt("00000000", 16)
  val memSize = 8192
  val core = Module(new Core(startAddress = baseAddress.U(WORD_LEN.W), suppressDebugMessage))
  val decoder = Module(new DMemDecoder(Seq(
    (BigInt(0x00000000L), BigInt(memSize)), // メモリ
    (BigInt(0x40000000L), BigInt(64)),      // GPIO
    (BigInt(0xA0000000L), BigInt(2<<21)),   // PSRAM

  )))
  
  val memory = Module(new Memory(Some(memoryPathGen), baseAddress.U(WORD_LEN.W), memSize))
  val gpio = Module(new Gpio)
  val psramBridge = Module(new DmemPortIoPSRAMBridge())
  val psram = Module(new SimPSRAM())

  core.io.imem <> memory.io.imem
  core.io.dmem <> decoder.io.initiator  // CPUにデコーダを接続
  decoder.io.targets(0) <> memory.io.dmem       // 0番ポートにメモリを接続
  decoder.io.targets(1) <> gpio.io.mem          // 1番ポートにGPIOを接続
  decoder.io.targets(2) <> psramBridge.io.mem   // 2番ポートにPSRAMを接続
  
  psramBridge.io.psram <> psram.io.psram  // PSRAMをブリッジに接続
  
  io.exit := gpio.io.out(0)
  io.success := gpio.io.out(1)
}

class MemTest extends AnyFlatSpec with ChiselScalatestTester {
  def runMemTest(c: MemTestSystem, timeout: Int = 10000, checkInterval: Int = 100) = {
    c.reset.poke(true.B)
    c.clock.step(4)
    c.reset.poke(false.B)
    c.clock.setTimeout(timeout + checkInterval)
    while( !c.io.exit.peekBoolean ) {
      c.clock.step(checkInterval)
    }
    assert(c.io.success.peekBoolean)
  }

  it must "runs MemTest Sequential" in { test(new MemTestSystem(i => f"src/test/scala/cpu/sw/memtest_sequential_${i}.hex", true)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) { c =>
    runMemTest(c)
  } }
  it must "runs MemTest Random" in { test(new MemTestSystem(i => f"src/test/scala/cpu/sw/memtest_random_${i}.hex", true)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) { c =>
    runMemTest(c, 20000)
  } }

}
