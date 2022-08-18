package gowin

import chisel3._
import chisel3.util._
import chiseltest._
import scala.util.control.Breaks
import org.scalatest.flatspec.AnyFlatSpec
import cpu.DmemPortIo

class PsramTestSystem extends Module {
    val io = IO(new Bundle{
        val mem = new DmemPortIo
    })

    val psram = Module(new SimPSRAM)
    val bridge = Module(new DmemPortIoPSRAMBridge)

    psram.io.psram <> bridge.io.psram
    bridge.io.mem <> io.mem
}

class PsramTest extends AnyFlatSpec with ChiselScalatestTester {

    def runWriteReadTest(c: PsramTestSystem, numberOfWords: Int, doShuffleRead: Boolean, doShuffleWrite: Boolean, seed: Int = 0x1234) = {
        val random = new scala.util.Random(seed)
        val totalCycles = 10000
        c.reset.poke(true.B)
        c.io.mem.ren.poke(false.B)
        c.io.mem.wen.poke(false.B)
        c.clock.step(4)
        c.reset.poke(false.B)
        c.clock.setTimeout(totalCycles + 10)
        c.clock.step(10)
        
        val sequentialList = 0 to numberOfWords - 1
        val writeList = if(doShuffleWrite) { random.shuffle(sequentialList) } else { sequentialList }
        val readList = if(doShuffleRead) { random.shuffle(sequentialList) } else { sequentialList }
        for(word <- writeList) {
            c.io.mem.addr.poke((word*4).U)
            c.io.mem.wdata.poke((0 to 1).map(i => BigInt(word * 2 + i) << 16*i).reduce((l, r) => l | r).U)
            c.io.mem.wstrb.poke("b1111".U)
            c.io.mem.wen.poke(true.B)
            fork.withRegion(Monitor) {
                while(!c.io.mem.wready.peekBoolean) {
                    c.clock.step(1)
                }
            } .joinAndStep(c.clock)
            c.io.mem.wen.poke(false.B)
        }
        for(word <- readList) {
            c.io.mem.addr.poke((word*4).U)
            c.io.mem.ren.poke(true.B)
            fork.withRegion(Monitor) {
                while(!c.io.mem.rvalid.peekBoolean) {
                    c.clock.step(1)
                }
                val actual = c.io.mem.rdata.peekInt()
                val expected = (0 to 1).map(i => BigInt(word * 2 + i) << 16*i).reduce((l, r) => l | r)
                assert(actual == expected, f"Data mismatch at ${word*4}%08x, expected: ${expected}%08x actual: ${actual}%08x")
            } .joinAndStep(c.clock)
            
            c.io.mem.ren.poke(false.B)
        }
    }

    case class ReadWriteInfo(address: Int, write: Boolean, value: BigInt = 0)

    def runSimultaneousWriteReadTest(c: PsramTestSystem, numberOfWords: Int, seed: Int) = {
        val random = new scala.util.Random(seed)
        val totalCycles = 10000
        c.reset.poke(true.B)
        c.io.mem.ren.poke(false.B)
        c.io.mem.wen.poke(false.B)
        c.clock.step(4)
        c.reset.poke(false.B)
        c.clock.setTimeout(totalCycles + 10)
        c.clock.step(10)
        
        val sequentialList = 0 to numberOfWords - 1
        // Initialize
        for(word <- sequentialList) {
            c.io.mem.addr.poke((word*4).U)
            c.io.mem.wdata.poke((0 to 1).map(i => BigInt(word * 2 + i) << 16*i).reduce((l, r) => l | r).U)
            c.io.mem.wstrb.poke("b1111".U)
            c.io.mem.wen.poke(true.B)
            fork.withRegion(Monitor) {
                while(!c.io.mem.wready.peekBoolean) {
                    c.clock.step(1)
                }
            } .joinAndStep(c.clock)
            c.io.mem.wen.poke(false.B)
        }
        val memory = sequentialList.map(word => (0 to 1).map(i => BigInt(word * 2 + i) << 16*i).reduce((l, r) => l | r)).toBuffer
        val writeList = sequentialList.map(address => new ReadWriteInfo(address, true, BigInt(32, random)))
        val readList = sequentialList.map(address => new ReadWriteInfo(address, false))
        val shuffledList = random.shuffle(writeList ++ readList)
        for(info <- shuffledList) {
            if( info.write ) {
                memory.update(info.address, info.value)
                c.io.mem.addr.poke((info.address*4).U)
                c.io.mem.wdata.poke(info.value.U)
                c.io.mem.wstrb.poke("b1111".U)
                c.io.mem.wen.poke(true.B)
                fork.withRegion(Monitor) {
                    while(!c.io.mem.wready.peekBoolean) {
                        c.clock.step(1)
                    }
                } .joinAndStep(c.clock)
                c.io.mem.wen.poke(false.B)
            } else {
                c.io.mem.addr.poke((info.address*4).U)
                c.io.mem.ren.poke(true.B)
                fork.withRegion(Monitor) {
                    while(!c.io.mem.rvalid.peekBoolean) {
                        c.clock.step(1)
                    }
                    val actual = c.io.mem.rdata.peekInt()
                    val expected = memory(info.address)
                    assert(actual == expected, f"Data mismatch at ${info.address*4}%08x, expected: ${expected}%08x actual: ${actual}%08x")
                } .joinAndStep(c.clock)
                
                c.io.mem.ren.poke(false.B)
            }
        }
    }
    it must "runs Psram" in { 
    test(new PsramTestSystem)
            //.withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) 
        { c =>
            runWriteReadTest(c, 128, false, false)
        } 
    }
    it must "runs Psram Random Read" in { 
    test(new PsramTestSystem)
            //.withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) 
        { c =>
            runWriteReadTest(c, 128, true, false)
        } 
    }
    it must "runs Psram Random Write" in { 
    test(new PsramTestSystem)
            //.withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) 
        { c =>
            runWriteReadTest(c, 128, false, true)
        } 
    }
    it must "runs Psram Random All" in { 
    test(new PsramTestSystem)
            //.withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) 
        { c =>
            runWriteReadTest(c, 128, true, true)
        } 
    }
    it must "runs Psram Simultaneous Random" in { 
    test(new PsramTestSystem)
            //.withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) 
        { c =>
            runSimultaneousWriteReadTest(c, 128, 0xdeadbeef)
        } 
    }
}
