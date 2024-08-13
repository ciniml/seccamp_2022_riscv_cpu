// SPDX-License-Identifier: BSL-1.0
// Copyright Kenta Ida 2021.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)

package fpga

import chisel3._
import _root_.circt.stage.ChiselStage
import cpu.Top

object Elaborate_ComProcCpuBoard extends App {
  ChiselStage.emitSystemVerilogFile(
    new Top, 
    Array("--target-dir", "rtl/comproc_cpu_board"),
    Array("--lowering-options=disallowLocalVariables")
  )
}

