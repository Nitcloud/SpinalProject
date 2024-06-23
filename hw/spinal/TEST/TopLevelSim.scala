package TEST

import spinal.core._
import spinal.core.sim._

object addSimEnvApp extends App {
    val dut = SimConfig.withWave.compile(addSimEnv(10, 8))
    dut.doSim{ dut =>
        dut.simEnvStart()
        dut.insertData(data(3, 5))
        dut.waitSimDone()
    }
}
