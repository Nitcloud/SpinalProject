package TEST

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import scala.collection.mutable.Queue
import scala.util.Random._
// import org.scalatest

case class inPort(dataWidth:Int=8) extends Bundle {
    val data1 = UInt(dataWidth bits)
    val data2 = UInt(dataWidth bits)
}

class flowTest() extends Component {
    val request = Flow(Bits(8 bits))
    val answer  = Flow(Bits(8 bits))
    val storage = Reg(Bits(8 bits)) init 0

    val fsm = new StateMachine {
        answer.setIdle()

        val idle: State = new State with EntryPoint {
                whenIsActive {
                when(request.valid) {
                    storage := request.payload
                    goto(sendEcho)
                }
            }
        }

        val sendEcho: State = new State {
            whenIsActive {
                answer.push(storage)
                goto(idle)
            }
        }
    }

    // 等价地
    // answer <-< request
}

class add(dataWidth:Int) extends Component {
    val dataIn = slave Flow(inPort(dataWidth))
    val sum = master Flow(UInt( width = 8 bits ))
    val sumCol = new Area{
        sum.valid := RegNext(dataIn.valid, False)
        sum.payload := RegNextWhen(dataIn.data1 + dataIn.data2, dataIn.valid)
    }
}

case class data(
    data1 : Int,
    data2 : Int,
    var sum : Int = 0
)

case class addSimEnv(period:Int, dataWidth : Int) extends add(dataWidth) {
    private val dataTestQueue = Queue[data]()
    private val sumRefQueue = Queue[data]()
    private val simResltQueue = Queue[Int]()

    private def init = {
        clockDomain.forkStimulus(period)
        dataIn.valid #= false
        clockDomain.waitSampling(count = 10)
    }

    private def referenceModel(dataTest : data) = {
        dataTest.sum = dataTest.data1 + dataTest.data2
        sumRefQueue.enqueue(dataTest)
    }

    private def portInDriver() = {
        val drv =  fork{
            while (true) {
                if(dataTestQueue.nonEmpty) {
                    val dataTest = dataTestQueue.dequeue()
                    referenceModel(dataTest)
                    dataIn.valid #= true
                    dataIn.data1 #= dataTest.data1
                    dataIn.data2 #= dataTest.data2
                    clockDomain.waitSampling()
                    dataIn.valid #= false
                } else {
                    clockDomain.waitSampling()
                }
            }
        }
    }

    private def portOutMon() = {
        val mon = fork{
            while(true) {
                if(sum.valid.toBoolean) {
                    simResltQueue.enqueue(sum.payload.toInt)
                }
                clockDomain.waitSampling()
            }
        }
    }

    private def scoreBoard() = {
        val score = fork{
            while(true) {
                if(sumRefQueue.nonEmpty && simResltQueue.nonEmpty) {
                    val sumRef = sumRefQueue.dequeue()
                    val sumActula = simResltQueue.dequeue()
                    assert(sumRef.sum == sumActula, message = s"data Mismatch ref:${sumRef},actual sum value $sumActula")
                }
                clockDomain.waitSampling()
            }
        }
    }

    def simEnvStart() = {
        init
        portInDriver
        portOutMon
        scoreBoard
    }

    def waitSimDone() = {
        clockDomain.waitSampling(count = 10)
        while(sumRefQueue.nonEmpty || simResltQueue.nonEmpty) {
            clockDomain.waitSampling(count = 10)
        }
    }

    def insertData(dataTest:data) = {
        dataTestQueue.enqueue(dataTest)
    }
}

object TopLevelVerilog extends App {
    Config.spinal.generateVerilog(new flowTest())
}
// class addSimTb extends FunSuite {
//     val dut=SimConfig.withWave.withCoverage.compile(addSimEnv(10,8))
//     test( testName = "data1为0，data2取0至最大值之间的随机值" ) {
//         dut.doSim{ dut =>
//             dut.simEnvStart()
//             for(i<-0 until 100) {
//                 dut.insertData(data(0, nextInt(256)))
//             }
//             dut.waitSimDone()
//         }
//     }
//     test( testName = "data1为最大值，data2取0至最大值之间的随机值" ) {
//         dut.doSim{ dut =>
//             dut.simEnvStart()
//             for(i<-0 until 100) {
//                 dut.insertData(data(256, nextInt(256)))
//             }
//             dut.waitSimDone()
//         }
//     }
//     test( testName = "data2取0，data1取0值至最大值之间的随机值" ){
//         dut.doSim{ dut =>
//             dut.simEnvStart()
//             for(i<-0 until 100) {
//                 dut.insertData(data(nextInt(256), 0))
//             }
//             dut.waitSimDone()
//         }
//     }
//     test( testName = "data2取最大值，data1取0值至最大值之间的随机值" ){
//         dut.doSim{ dut =>
//             dut.simEnvStart()
//             for(i<-0 until 100) {
//                 dut.insertData(data(nextInt(256), 256))
//             }
//             dut.waitSimDone()
//         }
//     }
//     test( testName = "data1，data2随机取值" ){
//         dut.doSim{ dut =>
//             dut.simEnvStart()
//             for(i<-0 until 100) {
//                 dut.insertData(data(nextInt(256), nextInt(256)))
//             }
//             dut.waitSimDone()
//         }
//     }
// }



// object TopLevelVhdl extends App {
//     Config.spinal.generateVhdl(TopLevel())    
// }
