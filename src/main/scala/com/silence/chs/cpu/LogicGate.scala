package com.silence.chs.cpu

/**
 * 逻辑门 ：与 或 非
 * 与 : 2 in , 1 out
 * 或 : 2 in , 1 out
 * 非 : 1 in , 1 out
 * 
 * 数学表达式
 * <table>
 *  <tr>
 *    <th>name</th>
 *    <th>Param</th>
 *    <th>Formula</th>
 *  </tr>
 *  <tr>
 *    <th>与</th>
 *    <th>A与B</th>
 *    <th>AB</th>
 *  </tr>
 *  <tr>
 *    <th>或</th>
 *    <th>A或B</th>
 *    <th>A + B</th>
 *  </tr>
 *  <tr>
 *    <th>非</th>
 *    <th>非A</th>
 *    <th>A'</th>
 *  </tr>
 * </table>
 */
abstract class LogicGate extends Simulation{

  def InverterDelay: Int
  def AndGeteDelay: Int
  def OrGateDelay: Int

  class Wire {
    private var signal = false
    private var actions : List[Action] = List.empty

    def getSignal = signal

    def setSignal(s : Boolean) = {
      if(s != signal) {
        signal = s
        actions.foreach(_())
      }
    }

    def addAction(a : Action) = {
      actions = a :: actions
      a()
    }
  }

  // 与门 
  def andGate(a : Wire, b : Wire, output : Wire) : Unit = {
    def andAction() = {
      val aSig = a.getSignal
      val bSig = b.getSignal
      afterDelay(AndGeteDelay) {
        output setSignal (aSig & bSig)
      }
    }
    a addAction andAction
    b addAction andAction
  }

  // 反转器 ： 信号取反
  def inverter(input : Wire, output : Wire) : Unit = {
    def invertAction() = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !inputSig
      }
    }
    output addAction invertAction
  }
  // 或门
  def orGate(a : Wire, b :Wire, output: Wire) : Unit = {
    def orAction() = {
      val aSig = a.getSignal
      val bSig = b.getSignal
      afterDelay(OrGateDelay) {
        output setSignal (aSig | bSig)
      }
    }
    output addAction orAction
  }
  // 亦或门 f = A (+) B = AB' + A'B
  final def xorGate(a : Wire, b :Wire, output : Wire) : Unit = {
    val ra, rb = new Wire
    inverter(a,ra)
    inverter(b, rb)
    val aOrrb, bOrra = new Wire
    andGate(a, rb, aOrrb)
    andGate(b, ra, bOrra)
    orGate(aOrrb, bOrra, output)
  }
  // 半加器
  final def halfAdder(a : Wire, b : Wire, s : Wire, c : Wire): Unit = {
    /**
     * a------+------
     */
    val d, e = new Wire
  }
}

