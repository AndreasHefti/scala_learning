package c2Week3

abstract class Gates extends Simulation {

  def InvDelay: Int
  def AndDelay: Int
  def OrDelay: Int

  class Wire {

    private var signal = false
    private var actions: List[Action] = List()

    def getSignal = signal
    def setSignal(s: Boolean) = {
      if (s != signal) {
        signal = s
        actions foreach (_())
      }
    }

    def addAction(a: Action) = {
      actions = a :: actions
      a()
    }
  }

  def inverter(in: Wire, out: Wire) = {
    def invAction(): Unit = {
      val inSig = in.getSignal
      afterDelay(InvDelay) {
        out setSignal !inSig
      }
    }
    in addAction invAction
  }

  private def gate(in1: Wire, in2: Wire, out: Wire, delay: Int)(op: (Boolean, Boolean) => Boolean) = {
    def opAction() = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(delay) {
        out setSignal op(in1Sig, in2Sig)
      }
    }
    in1 addAction opAction
    in2 addAction opAction
  }

  def andGate(in1: Wire, in2: Wire, out: Wire) = {
    gate(in1, in2, out, AndDelay)(_ & _)
//    def andAction() = {
//      val in1Sig = in1.getSignal
//      val in2Sig = in2.getSignal
//      afterDelay(AndDelay) {
//        out setSignal (in1Sig & in2Sig)
//      }
//    }
//    in1 addAction andAction
//    in2 addAction andAction
  }

  def orGate(in1: Wire, in2: Wire, out: Wire) = {
    gate(in1, in2, out, OrDelay)(_ | _)
//    def andAction() = {
//      val in1Sig = in1.getSignal
//      val in2Sig = in2.getSignal
//      afterDelay(AndDelay) {
//        out setSignal (in1Sig | in2Sig)
//      }
//    }
//    in1 addAction andAction
//    in2 addAction andAction
  }

  def probe(name: String, wire: Wire) = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }

    wire addAction probeAction
  }

}
