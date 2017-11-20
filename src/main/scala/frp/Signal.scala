package frp

import scala.util.DynamicVariable

class Signal[T](expr: => T, name: String) {
  import Signal._

  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)

  protected def update(expr: => T): Unit = {
    println("***** update: " + name)
    myExpr = () => expr
    computeValue()
  }

  protected def computeValue(): Unit = {
    println("***** computeValue: " + name)
    val newValue = caller.withValue(this)(myExpr())

    if (myValue != newValue) {
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }

  }

  def apply(): T = {
    println("***** apply: " + name + " caller: " + caller.value)
    observers += caller.value
    println("***** apply: obs " + observers)
    assert(!caller.value.observers.contains(this), "cyclic signal definition ")
    myValue
  }

  override def toString: String = name + " " + myValue
}

object NoSignal extends Signal[Nothing](???, "NoSignal") {
  override def computeValue(): Unit = ()
}

object Signal {
  // NOTE: as global variable just as a simplification ans has some known problems
  //       when it comes to concurrency, if two or more threads are intend to change
  //       the caller stack on the same time.
  //       We can synchronize the caller stack but this has its own problems and complexity
  //private val caller = new StackableVariable[Signal[_]](NoSignal)

  // Instead we can use a DynamicVariable that is thread scoped and has the same interface
  // like the created StackableVariable, so we can just replace it here
  // NOTE: That scala offers Thread-local variables support within DynamicVariable easily
  // NOTE: but also the Thread-local solution has his disadvantages:
  //       the caller is still a imperative state and is managed by the JVM in a global hash map
  //       so the performance may not that be that good and if threads becomes workers
  //       there may be also long looking times
  private val caller = new DynamicVariable[Signal[_]](NoSignal)

  // A complete functional solution would be to propagate the caller for each call thought the Signal interface
  // what would result in much more code and effort



  def apply[T](expr: => T) = new Signal(expr, "s")
}
