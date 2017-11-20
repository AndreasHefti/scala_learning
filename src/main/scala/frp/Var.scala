package frp

class Var[T](expr: => T, name: String) extends Signal[T](expr, name) {
  override def update(expr: => T): Unit = super.update(expr)
}

object Var {
  def apply[T](expr: => T) = new Var(expr, "v")
}
