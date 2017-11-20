package week4

/**
  * Created by andreashefti on 24.10.17.
  */
abstract class Bool {

  def ifElse[T](t: => T, e: => T): T

  def && (x: => Bool): Bool = ifElse(x, FALSE)
  def || (x: => Bool): Bool = ifElse(TRUE, x)
  def unary_! : Bool = ifElse(FALSE, TRUE)

  def == (x: => Bool): Bool = ifElse(x, x.unary_!)
  def != (x: => Bool): Bool = ifElse(x.unary_!, x)

  def < (x: => Bool):Bool = ifElse(FALSE, x)

}

object FALSE extends Bool {
  def ifElse[T](t: => T, e: => T): T = e
}

object TRUE extends Bool {
  def ifElse[T](t: => T, e: => T): T = t
}
