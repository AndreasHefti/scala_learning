package c2Week1

import scala.util.control.NonFatal

abstract class Try[+T] {
  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Success(x) => try f(x) catch { case NonFatal(ex) => Failure(ex) }
    case fail: Failure => fail
  }

  def map[U](f: T => U): Try[U] = this match {
    case Success(x) => Try(f(x))
    case fail: Failure => fail
  }
}

case class Success[T](x: T) extends Try[T]
case class Failure(ex: Throwable) extends Try[Nothing]

object Try {
  // Note the call-by-name declaration here for the expr attribute.
  //      Otherwise the expression would have been evaluated before the call this is not what we want
  def apply[T](expr: => T): Try[T] =
    try Success(expr)
    catch {
      case NonFatal(e) => Failure(e)
    }
}

