package week4

import java.util.NoSuchElementException

/**
  * Created by andreashefti on 23.10.17.
  */
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def nth(index: Int): T

  def prepend[U >: T](elem: U): List[U] = new Cons[U](elem, this )
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  // Note: head and tail are already defined by using 'val' declaration in the class definition

  def nth(index: Int): T =
    if ( index == 0 ) head else tail.nth( index - 1 )

  override def toString() = head.toString + " " + tail.toString
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  def nth(index: Int): Nothing = throw new IndexOutOfBoundsException("Nil.indexOf")

  override def toString() = "nil"
}

object List {
  def apply[T](): List[T] = Nil
  def apply[T](x1: T): List[T] = new Cons[T](x1, Nil)
  def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, Nil))

  def prepend[T](list: List[T], t: T): List[T] = new Cons[T]( t, list )
}



