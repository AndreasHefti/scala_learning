package week3

import java.util.NoSuchElementException

/**
  * Created by andreashefti on 23.10.17.
  */
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def nth(index: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  // Note: head and tail are already defined by using 'val' declaration in the class definition

  def nth(index: Int): T =
    if ( index == 0 ) head else tail.nth( index - 1 )

  override def toString() = head.toString + " " + tail.toString
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  def nth(index: Int): Nothing = throw new IndexOutOfBoundsException("Nil.indexOf")

  override def toString() = "nillll"
}

object List {
  def apply[T](): List[T] = new Nil
  def apply[T](x1: T): List[T] = new Cons[T](x1, new Nil)
  def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, new Nil))
}



