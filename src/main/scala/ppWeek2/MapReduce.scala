package ppWeek2

import ppWeek2.ParallelMapSession.mapASegSeq

object MapReduce {

  sealed abstract class Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def reduce[A](t: Tree[A], f: (A,A) => A): A = t match {
    case Leaf(v) => v
    case Node(l, r) => f(reduce(l, f), reduce(r, f))
  }

  def toList[A](t: Tree[A]): List[A] = t match {
    case Leaf(v) => List(v)
    case Node(l, r) => toList(l) ++ toList(r)
  }

  def map[A,B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Node(l, r) => Node(map(l,f), map(r, f))
  }

  def mapReduce[A,B](t: Tree[A], f: A => B): List[B] = t match {
    case Leaf(v) => List(f(v))
    case Node(l, r) => toList(map(l,f)) ++ toList(map(r, f))
  }

  // toList(t) == reduce(map(t, List(_)), _ ++ _)


  // if f : (A,A) => is associative, t1:Tree and t2:Tree and
  // if toList(t1) == toList(t2), then:
  // reduce(t1, f) == reduce(t2, f)
  val threshold = 10
  def reduceSeg[A](in: Array[A], left: Int, right: Int, f: (A,A) => A): A = {
    if (right - left < threshold) {
      var res = in(left)
      var i = left + 1
      while (i < right) {
        res = f(res, in(i))
        i = i + 1
      }
      res
    } else {
      val mid = left + (right - left) / 2
      val (a1, a2) = ParallelUtils.parallel(
        reduceSeg(in, left, mid, f),
        reduceSeg(in, mid, right, f)
      )
      f(a1, a2)
    }
  }

  def mapSeg[A, B](in: Array[A], left: Int, right: Int, numTasks: Int, f: A => B, out: Array[B]): Unit = {
    val width = right - left
    val ranges = 0 to width  by (width / numTasks)
    ranges
      .zip(ranges.tail)
      .map {
        case (s, e) => ParallelUtils.task { mapASegSeq(in, s, e, f, out) }
      }
      .foreach( _.join() )
  }

}
