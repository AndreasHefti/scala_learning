package ppWeek2

import ppWeek2.ScanTreeSeq._

object ScanTreePar {

  sealed abstract class Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  sealed abstract class TreeRes[A] { val res: A }
  case class LeafRes[A](override val res: A) extends TreeRes[A]
  case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]


  def reduce[A](t: Tree[A], f: (A,A) => A): TreeRes[A] = t match {
    case Leaf(v) => LeafRes(v)
    case Node(l, r) => {
      val (tl, tr) = ParallelUtils.parallel(reduce(l, f), reduce(r, f))
      NodeRes(tl, f(tl.res, tr.res), tr)
    }
  }

  def map[A](t: TreeRes[A], a0: A, f: (A,A) => A): Tree[A] = t match {
    case LeafRes(v) => Leaf(f(a0, v))
    case NodeRes(l, _, r) => {
      val (tl, tr) = ParallelUtils.parallel(map[A](l, a0, f), map[A](r, f(a0, l.res), f))
      Node(tl, tr)
    }
  }

  // not balanced
  def prepend[A](x: A, t: Tree[A]):  Tree[A] = t match {
    case Leaf(v) => Node(Leaf(x), Leaf(v))
    case Node(l,r) => Node(prepend(x, l), r)
  }

  def scanLeft[A](t: Tree[A], a0: A, f: (A,A) => A): Tree[A] =
    prepend(a0, map(reduce(t, f), a0, f))


  def main(args: Array[String]): Unit = {
    val t1 = Node(Node(Leaf(1), Leaf(3)), Node(Leaf(8), Leaf(50)))
    val plus = (x:Int, y: Int) => x + y

    val tRes = reduce(t1, plus)
    println(tRes)
    val t2 = map(tRes, 100, plus)
    println(t2)

    val t3 = scanLeft(t1, 100, plus)
    println(t3)
  }

}
