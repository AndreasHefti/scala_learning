package ppWeek2

object ScanArrayWithTreeRes {

  sealed abstract class TreeRes[A] { val res: A }
  case class LeafRes[A](from: Int, to:Int, override val res: A) extends TreeRes[A]
  case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]

  def reduceSeg[A](in: Array[A], left: Int, right: Int, a0: A, f: (A,A) => A): A = {
    var a = a0
    var i = left
    while  (i < right) {
      a = f(a, in(i))
      i = i + 1
    }
    a
  }

  val threshold = 10
  def upsweep[A](in: Array[A], from: Int, to: Int, f: (A,A) => A): TreeRes[A] = {
    if (to - from < threshold)
      LeafRes(from, to, reduceSeg(in, from + 1, to, in(from), f))
    else {
      val mid = from + (to - from) / 2
      val (tl, tr) = ParallelUtils.parallel(
        upsweep(in, from, mid, f),
        upsweep(in, mid, to, f)
      )
      NodeRes(tl, f(tl.res, tr.res), tr)
    }
  }

  def scanLeftSeg[A](in: Array[A], from: Int, to: Int, a0: A, f: (A, A) => A, out: Array[A]): Unit = {
    if (from < to) {
      var i = from
      var a = a0
      while (i < to) {
        a = f(a, in(i))
        i = i + 1
        out(i) = a
      }
    }
  }

  def downsweep[A](in: Array[A], a0: A, f: (A,A) => A, t: TreeRes[A], out: Array[A]): Unit = t match {
    case LeafRes(from, to, res) => scanLeftSeg(in, from, to, a0, f, out)
    case NodeRes(l, _, r) => {
      val (_,_) = ParallelUtils.parallel(
        downsweep(in, a0, f, l, out),
        downsweep(in, f(a0, l.res), f, r, out)
      )
    }
  }

  def scanLeft[A](in: Array[A], a0: A, f: (A,A) => A, out: Array[A]): Unit = {
    val t = upsweep(in, 0, in.length, f)
    downsweep(in, a0, f, t, out)
    out(0) = a0
  }



}
