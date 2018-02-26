package ppWeek2

object Scan {

  // scan left O(n)
  def scanLeft[A](in: Array[A], a0: A, f: (A,A) => A, out: Array[A]): Unit = {
    var i = 0
    out(0) = a0
    while ( i < in.length) {
      out(i + 1) = f(out(i), in(i))
      i = i + 1
    }
  }



  // how can we paralleling scanLeft?
  // by using "specialized" parallel implementations of mapSeg and reduceSeg

  def reduceSeg[A](in: Array[A], left: Int, right: Int, a0: A, f: (A,A) => A): A = {
    var a = a0
    var i = left
    while  (i < right) {
      a = f(a, in(i))
      i = i + 1
    }
    a
  }

  def mapSeg[A, B](in: Array[A], left: Int, right: Int, fi: (Int,A) => B, out: Array[B]): Unit = {
    ???
  }

  def scanLeftP[A](in: Array[A], a0: A, f: (A,A) => A, out: Array[A]): Unit = {
    val fi = { (i: Int, v: A) => reduceSeg(in, 0, i, a0, f) }
    mapSeg(in, 0, in.length, fi, out)
    val last = in.length - 1
    out(last + 1) = f(out(last), in(last))
  }

  def main(args: Array[String]): Unit = {
    val a1 = Array[Int](1,2,3)
    val a2 = Array[Int](0, 0, 0, 0)
    scanLeft[Int](a1, 100, _ + _, a2)
    println(a2.deep.mkString(","))

  }


}
