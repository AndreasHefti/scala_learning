package ppWeek2

object ParallelMapSession {

  def mapASegSeq[A, B](in: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
    var i = left
    while(i < right) {
      out(i) = f(in(i))
      i = i + 1
    }
  }

  def mapASegSeqP1[A, B](in: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
    if (right - left < 5 )
      mapASegSeq(in, left, right, f, out)
    else {
      val mid = left + (right - left) / 2
      ParallelUtils.parallel(
        mapASegSeq(in, left, mid, f, out),
        mapASegSeq(in, mid, right, f, out)
      )
    }
  }

  def mapASegSeqP2[A, B](in: Array[A], left: Int, right: Int, numTasks: Int, f: A => B, out: Array[B]): Unit = {
    val width = right - left
    val ranges = 0 to width  by (width / numTasks)
    ranges
      .zip(ranges.tail)
      .map {
        case (s, e) => ParallelUtils.task { mapASegSeq(in, s, e, f, out) }
      }
      .foreach( _.join() )
  }

  def testMapASeq(): Unit = {
    val in = Array[Int](1,2,3,4,5,6,7,8,9)
    val out = Array[Int](0,0,0,0,0,0,0,0,0)
    val f = (x: Int) => x * x

    mapASegSeq(in, 0, in.length, f, out)

    println(out.deep.mkString(","))
  }

}
