package ppWeek3

import org.scalameter.{Key, Warmer, config}

object ParallelLoop {


  def initArrayPar(xs: Array[Int])(v: Int): Unit = {
    for (i <- (0 until xs.length).par ) {
      xs(i) = v
    }
  }

  def initArraySeq(xs: Array[Int])(v: Int): Unit = {
    for (i <- (0 until xs.length) ) {
      xs(i) = v
    }
  }

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val array: Array[Int] = new Array[Int](length)
    val seqtime = standardConfig measure {
      initArraySeq(array)(5)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      initArrayPar(array)(5)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}
