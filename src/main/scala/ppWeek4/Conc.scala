package ppWeek4

import org.scalameter.{Key, Warmer, config}
import ppWeek4.Conc.concat

import scala.annotation.tailrec
import scala.collection.parallel.Combiner
import scala.reflect.ClassTag

sealed trait Conc[+T] {
  def level: Int
  def size: Int
  def left: Conc[T]
  def right: Conc[T]

  def <>[N >: T](that: Conc[N]): Conc[N] = {
    if (this == Empty) that
    else if (that == Empty) this
    else concat(this, that)
  }
}

object Conc {


  /** This is not a simple concat but also
    * re-balance the tree by concatenate one tree to the other
    *
    * This is done by defining different cases:
    * 1. the height difference of the trees is 1 or less
    *   --> we can simply link the trees
    * 2. the left tree is left-leaning; higher then the right tree
    *   -->
    * @param xs
    * @param ys
    * @tparam T
    * @return
    */
  private def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    val diff = ys.level - xs.level
    // handling case 1
    if (diff >= -1 && diff <= 1)  new <>(xs, ys)
    else if(diff <= -1){
      lrConcat(xs, ys)
    } else {
      lrConcat(ys, xs)
    }
  }

  private def lrConcat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    // handling case 2.1
    if (xs.left.level >= xs.right.level){
      val nr = concat(xs.right, ys)
      new <>(xs.left, nr)
    } else { // handling case 2.2
      val nrr = concat(xs.right.right, ys)
      if (nrr.level == xs.level - 3) {
        val nl = xs.left
        val nr = new <>(xs.right.left, nrr)
        new <>(nl, nr)
      } else{
        val nl = new <>(xs.left, xs.right.left)
        val nr = nrr
        new <>(nl, nr)
      }
    }
  }

  def appendLeaf[T](xs: Conc[T], ys: Conc[T]): Conc[T] = xs match {
    case Empty => ys
    case xs: Single[T] => new <>(xs, ys)
    case xs: Chunk[T] => new <>(xs, ys)
    case _ <> _ => new Append[T](xs, ys)
    case xs: Append[T] => append(xs, ys)
  }

  @tailrec
  private def append[T](xs: Append[T], ys: Conc[T]): Conc[T] = {
    if (xs.right.level > ys.level) new Append[T](xs, ys)
    else {
      val zs = new <>(xs.right, ys)
      xs.left match {
        case ws: Append[T] => append(ws, zs)
        case ws if ws.level <= zs.level => ws <> zs
        case ws => new Append(ws, zs)
      }
    }
  }
}

case object Empty extends Conc[Nothing] {
  def level = 0
  def size = 0
  val left: Conc[Nothing] = null
  val right: Conc[Nothing] = null
}

class Single[T](val x: T) extends Conc[T] {
  def level = 0
  def size = 1
  val left: Conc[T] = Empty
  val right: Conc[T] = Empty
}

case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  val level = 1 + math.max(left.level, right.level)
  val size = left.size + right.size
}

/** This type is used as a marker used by add operation of a Combiner/Builder */
case class Append[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  val level = 1 + math.max(left.level, right.level)
  val size = left.size + right.size
}

/** Chink Nodes are similar to Single nodes, but instead of a Single
  * element they hold a array of elements
  * @param array
  * @param size
  * @tparam T
  */
class Chunk[T](val array: Array[T], val size: Int) extends Conc[T] {
  def level = 0
  val left: Conc[T] = Empty
  val right: Conc[T] = Empty
}

class ConcBuffer[T: ClassTag](val k: Int, private var conc: Conc[T]) {
  private var chunk: Array[T] = new Array(k)
  private var chunkSize: Int = 0

  final def +=(elem: T): ConcBuffer[T] = {
    if (chunkSize >= k)
      expand()
    chunk(chunkSize) = elem
    chunkSize += 1
    this
  }

  private def expand(): Unit = {
    conc = Conc.appendLeaf[T](conc, new Chunk[T](chunk, chunkSize))
    chunk = new Array(k)
    chunkSize = 0
  }

  final def combine(that: ConcBuffer[T]): ConcBuffer[T] = {
    val combinedConc = this.result <> that.result
    new ConcBuffer(k, combinedConc)
  }

  def result: Conc[T] = {
    conc = Conc.appendLeaf(conc, new Chunk(chunk, chunkSize))
    conc
  }

}

object ConcBuffer {

  val stdConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val size = 1000000

    def run(p: Int): Unit = {
      val taskSupport = new collection.parallel.ForkJoinTaskSupport(
        new scala.concurrent.forkjoin.ForkJoinPool(p)
      )
      val strings = (0 until size).map(_.toString)
      val time = stdConfig measure {
        val paralelized = strings.par
        paralelized.tasksupport = taskSupport
        paralelized.aggregate(new ConcBuffer[String](4, Empty))(_ += _, _ combine _).result
      }
      println(s"p = $p, time = $time ms")
    }

    run(1)
    run(2)
    run(4)
    run(8)
  }
}
