import java.util.concurrent.{ForkJoinPool, ForkJoinTask}
import org.scalameter._
// simple thread example

class Hello extends Thread {
  override def run(): Unit = {
    println("Hello from Thread: " + getName)
  }
}

val hello1 = new Hello
hello1.start()
hello1.join()

// atomicity

object CounterNotSynchronized {
  private var count: Long = 0
  def incrementAndGet(): Long = {
    count = count + 1
    return count
  }
}

object CounterSynchronized {
  private val lock = new AnyRef
  private var count: Long = 0
  def incrementAndGet(): Long = lock.synchronized {
    count = count + 1
    return count
  }
}

def startCountThread() = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for (i <- 0 until 10) yield CounterNotSynchronized.incrementAndGet()
      println(uids)
    }
  }
  t.start()
}

startCountThread()
startCountThread()

// deadlock
class Account(private var amount: Int = 0) {
  def transfer(target: Account, n: Int): Unit =
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
}

def startAccountThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run(): Unit =
      for (i <- 0 until n)
        a.transfer(b, 1)
  }
  t.start()
  t
}

val a1 = new Account(500000)
val a2 = new Account(700000)

// this will sometimes stuck in a dead lock
// because wen a1 holds a monitor for itself while at the same time
// a2 holds a monitor for itself while both want to get a monitor from
// the other
/*
val t1 = startAccountThread(a1, a2, 150000)
val t2 = startAccountThread(a2, a1, 150000)
t1.join()
t2.join()
println("finished")
*/

// we can avoid such kind of dead locks by control the order on which the
// Accounts gets the monitors. For example the account with the lower uuid
// first

class AccountU(private var amount: Int = 0) {
  val uid = CounterSynchronized.incrementAndGet()

  private def lockAndTransfer(target: AccountU, n: Int): Unit = {
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
  }
  def transfer(target: AccountU, n: Int): Unit =
    if (this.uid < target.uid) this.lockAndTransfer(target, n)
    else target.lockAndTransfer(this, n)
}

def startAccountUThread(a: AccountU, b: AccountU, n: Int) = {
  val t = new Thread {
    override def run(): Unit =
      for (i <- 0 until n)
        a.transfer(b, 1)
  }
  t.start()
  t
}


val au1 = new AccountU(500000)
val au2 = new AccountU(700000)
val t1 = startAccountUThread(au1, au2, 150000)
val t2 = startAccountUThread(au2, au1, 150000)
t1.join()
t2.join()
println("finished")

// sum segments (VectorNorm) in parallel
def sumSegment(vector: Array[Int], p: Double, s: Int, t: Int): Double = {
  var sum: Double = 0
  for (i <- s until t)
    sum += Math.pow(Math.abs(vector(i)), p)
  sum
}

def pNorm(vector: Array[Int], p: Double): Double =
  Math.pow(sumSegment(vector, p, 0, vector.length), 1/p)

// still sequential but in two parts
def pNormTwoParts(vector: Array[Int], p: Double): Double = {
  val m = vector.length / 2
  val (sum1, sum2) = (sumSegment(vector, p, 0, m), sumSegment(vector, p, m, vector.length))
  return Math.pow(sum1 + sum2, 1/p)
}

val a: Array[Int] = Array(3, 4, 6, 3, 2, 1, 3, 5, 6)
pNorm(a, 2)
pNormTwoParts(a, 2)
/*
def pNormTwoPartsParallel(vector: Array[Int], p: Double): Double = {
  val m = vector.length / 2
  val (sum1, sum2) = parallel(sumSegment(vector, p, 0, m), sumSegment(vector, p, m, vector.length))
  return Math.pow(sum1 + sum2, 1/p)
}

def task[A](taskA: => A): ForkJoinTask[A] = {
  ForkJoinPool.commonPool().
}


def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
  val tB: Task[B] = task { taskB }
  val tA: A = taskA
  (tA, tB.join())
}
*/

// benchmarking with scala meter
val time =  measure {
  (0 until 1000000).toArray
}