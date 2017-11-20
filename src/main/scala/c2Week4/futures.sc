import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Random, Try}

// Futures...

val f = Future { sys.error("failed") }
val g = Future { "*** " + Thread.currentThread().getName }
val h = f fallbackTo g
h foreach println

// recursive retry
def retry[T](times: Int)(block: => Future[T]): Future[T] = {
  if(times == 0)
    Future.failed(new Exception("Sorry"))
  else
    block fallbackTo {
      retry(times-1){block}
    }
}

def retryWithFoldLeft[T](times: Int)(block: => Future[T]): Future[T] = {
  val ns = (1 to times).toList
  val attempts: List[() => Future[T]] = ns.map(_ => ()=>block)
  val failed: Future[T] = Future.failed(new Error("Sorry"))
  val result = attempts.foldLeft(failed)((a, block) => a recoverWith { case _ => block() })
  result
}



def retryWithFoldRight[T](noTimes: Int)(block: => Future[T]): Future[T] = {
  val ns = (1 to noTimes).toList
  val attempts = ns.map(_ => ()=>block)
  val failed: Future[T] = Future.failed(new Error("Sorry"))
  val result = attempts.foldRight(() =>failed)((block, a) => () => { block() fallbackTo { a() } } )
  result ()
}
lazy val testFuture1 = Future[Int] {
  println("thread start: " + Thread.currentThread().getName)

  Try {
    Thread.sleep(500)
  }
  //    if (Random.nextBoolean()) {
  //      println("thread failed: " + Thread.currentThread().getName)
  //      throw new Error("failed!")
  //    }
  println("thread end: " + Thread.currentThread().getName)
  5
}
val testFuture2 = Future[Int] {
  println("thread start: " + Thread.currentThread().getName)

  Try {
    Thread.sleep(500)
  }
  //    if (Random.nextBoolean()) {
  //      println("thread failed: " + Thread.currentThread().getName)
  //      throw new Error("failed!")
  //    }
  println("thread end: " + Thread.currentThread().getName)
  5
}

val r = retry[Int](3) {
  testFuture2
}

r foreach println

while (!r.isCompleted) {
  println("notCompl")
}