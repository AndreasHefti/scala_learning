import scala.util.{Failure, Random, Success, Try}

def retry[T](n: Int)(expr: => T): T = {
  Try{ expr } match {
    case Success(x) => x
    case _ if n > 1 => retry(n - 1)(expr)
    case Failure(e) => throw e
  }
}

var n = 0
retry(3) {
  n += 1
  if(Random.nextBoolean()) {
    println("failure " + n)
    throw new Error("failure " + n)
  } else {
    "success " + n
  }
}