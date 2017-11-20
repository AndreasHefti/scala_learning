/* Streams */

def isPrime(n: Int): Boolean =
  (2 until n) forall (x => n % x != 0)

// works but is not efficient because first
// the whole range is filtered and then the second element is taken
((1000 to 10000) filter isPrime)(1)

// with streams we can do a more lazy evaluation
(1000 to 10000).toStream

((1000 to 10000).toStream filter isPrime take(2)).tail.head

def streamRange(from: Int, to: Int): Stream[Int] =
  if (from >= to) Stream.empty
  else Stream.cons(from, streamRange(from + 1, to))

streamRange(0, 100)


((1000 to 10000).toStream filter isPrime)(1)

// if Xs is a Stream, the cons operator always creates a list
var x = 2
var intStream: Stream[Int] = streamRange(0, 100)
var intList: List[Int] = x :: intStream.toList
var intStreamWithx = x #:: intStream

def streamRangeWithOutput(from: Int, to: Int): Stream[Int] = {
  print(from + " ")
  if (from >= to) Stream.empty
  else Stream.cons(from, streamRangeWithOutput(from + 1, to))
}

streamRangeWithOutput(1, 10).take(3).toList

// Infinite Streams
def from(n: Int): Stream[Int] = n #:: from(n + 1)
val nats = from(0)
val m4s = nats map (_ * 4)

(m4s take 100).toList

/* Ancient method to get prime numbers ist iteratively get prime
 * numbers from 2 on and eliminate all multiples of this prime number
 */
def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

val primes = sieve(from(2))

primes.take(100).toList

// We can now also define the approximation used for the square-root as a stream
def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

sqrtStream(15).take(10).toList

// Now we can reuse the isGoodEnough function from before as a termination critaria
def isGoodEnough(guess: Double, x:Double): Boolean =
  math.abs((guess * guess - x) / x ) < 0.0001

sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList
//sqrtStream(15).takeWhile(isGoodEnough(15).).toList