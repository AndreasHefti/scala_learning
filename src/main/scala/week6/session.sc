/* Other (immutable) Collections
 *
 * Sequences: List, Vector, Range and (Array and String (sequence of Char)
 * Array and String are form Java and not directly a subtype
 * of Sequences but adapted to that
 *
 * Iterable: Sequences, Set and Map
 */

var list = List(1,2,6,-30)
var vector = Vector(1,2,6,-30)
var array = Array(1,2,6,-30)
var s = "Hello World"

list map (x => x * 2)
vector map (x => x * 2)
array map (x => x * 2)
s filter (x => x.isUpper)

/* Ranges are ranges of integer values*/

val range1 = 1 to 5
val range2 = 1 until 5
range1.head
range1.last
range2.head
range2.last

var range3 = 1 to 10 by 3
range3.head
range3.last

var range4 = 0 to 10 by 3
range4.head
range4.last

// NOTE:
var range5 = 0 to 10 by -3

// sequence ops

vector exists (x => x == 6)
array exists (x => x == -6)

list forall (x => x > -30)
vector forall (x => x > -31)

var pairs = (0 until 5) zip s
var pairs1 = List(0,1,2,3,4) zip s

pairs.unzip
pairs1.unzip

s flatMap (c => List('.', c))

array.sum
array max

// exercise: To list all combinations of numbers x and y
// where x is in a range from 1 to M and y in a range from 1 to N
val m = 5
val n = 5
(1 to m) flatMap (x => (1 to n) map (y => (x, y)))

// exercise: Compute the scalar of two vectors
def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  //(xs zip ys).map(xy => xy._1 * xy._2).sum
  // with pattern matching applied
  // (xs zip ys).map( xy => xy match { case (x, y) => x * y}).sum
  // and shorter
  (xs zip ys).map{ case (x, y) => x * y}.sum

// exercise: A number is prime if the only divisors of n are 1 and itself
// mathematical solution (not efficient)

def isPrime(n: Int): Boolean =
  (2 until n) forall (x => n % x != 0)

isPrime(1)
isPrime(2)
isPrime(3)
isPrime(4)

/* replace for loops with higher order functions of sequences
 *
 * Vector[(x,y)] primePairs
 * for (x = 1; x < n; x++) {
 *   for (y = 1; y < x; y++) {
 *     if ( isPrime(x + y) {
 *       primePairs.add((x,y))
 *     }
 *   }
 * }
 */

val n1 = 7
val combs = (1 until n1) map (x => (1 until x) map (y => (x,y)))

(combs foldRight Seq[(Int, Int)]())(_ ++ _)
// is the same as:
combs.flatten
// and the same as:
val combs2 = (1 until n1) flatMap (x => (1 until x) map (y => (x,y)))

// useful law:
// xs flatMap f = (xs map f).flatten

// sometimes in functional programming it is hard to get combined
// higher order function expressions like the following:

(1 until n1) flatMap (x =>
  (1 until x) map (y => (x,y))) filter (pair =>
    isPrime(pair._1 + pair._2))

// Scala for this cases has for-expressions


case class Person(name: String, age: Int)

var persons: List[Person] = new Person("A", 19) :: new Person("B", 39) :: Nil

for ( p <- persons if p.age > 20 ) yield p.name

// is the same as:

persons filter (p => p.age > 20) map (p => p.name)

// adapted to our primePair problem:

for {
  x <- 1 until n1
  y <- 1 until x
  if isPrime(x + y)
} yield (x, y)


def scalarProductWithForExpr1(xs: Vector[Double], ys: Vector[Double]): Double =
  ( for {
    x <- xs
    y <- ys
  } yield x * y ).sum

def scalarProductWithForExpr2(xs: Vector[Double], ys: Vector[Double]): Double =
  ( for ((x, y) <- xs zip ys ) yield x * y ).sum

/* Sets: Sets are unordered and have no duplicates, the contains operation
 * is fundemental and fast
 *
 */

val fruit = Set("apple", "banana", "pear", "pineapple")
val one2Six = (1 to 6).toSet

one2Six map (_ + 2)
fruit filter (_.startsWith("app"))
one2Six.nonEmpty

/* exercise: how many queens can be placed on a chess board of size n
 * so that they do not interfere
 */

def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSave(col, queens)
      } yield col :: queens

  def isSave(col: Int, queens: List[Int]):Boolean = {
    val row = queens.length
    val coords = (row - 1 to 0 by -1) zip queens
    coords forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  placeQueens(n)
}

def showQueens(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("*").updated(col, "X").mkString
  "\n" + (lines mkString "\n")
}

(queens(10) take 3 map showQueens) mkString "\n"

for ( queenSet <- queens(4) )
  yield println(showQueens(queenSet))

/* Maps
 *
 */

val capitals = Map("US" -> "Washington", "Switzerland" -> "Bern")

capitals("US")
// capitals("USSR") throws a NoSuchElementException
// NOTE the return type here is a Option
capitals get "US"
capitals get "USSR"

/* Option can be of type Some with the expected value assigned or None
 * if there was no date
 * Options can also be used within pattern matching
 */

def getCapital(country: String): String = capitals get country match {
  case None => "no Country " + country + " found"
  case Some(c) => c
}

getCapital("US")
getCapital("USSR")

/* group by and order by of maps */
val fruits = List("apple", "pear", "banana", "pineapple")
fruits sortWith (_.length < _.length)
fruits groupBy (_.head)


/* exercise polynomials as Maps */

// NOTE: a pair can also been created with:
"hallo" -> "world"
("hallo", "world")

class Poly(val terms0: Map[Int, Double]) {
  val terms = terms0 withDefaultValue 0.0
//  def + (other: Poly) = {
//    new Poly(terms ++ (other.terms map plus))
//  }
//  def plus(term: (Int, Double)): (Int, Double) = {
//    val (exp, coeff) = term
//    exp -> (coeff + terms(exp))
//  }
  def + (other: Poly) =
    new Poly((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] =
    terms + (term._1 -> (term._2 + terms(term._1) ))

  override def toString() = {
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }
}

object Poly {
  def apply(terms: (Int, Double)*): Poly = {
    new Poly(terms.toMap)
  }
}

val p1 = Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2

