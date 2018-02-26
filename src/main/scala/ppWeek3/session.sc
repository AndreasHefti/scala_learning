import scala.collection.parallel.{Combiner, Splitter}
import scala.collection.{GenSet, mutable}

def initArray(xs: Array[Int])(v: Int): Unit = {
  for (i <- (0 until xs.length).par ) {
    xs(i) = v
  }
}


// parallel sum
def sum(xs: Array[Int]): Int =  {
  xs.par.foldLeft(0)(_ + _)
}

// does this run in parallel?
// NO!
// -> because fold-Left-Right operation is sequential in nature
// also reduce-Left-Right- and scan-Left-Right-operations are

// but scala offers the fold operation that has a sightly different signature
// def fold[A](z: A)(f: (A, A) => A): A

def sumPar(xs: Array[Int]): Int =  {
  xs.par.fold(0)(_ + _)
}

def maxPar(xs: Array[Int]): Int =  {
  xs.par.fold(Int.MinValue)(Math.max)
}

// children's play
def play(a: String, b: String): String = List(a, b).sorted match {
  case List("paper", "scissors") => "scissors"
  case List("paper", "rock") => "paper"
  case List("rock", "scissors") => "rock"
  case List(a, b) if a == b => a
  case List("", b) => b
}

Array("paper", "rock", "paper", "scissors")
  .par.fold("")(play)

// this is like since par.fold uses a tree reduction
play(play("paper", "rock"), play("paper", "scissors"))

// but the par.fold is allowed to structure the reduction-tree differently like:
play("paper", play("rock", play("paper", "scissors")))

// in this case we have another result because the play function is not associative

// another problem with fold: count number of vowels

def isVowel(c: Char): Boolean = {
  "AEIOUaeiou".contains(c)
}

// because of the signature of fold[A](A)((A,A) => A) this is not compiling
// "Hello World".fold(0)((count, c) => if (isVowel(c)) count + 1 else count)

// we use aggregate instead that uses internally a foldLeft operation for a task and
// a fold operation to combine (aggregate) the task results
("Hello World").par.aggregate(0)((count, c) => if (isVowel(c)) count + 1 else count, _ + _)

// parallel collections
def intersectionWrong(a: GenSet[Int], b: GenSet[Int]): Set[Int] = {
  val result = mutable.Set[Int]()
  for (x <- a) if (b contains x) result += x
  result.toSet
}
// sequential call
intersectionWrong((0 until 1000).toSet, (0 until 1000 by 4).toSet)
// parallel call
intersectionWrong((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

// what is the problem with the parallel call?
// the result set is mutable and is accessed in parallel from all
// iteration threads. this can lead to errors or wrong result

// RULE: avoid mutations on the same memory location without proper synchronization

// instead we can use filter
def intersection(a: GenSet[Int], b: GenSet[Int]): GenSet[Int] = {
  if (a.size < b.size) a.filter(b(_))
  else b.filter(a(_))
}

// RULE: never modify a parallel collection on which a data-parallel
//       operation is in progress
//       --> never write to a collection that is concurrently traversed
//       --> never read from a collection that is concurrently modified

def graphMapWrong(g: mutable.Map[Int, Int]): Unit = {
  for ((k, v) <- g) {
    g(k) = g(v)
  }
}

val graph = mutable.Map[Int, Int]() ++= (0 until 10).map(i => (i, i + 1))
graph(graph.size - 1) = 0

graphMapWrong(graph)
println(graph)
val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })

// use TrieMap for parallel processing on the map

// filter with Builder
trait Trav[T] {
  def foreach(f: T => Unit): Unit
  def newBuilder: mutable.Builder[T, Traversable[T]]

  def filter(p: T => Boolean): Traversable[T] = {
    val b = newBuilder
    foreach(v => if (p(v)) b += v)
    b.result()
  }
}

// Combiner are the parallel counterparts to Builder
trait Comb[A, Repr] extends mutable.Builder[A, Repr] {
  def combine(that: Combiner[A, Repr]): Combiner[A, Repr]
}

trait homeWork[T] extends Traversable[T] with Splitter[T] with Combiner[T, Traversable[T]] {
  val threshold = 10
  def filterPar(p: T => Boolean): Traversable[T] = {
    if (this.size < threshold)
      filter(p).toSeq
    else {
      val children = for (child <- split) yield task { child.filterPar(p) }
    }
  }
}
