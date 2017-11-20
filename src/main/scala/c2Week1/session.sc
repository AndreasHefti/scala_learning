import scala.tools.nsc.doc.doclet

/* With PartialFunction in Scala one can query a defined function with
 * an argument if the function is defined for this argument:
 */

val f: PartialFunction[String, String] = {case "ping" => "pong"}

f.isDefinedAt("ping")
f("ping")
f isDefinedAt "pong"

/* The isDefinedAt gives only a valid result for the root pattern match */

val g: PartialFunction[List[Int], String] = {
  case Nil => "null"
  case x :: rest =>
    rest match {
      case Nil => "notPossible"
    }
}

g.isDefinedAt(List(1,2,3))
// -> but g(List(1,2,3)) will give you PatternMatch error

/* For expression translations
*
*  for (x <- e1) yield e2
*
*  will be translated by the compiler to
*
*  e1.map(x => e2)
*
*  for (x <- e1 if f; s) yield e2
*  for (x <- e1.withFilter(x => f); s) yield e2
*
*  for (x <- e1; y <- e2; s) yield e3
*  e1.flatMap(x => for (y <- e2; s) yield e3)
*
*/
//var s: Int => Int = _ + 2
for (x <- (0 to 5)) yield x

/* For expressions as query language */

case class Book(title: String, authors: List[String])

val booksDB: List[Book] = List(
  Book(title = "Story A", authors = List("Albert, C", "Zigi, F")),
  Book(title = "Story B", authors = List("Samuel, V")),
  Book(title = "Story C", authors = List("Max, M")),
  Book(title = "Story D", authors = List("Simon, T", "Albert, C")),
  Book(title = "Story E", authors = List("Larry, A")),
  Book(title = "Story F", authors = List("Gilbert, L", "Simon, T", "Albert, C"))
)

for {
  b <- booksDB;
  a <- b.authors
  if a startsWith "S"
} yield b.title

for {
  b1 <- booksDB
  b2 <- booksDB
  if b1 != b2
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield  a1

for {
  b1 <- booksDB
  b2 <- booksDB
  if b1.title < b2.title
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield  a1

( for {
  b1 <- booksDB
  b2 <- booksDB
  if b1.title < b2.title
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield  a1).distinct

val books: Set[Book] = Set(
  Book(title = "Story A", authors = List("Albert, C", "Zigi, F")),
  Book(title = "Story B", authors = List("Samuel, V")),
  Book(title = "Story C", authors = List("Max, M")),
  Book(title = "Story D", authors = List("Simon, T", "Albert, C")),
  Book(title = "Story E", authors = List("Larry, A")),
  Book(title = "Story F", authors = List("Gilbert, L", "Simon, T", "Albert, C"))
)

for {
  b1 <- books
  b2 <- books
  if b1 != b2
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield  a1

/* Exercise translate for expressions */

for {
  b <-books; a <- b.authors if a startsWith "Max"
} yield b.title

books flatMap (b =>
  b.authors withFilter(a => a startsWith "Max") map (_ => b.title))

/* Generic random generator */



trait Generator[+T] {
  self => // an alias for "this"

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }

}

def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}

val intGen = new Generator[Int] {
  def generate = new java.util.Random().nextInt()
}

def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- intGen) yield lo + x % (hi - lo)

def oneOf[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx)




val boolGen = new Generator[Boolean] {
  def generate = intGen.generate > 0
}

val intPairGen = new Generator[(Int, Int)] {
  def generate = (intGen.generate, intGen.generate)
}

//... or

val boolGen1 = for (x <- intGen) yield x > 0

def pairGen[T, U](t: Generator[T], u: Generator[U]) = for {
  x <- t
  y <- u
} yield (x, y)

pairGen(intGen, intGen).generate

def intListGen: Generator[List[Int]] = for {
  isEmpty <- boolGen1
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists = single(Nil)
def nonEmptyLists = for {
  head <- intGen
  tail <- intListGen
} yield head :: tail



intListGen.generate







