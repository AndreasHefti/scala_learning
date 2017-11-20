
trait Generator[+T] {
  self => // an alias for "this"

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }

  def filter(p: T => Boolean): Generator[T] =
    if (p(generate))

}

def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}

val integers = new Generator[Int] {
  def generate = new java.util.Random().nextInt()
}

val booleans = integers.map(_ >= 0)


trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def leafs: Generator[Leaf] = for {
  x <- integers
} yield Leaf(x)

def inners: Generator[Inner] = for {
  l <- trees
  r <- trees
} yield Inner(l, r)

def trees: Generator[Tree] = for {
  isLeaf <- booleans
  tree <- if (isLeaf) leafs else inners
} yield tree

trees.generate


//def intTreeGen: Generator[Tree] = for {
//  isLeaf <- booleans
//  tree <- if (isLeaf) emptyTrees else nonEmptyTrees
//} yield tree
//
//def emptyTrees = single(Leaf(integers.generate))
//def nonEmptyTrees = for {
//  left <- intTreeGen
//  right <- intTreeGen
//} yield Inner(left, right)
//
//intTreeGen.generate
