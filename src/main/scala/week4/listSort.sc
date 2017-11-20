/* List operations with pattern matching */

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  // the empty list case
  case List() => List(x)
  // the same as Cons(y, ys)
  // y is head and ys tail
  case y :: ys =>
    if (x <= y) x :: xs
    else y :: insert(x, ys)
}

var l = insert(2, List())
l = insert(1, l)
l = insert(8, l)
l = insert(5, l)

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}