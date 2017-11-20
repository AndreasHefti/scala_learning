/* More List operations */

val l1 = List(1)
val l2 = List(2)
val l12 = l1 ++ l2
val l21 = l2 ::: l1
val l121 = 1 :: l21

val l123456 = 1::2::3::4::5::6::Nil

var head = l123456.head
var last = l123456.last
var tail = l123456.tail
var init = l123456.init
var take = l123456 take 3
var drop = l123456 drop 3
var at = l123456(3)
var reverse = l123456.reverse
var updated = l123456 updated (3, 100)
var indexOf = l123456 indexOf 4
var contains = l123456 contains 34


def last_Impl_Example[T](xs: List[T]): T = xs match {
  // empty list case
  case List() => throw new Error()
  // List with just one element
  case List(x) => x
  case y :: ys => last_Impl_Example(ys)
}

def init_Impl_Example[T](xs: List[T]): List[T] = xs match {
  // empty list case
  case List() => throw new Error()
  // List with just one element
  case List(x) => List()
  case y :: ys => y :: init_Impl_Example(ys)
}

def concat_Impl_Example[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat_Impl_Example(zs, ys)
}

def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n + 1)
removeAt(1, List('a', 'b', 'c', 'd'))

// Bad performance n*n, how can we do better?
def reverse_Init_Example[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse_Init_Example(ys) ++ List(y)
}
reverse_Init_Example(l123456)

/*
 * As a non-trivial example, let's define a function to sort lists that is more
 * efficient than insertion sort, the Merge Sort.
 */

def msortIntDraft(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (left, right) = xs splitAt n
    // merge with nested match (nested match is not really readable
    // and can also be done with one match on a pair...
    def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
      case Nil => ys
      case xHead :: xTail =>
        ys match {
          case Nil => xs
          case yHead :: yTail =>
            if (xHead < yHead) xHead :: merge(xTail, ys)
            else yHead :: merge(xs, yTail)
        }
    }
    merge(msortIntDraft(left), msortIntDraft(right))
  }
}



/* Pairs and Tuples */

val pair = ("answer", 42)
// pattern matching used for assignment
val (label, value) = pair
// instead of
val label1 = pair._1
val value1 = pair._2

val triple = ("answer", 42, "what does it mean?")


def msortInt(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (left, right) = xs splitAt n
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (xHead :: xTail, yHead :: yTail) =>
        if (xHead < yHead) xHead :: merge(xTail, ys)
        else yHead :: merge(xs, yTail)
    }
    merge(msortInt(left), msortInt(right))
  }
}

val nums = List(2,5,3,-1,4,-7,2,10,345)
msortInt(nums)

/* Make msort general with implicit parameter
*  For this we parameterize msortInt with a type T
*  But then we also need a compare function that is able to compare
*  value of type T
*
*  So we create a polymorphic msort function where we can also pass
*  a comparison function
*
* */

def msort_lessThen[T](xs: List[T])(lessThen: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (left, right) = xs splitAt n
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (xHead :: xTail, yHead :: yTail) =>
        if (lessThen(xHead, yHead)) xHead :: merge(xTail, ys)
        else yHead :: merge(xs, yTail)
    }
    merge(msort_lessThen(left)(lessThen), msort_lessThen(right)(lessThen))
  }
}

/* we can also do with pre defined Ordering from math package */

def msort_Ordering[T](xs: List[T])(ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (left, right) = xs splitAt n
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (xHead :: xTail, yHead :: yTail) =>
        if (ord.lt(xHead, yHead)) xHead :: merge(xTail, ys)
        else yHead :: merge(xs, yTail)
    }
    merge(msort_Ordering(left)(ord), msort_Ordering(right)(ord))
  }
}

/* And if we make the comparison function implicit the compiler will
 * choose the right one and we don't have to write it on a call
 */

def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (left, right) = xs splitAt n
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (xHead :: xTail, yHead :: yTail) =>
        if (ord.lt(xHead, yHead)) xHead :: merge(xTail, ys)
        else yHead :: merge(xs, yTail)
    }
    merge(msort(left), msort(right))
  }
}

msort(nums)
msort(List("banana", "apple", "organe", "pineapple"))

/* Higher Order Functions for List */

def squareList1(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList1(ys)
  }

def squareList2(xs: List[Int]): List[Int] = xs map (x => x * x)

squareList1(List(1,2,3))
squareList2(List(1,2,3))

nums filter (x => x > 0)
nums filterNot (x => x > 0)
nums partition (x => x > 0)

nums takeWhile (x => x > 0)
nums dropWhile (x => x > 0)
nums span (x => x > 0)

/* pack example
*
*  pack(List("a", "a", "a", "b", "c", "c", "a"))
*
*  List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))
* */

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs span (y => x == y)
    first :: pack(rest)
  }
}

pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs span (y => x == y)
    (first.head, first.length) :: encode(rest)
  }
}

encode(List("a", "a", "a", "b", "c", "c", "a"))

/* or by using map and pack*/
def encodeWithMap[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))


encodeWithMap(List("a", "a", "a", "b", "c", "c", "a"))

/* Reduction of Lists */

def sum(xs: List[Int]): Int = ( 0 :: xs ) reduceLeft((x, y) => x + y)
def product(xs: List[Int]): Int = ( 1 :: xs ) reduceLeft((x, y) => x * y)

// can also be written with _ shortcuts for the anonymous function parameter

def sum1(xs: List[Int]) = ( 0 :: xs ) reduceLeft(_ + _)
def product1(xs: List[Int]) = ( 1 :: xs ) reduceLeft(_ * _)

// folding with accumulator

def myFoldLeft[U, T](acc: U)(op: (U, T) => U)(xs: List[T]): U = xs match {
  case Nil => acc
  case head :: tail => myFoldLeft(op(acc, head))(op)(tail)
}

myFoldLeft[Int, Int](0)(_ + _)(List(1,2,3,4,5))
myFoldLeft[String, String]("0")(_ + _)(List("1","2","3","4","5"))

def myFoldRight[U, T](acc: U)(op: (U, T) => U)(xs: List[T]): U = xs match {
  case Nil => acc
  case head :: tail => op(myFoldRight(acc)(op)(tail), head)
}

myFoldRight[Int, Int](0)(_ + _)(List(1,2,3,4,5))
myFoldRight[String, String]("0")(_ + _)(List("1","2","3","4","5"))

/* concat with foldRight */
def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys)(_ :: _)
// xs.foldRight(ys)(_ :: _)

/* exercises */
def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, y) => y + 1)


mapFun[Int, Int](List(1,2,3), x => x + 4)
lengthFun(List(1,2,3))



/* Reasoning about correctness of code is also part of functional programming
 * by using structural induction
 *
 * For example the concatenation function on lists fulfills the
 * following rules:
 *
 * (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
 *        xs ++ Nil = xs
 *        Nil ++ xs = xs
 *
 * the natural induction in mathematics says that P(n) for all integers n >= b
 * we have to prove that P(b) is true
 * and we have to prove that P(b+1) is also true
 *
 * TODO: follow the videos and solve the last exercise with map
 *
 *
 */