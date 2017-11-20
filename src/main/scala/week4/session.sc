import week4.{Bool, FALSE, TRUE}

TRUE == FALSE
TRUE == TRUE

FALSE < TRUE
FALSE < FALSE
TRUE < TRUE
TRUE < FALSE

val list = week4.List[Int](1, 2)

/* Type bounds
 *
 *  int Java <T extends IntSet> is equivalent to
 *  Scala    [T <: IntSet>
 *
 *  <: IntSet is a upper bound of the type T
 *
 *  generally:
 *
 *  T <: S means: T is a subtype of S
 *  T :> S means: T is a supertype of S or S is a subtype of T
 *
 *  In Scala als lower bounds can be defined for types:
 *
 *  [S >: NonEmpty]
 *
 *  In this case S can only range over supertypes of NonEmpty
 *  --> NonEmpty, IntSet, AnyRef, Any
 *
 *  And it is also possible to define lower and upper bound like
 *
 *  [S >: NonEmpty <: IntSet]
 *
 *  In this case S can be NonEmpty or IntSet
 *
 *  covariance:
 *
 *  NonEmpty <: IntSet --> NonEmpty is a subtype of IntSet
 *
 *  therefore should also
 *
 *  List[NonEmpty] <: List[IntSet] a list of NonEmpty be a subtype of List[IntSet]
 *
 *  if this is true then the relationship is covariant
 *  In Scala List is covariant, arrays are not covariant
 *  What should be covariant ant what not?
 *  What is the difference between List and Array?
 *
 *  --> List is immutable while Array is mutable
 *
 *  actually there are three cases if you have A <: B then
 *  C[A] <: C[B]   is covariant
 *  C[A] >: C[B]   is contravariant
 *  C[A] x  C[B]   is nonvariant
 *
 *  Scala lets you declare the variance of a type by annotation like
 *  class C[+A] {...} C is covariant
 *  class C[-A] {...} C is contravariant
 *  class C[A] {...} C is nonvariant
 *
 *  function from   A2 => B2
 *    contravariant >     : covariant
 *                  :     >
 *                  A1 => B1
 *
 *  Functions are contravariant in their argument type(s) and
 *  covariant in their result type
 *
 *  trait Function[-T, +U] {
 *    def apply(x. T): U
 *  }
 *
 *  rules
 *
 *  If a classes declares a covariant or contravariant type
 *  with + or - prefix, then the Scala compiler
 *  checks the rule:
 *  contravariant in their argument type(s) and
 *  covariant in their result type
 *
 *  If this rule is broken the compiler shows an error
 *  Normally methods that seems to mutates the state of an object
 *  or just have a covariant argument like the
 *  prepend method in the new List are a problem and
 *  can mostly be solved with a definition of a lower bound
 *  as is done in prepend of List
 *
 */

/* Decomposition
 *
 * Means if you have a class hierarchy and a function
 * or method that should do something on a composite
 * tree of this types some type check ist used.
 *
 * This can be a method on each type that indicates this
 * type or the use of an instance of check what is not recommended
 * or some other check that that can be used to differ
 * the types in a if-elseif-elseif... else block
 *
 * Decomposition means that we instead of implementing this
 * method function in one we decompose ist in a manner that
 * every subtype of the type hierarchy has to implement
 * its particular part of this method/function
 *
 * this works but has also has some disadvantages like to
 * have to touch many classes and files to implement a new
 * method/function.
 * Or if the function needs a knowledge of the while composite
 * but also the knowledge of the subtypes of the leafs,
 * decomposition is not a solution
 *
 */

/* Pattern Matching
 *
 *  Pattern Matching means, the possibility to remember/save
  * the construction of an object from a specific type and construction
  * attribute.
  *
  * In Scala this is possible within case classes. Within case classes,
  * the Scala compiler automatically generates object companions that
  * acts as a factory and reference.
  * This also leads to the fact that to construct case classes the new
  * operator is not used. One can just call the Number(3) that calls the
  * corresponding apply method in the auto generated companion object
  *
  * Patterns are constructed from:
  *
  * constructors: Number(n) or Number(_) where the _ means we need no n for
  * the following expression.
  * variables: like n, e1, e2,
  * wildcard patterns. like _
  * constants: 1, true or literals "some"
  *
  * wildcard are very powerful. For example a wildcard can be like:
  * Sum(Number(1), Number(2))
  * this would match only the case were e is a Sum with a left Number
  * operand that has the value 1 and a right Number operand that
  * has the value 2
  *
  *
 */

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
    case Var(_, e1) => e1.eval
    // if no pattern matches a MatchError is thrown
  }

  def show: String = this match {
    case Number(n) =>  String.valueOf(n)
    case Sum(e1, e2) => e1.show + "+" + e2.show
    case Prod(Sum(e1, e2), e3) => "(" + e1.show + "+" + e2.show + ")*" + e3.show
    case Prod(e1, Sum(e2, e3)) => e1.show + "*(" + e2.show + "+" + e3.show + ")"
    case Prod(e1, e2) => e1.show + "*" + e2.show
    case Var(name, _) => name
  }
}
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(name: String, e1: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

var n1 = Number(2)

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
  // if no pattern matches a MatchError is thrown
}

eval(Sum(Number(1), Number(2)))
Sum(Number(1), Number(2)).eval

Sum(Number(1), Number(2)).show

Sum(Prod(Number(2), Var("x", Number(1))), Var("y", Number(2))).show
Prod(Sum(Number(2), Var("x", Number(1))), Var("y", Number(2))).show


/*  The Expression Problem

    Is the question whether we should use Decomposition or not and mostly
    depends on a question the the future process of development whether
    it is more possible to write new methods for the whole hierarchy or
    to write more often new types for the hierarchy



 */



