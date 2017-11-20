import week3._

/*  Class hierarchies
 *
 *  In Scala everything is an object and extends from Object base class
 *
 *
 *
 *  Scala, like Java, knows about abstract class definitions which are
 *  rather the same like abstract classes in Java.
 *
 *
 *
 */

val listTest: List[Int] = new Cons(1, new Cons(2, new Cons(3, new Nil)))

/* abstract base class example with a implemented foo and a defined bar */
abstract class Base {
  def foo = 1
  def bar: Int
}

/* if we want to override bar we have to declare it */
class Sub extends Base {
  override def foo = 2
  // override declaration on implementations is optional
  /* override */ def bar = 3
}

/*
 *
 *  As example we define an abstract IntSet class that defines tww method,
 *  include to add a int to the set and contains to check if a particular
 *  int is in the set
 */

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int ): Boolean
  def union(other: IntSet): IntSet
}


/* We now can implement this within a binary search tree
 * Note that this binary search tree is a persistent data structure
 */

class NonEmpty(value: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int): IntSet = {
    //    println( value + ":" + x )
    if (x < value) new NonEmpty(value, left incl x, right)
    else if (x > value) new NonEmpty(value, left, right incl x)
    else this
  }

  def contains(x: Int): Boolean =
    if (x < value) left contains x
    else if (x > value) right contains x
    else true
  override def toString = "{" + left + value + right + "}"

  def union(other: IntSet): IntSet = {
    //println("**" + value)
    //((left union right) union other) incl value
    // this is a better performing version then the above because
    // the recursion terminates earlier
    left union (right union (other incl value))
  }
}

/*class*/ object Empty extends IntSet {
  def incl(x: Int): IntSet = {
 //   println( "empty:" + x )
    new NonEmpty(x, Empty, Empty)
  }
  def contains(x: Int): Boolean = false
  def union(other: IntSet): IntSet = other
  override def toString = "."
}



val t1 = new NonEmpty(3, Empty, Empty ) incl 4 incl 5 incl 16 incl 23 incl 11
val t2 = new NonEmpty(1, Empty, Empty ) incl 6 incl 7 incl 24 incl 55
t1 union t2
t2 union t1



/** Packages nad imports
  *
  */

new Rational(1,2)



/** Trait's
  *
  * A Trait are like an Interface in Java 8 or 9.
  * A class can inherit from one ore more Trait's and Trains can also
  * contains fields and method implementations
  *
  * So with Trait's we can achieve multi-inheritance in Scala
  *
  * Trait's can't have parameters/class values
  */

trait Planar {
  def height: Int               // a method declaration to get the height
  def width: Int                // a method declaration to get the width
  def surface = height * width  // a concrete default method that can be overridden
}

/** The Nothing and Null type
  *
  * The Nothing type, therefore no value exists, is used either for error
  * cases, exception handling or for empty collections
  */

def error1(msg: String) = throw new Error(msg)
def error2(msg: String): Nothing = throw new Error(msg)
//error1("test1")

val x = null
val y: String = x

/* Error:(97, 20) an expression of type Null is ineligible for implicit conversion
 * lazy val z: Int = null
 *                ^
 *  Error:(97, 20) type mismatch;
 *  found   : Null(null)
 *  required: Int
 *  lazy val z: Int = null
                  ^
val z: Int = null
*/

if (true) 1 else false // id of type AnyVal


/** Lists */

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

singleton[Int](1)
singleton[Int](2)
singleton[Boolean](true)

singleton(1)
singleton(true)


val list: List[Int] = new Cons(1, new Cons(2, new Cons(3, new Nil)))
list.nth(2)
//ist.nth(10)
list.nth(0)





