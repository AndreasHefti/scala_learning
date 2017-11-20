/** To introduce data structure we will implement a library to
  * handling rational numbers.
  *
  * Remembering a rational number can be defined with two integers
  * A rational number is x / y where x is the numerator and the y
  * is the denominator.
  *
  * So we can create a data structure for a rational number defined by
  * two integers
  *
  */

val x = new Rational(1, 2)
x.numer
x.denom

/** function declaration for a addRational function */
def addRational(r: Rational, s: Rational): Rational =
  new Rational(
    r.numer * s.denom + s.numer * r.denom,
    r.denom * s.denom
  )

def makeString(r: Rational) = r.numer + "⁄" + r.denom

makeString(addRational(new Rational(1, 2), new Rational(2, 3)))

/** Or implements within a method directly in the class Rational*/

val rx = new Rational(1, 3)
val ry = new Rational(5, 7)
val rz = new Rational(3, 2)

rx.sub(ry).sub(rz)
x.sub(ry.sub(rz))

ry.add(ry)

rx.less(ry)
rx.max(ry)

/** In Scala a method with a parameter can be used as a infix
  * operator and it is possible to write:
  */

rx sub ry sub rz
rx less ry
rx max ry

rx < ry

/** The above gives the correct result but not the simplest form
  * of the rational data when no gcd is applied.
  *
  * We can perform to compute the simplest form within a function
  * in Rational but where is the best place to only do it once?
  */




class Rational(x: Int, y: Int) {
  /** We can define require inside a class to prove some required
    * stuff. require is a scala 'global' function that takes a
    * condition and an error message
    */
  require(y != 0, "denominator must be none-zero")

  /** Another constructor that call the default constructor with x and 1
    *
    * @param x
    * @return
    */
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b:Int): Int = if (b==0) a else gcd(b, a % b)
  val numer: Int = x / gcd(x, y)
  val denom: Int = y / gcd(x, y)

  def less(r: Rational): Boolean = numer * r.denom < r.numer * denom

  def max(r: Rational): Rational = if (less(r)) r else this

  def add(r: Rational): Rational =
    new Rational(
      r.numer * this.denom + this.numer * r.denom,
      r.denom * this.denom
    )

  def sub(r: Rational): Rational = add(r.neg)
  def neg: Rational = new Rational( -numer, denom )

  // In Scala it is also possible to use symbols as method names
  // to allow to write mathematical syntax like rx < ry
  def +(r: Rational): Rational = add(r)
  def <(r: Rational): Boolean = less(r)
  def -(r: Rational): Rational = sub(r)
  def unary_- : Rational = neg

  // if override a method from super class we need to declare it with override
  override def toString() = numer + "⁄" + denom;
}