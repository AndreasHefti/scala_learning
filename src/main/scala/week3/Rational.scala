package week3

/**
  * Created by andreashefti on 23.10.17.
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
  def this (x: Int) = this (x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val numer: Int = x / gcd(x, y)
  val denom: Int = y / gcd(x, y)

  def less(r: Rational): Boolean = numer * r.denom < r.numer * denom

  def max(r: Rational): Rational = if (less(r)) r else this

  def add(r: Rational) = new Rational(
    r.numer * this.denom + this.numer * r.denom,
    r.denom * this.denom
  )

  def sub(r: Rational): Rational = add(r.neg)

  def neg = new Rational(-numer, denom)

  // In Scala it is also possible to use symbols as method names
  // to allow to write mathematical syntax like rx < ry
  def +(r: Rational): Rational = add(r)

  def <(r: Rational): Boolean = less(r)

  def -(r: Rational): Rational = sub(r)

  def unary_- : Rational = neg

  // if override a method from super class we need to declare it with override
  override def toString() = numer + "⁄" + denom
}
