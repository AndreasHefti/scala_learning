/** A fix point of a function f(x) is the number
  * for that f(x) = x.
  *
  * for some functions we can locate the fix-point
  * with the following method
  *
  * x, f(x), f(f(x)), f(f(f(x)))...
  *
  * as long as the result approximates close enough.
  * And this is also the Nowton-Approximation method
  * we used for sqrt(x) in a previous example.
  *
  */

def abs(x: Double) = if (x < 0) -x else x;

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x ) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next))
      next
    else
      iterate(next)
  }

  iterate(firstGuess)
}

fixedPoint( x => 1 + x / 2)(1)

/** sqrt(x) is specified as
  * sqrt(x) = the number y such that y * y = x
  * sqrt(x) = the number y such that y = x / y
  *
  * so sqrt(x) is the fixed point of the function
  *  y => x / y
  */

def sqrtNotWorking(x: Double) = fixedPoint(y => x / y)(1)

/** NOTE: this above function is not working correctly
  * because it oscillates between 1 and 2
  *
  * In such a case a average dump is a good method to
  * solve the problem. average dump means that we
  * take the average of the last guess and the new guess
  *
  */


def sqrtAD(x: Double) = fixedPoint(y => (y + x / y) / 2)(1)

sqrtAD(2)

def averageDump(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt(x: Double) =  fixedPoint(averageDump(y => x / y) )(1)
sqrt(2)