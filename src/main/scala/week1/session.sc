/** Difference between val, var and def
  *
  * The val (Value) declaration is immediately resolved to a value
  * and is static, not changeable after assignment while the var
  * (Variable) declaration is also immediately resolved but also
  * changeable like a usual variable.
  * The def declaration is like a Call By Name and is resolved
  * every time it is used.
  *
  */

def getSome() : Int = {
  print( "getSome: called " )
  12
}

val x: Int = getSome
var y: Int = getSome

def z: Int = getSome
val z1 = z
val z2 = z


/** Call by Name and Call by Value
  *
  * Call By Value means that an expression is resolved to a value
  * before passing as argument to a function. For example:
  *
  * add( 1 + 2, 3 * 4 )
  * --> add( 3, 3 * 4 )
  * --> add( 3, 12 )
  * --> 3 + 12
  * --> 15
  *
  * Call By Name means that an expression is passed as an argument
  * to a function and resolved when it is used in the function
  *
  * add( 1 + 2, 3 * 4 )
  * --> ( 1 + 2 ) + ( 3 * 4 )
  * --> 3 + ( 3 * 4 )
  * --> 3 + 12
  * --> 15
  *
  * Normally scala uses Call By Value but we can force to use Call by Name
  * on a method/function definition by using declare with => after the
  * argument name and before the argument type declaration:
  */

def printGet() : Int = {
  print( "printGet: " )
  4
}

def addCallByValue( x:Int, y:Int ) : Int = {
  print( "add: " )
  x + y
}

def addCallByName( x:Int, y: => Int ) : Int = {
  print( "add: " )
  x + y
}

addCallByValue( 3, printGet() )
addCallByName( 3, printGet() )

/** Blocks and lexical scope
  *
  */

def sqrt(x: Double) = {
  def abs(x: Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) < 0.001 * x

  def improve(guess: Double) = (guess + x / guess) / 2

  sqrtIter(1.0)
}



sqrt( 2 )
sqrt( 4 )
sqrt( 1.0e-20)
sqrt( 1.0e20)
sqrt( 1.0e50)

/** Recursion And Tail-Call-Recursion
  */

def factorialNotTailRecursive( n:Int ):Int =
  if ( n == 0 ) 1 else n * factorialNotTailRecursive( n - 1 )

def factorialTailRecursive( n:Int ):Int = {
  def _fac( n:Int, acc:Int ):Int =
    if ( n == 0 ) acc else _fac( n - 1, n * acc )

  _fac( n, 1 )
}

factorialNotTailRecursive( 4 )
factorialTailRecursive( 4 )
factorialNotTailRecursive( 6 )
factorialTailRecursive( 6 )


