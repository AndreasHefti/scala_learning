def cube(x: Int): Int = x * x * x;

/** Higher Order Functions
  *
  * Function parameter definition: m( f: Int => Int ) defines a function
  * from Int to Int as a function parameter.
  */

def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f( a ) )
  }
  loop(a, 0)
}

sum( x => x, 1, 4 )
sum( (x: Int) => (x: Int), 1, 4 )

/** A curried function can be defined by returning a function as shown below
  */

def sumCurried(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b)
      0
    else
      f(a) + sumF(a + 1, b)
  sumF
}

def sumIntsCurried = sumCurried( x => x )
def sumCubesCurried = sumCurried( cube )

sumIntsCurried( 1, 4 )
sumCubesCurried( 1, 4 )

/** In Scala it is also possible to separate the function parameter like shown
  * in the following example with a corresponding function call.
  *
  * This is syntactic sugar for curried function definitions where it is possible
  * to partial apply the separated definitions
  */
def sumSep(f: Int => Int)( a: Int, b: Int) = sum( f, a, b )

def sumInts = sumSep( x => x )
def sumCubes = sumSep( cube )

sumInts( 1, 4 )
sumCubes( 1, 4 )


def sumCurried2(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b)
    0
  else
    f(a) + sumCurried2(f)(a + 1, b)
}

sumCurried2(cube)(1, 4)

def sumCurried3(f: Int => Int)(a: Int)( b: Int): Int = {
  if (a > b)
    0
  else
    f(a) + sumCurried3(f)(a + 1)( b)
}

sumCurried3(cube)(1)(4)
///sumCurried3 cube 1 4



def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b)
    1
  else
    f(a) * product(f)(a + 1, b)
}

product(cube)(1, 4)

def factorial(x: Int) = product( x => x )(1, x)

factorial(1)
factorial(2)
factorial(3)


/** A more general version is the map reduce where also the reduce method is
  * a function parameter
  */

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, identity:Int)(a: Int, b: Int ): Int = {
  if (a > b)
    identity
  else
    combine(f(a), mapReduce(f, combine, identity)(a + 1, b) )
}

def sumInt(x: Int, y: Int): Int = x + y
def productInt(x: Int, y: Int): Int = x * y

mapReduce(cube, sumInt, 0)(1, 4)

def productWithMapReduce(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x, y) => x * y, 1)(a, b)


/** Types????
  *
  */

type FIntToBool = Int => Boolean

def contains( f: FIntToBool, value: Int ): Boolean = f(value)