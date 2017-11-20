import c2Week3.BankAccount
// state and state handling

val ba1 = new BankAccount
ba1.deposit(100)
ba1.withdraw(50)
ba1.withdraw(20)
ba1.withdraw(20)

/* operational equivalence
 *
 * val x = new BankAccount
 * val y = new BankAccount
 *
 * id x == y ??
 *
 * is f(x, y) == f(x, x) for every possible "test" case? if yes
 * we can say that x is equivalent to y
 */

val x = new BankAccount
val y = new BankAccount

def testBankAccounts(x: BankAccount, y: BankAccount) = {
  x deposit 30
  y deposit 50
  y withdraw 20
}

testBankAccounts(x, x)
testBankAccounts(x, y)

testBankAccounts(x, x) == testBankAccounts(x, y)

// loops
/* A while loop can be defines as function in scala as seen below
 * The condition and command are past by name so that they are evaluated
 * each time they are called
 * And the WHILE is tail recursive
 */

def WHILE(condition: => Boolean)(command: => Unit): Unit =
  if (condition) {
    command
    WHILE(condition)(command)
  } else ()

var i: Int = 0;
WHILE(i < 10) {
  print(i)
  i = i + 1
}

def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
  command
    if (condition) ()
    else REPEAT(command)(condition)
  }

i = 0;
REPEAT {
  print(i)
  i += 1
} (i == 10)

// can we support a UNTIL (condition) syntax for this function in scala?

def repeat(command: => Unit) = new AnyRef {
  def until(condition: => Boolean): Unit = {
    command
    if (condition) ()
    else until(condition)
  }
}

// In this solution a structural type is used

i = 0
repeat {
  print(i)
  i += 1
} until (i == 10)

trait Until {
  def until(condition: => Boolean): Unit
}
def rep(command: => Unit) = new Until {
  def until(condition: => Boolean): Unit = {
    command
    if (condition) ()
    else until(condition)
  }
}

i = 0
rep {
  print(i)
  i += 1
} until (i == 10)

// in scala we have a do
i = 0
do {
  print(i)
  i += 1
} while (i < 10)

// natural for is not supported in scala and also cannot be defined within
// a higher order function
// we have the for expression

for( i <- 0 until 10 ) {print(i)}

