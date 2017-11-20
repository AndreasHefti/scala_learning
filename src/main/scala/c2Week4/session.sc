import c2Week4.{BankAccount, Consolidator}
import frp.{Signal, StackableVariable, Var}


// Observer pattern and reactive programming

val a, b = new BankAccount
val c = new Consolidator(List(a, b))

c.totalBalance
a deposit 20
c.totalBalance
b deposit 30
c.totalBalance

// this was a imperative solution.
// looking at the implementations of publisher and subscriber there are
// a lot of methods (giving Unit back) and there are also many moving
// parts that needs to be coordinated
// no concurrency handling and views are still bound the one state
// because view update happens immediately
// this solution is far from being functional

// Signals

val sv1 = new StackableVariable[Int](1)
sv1.withValue(2) (println("values:" + sv1.value))
sv1.withValue(3) (println("values:" + sv1.value))




val v1 = Var(2)


val s1 = Signal(v1() + 5)

s1()

v1() = 4

s1()

v1()

v1()

// This works



