package week4

/**
  * Natural Number with Piano-Number implementation
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new Error("predecessor on Zero")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (this.isZero) this else throw new Error("negative")
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat = if (this.isZero) this else n - that.predecessor
}