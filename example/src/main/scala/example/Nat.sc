// Peano numbers

abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  override def isZero = true

  override def predecessor: Nat = throw new NoSuchElementException

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat =
    if (that.isZero) this
    else throw new Error("Result is negative")
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat =
    if (that.isZero) this
    else n - that.predecessor
}

new Succ(new Succ(Zero)) + new Succ(Zero)