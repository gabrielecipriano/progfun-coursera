abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}


object Zero extends Nat {
  override def isZero = true

  override def predecessor = throw new UnsupportedOperationException

  override def successor = new Succ()

  override def +(that: Nat) = that

  override def -(that: Nat) = throw new UnsupportedOperationException
}

class Succ(n: Nat) extends Nat {
  override def isZero = false

  override def predecessor = ???

  override def successor = ???

  override def +(that: Nat) = add(n, that)

  override def -(that: Nat) = minus(n, that)

  def add(n: Nat, that: Nat): Nat = that match {
    case Zero => n
    case _ => add(n.successor, that.predecessor)
  }

  def minus(n: Nat, that: Nat): Nat = that match {
    case Zero => n
    case _ => minus(n.predecessor, that.predecessor)
  }
}