object intsets {
  val t1 = new NonEmpty(3, new Empty, new Empty)
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  override def incl(x: Int) = new NonEmpty(x, new Empty, new Empty)

  override def contains(x: Int) = false

  override def union(other: IntSet) = other
}

class NonEmpty(value: Int, left: IntSet, right: IntSet) extends IntSet {
  override def incl(x: Int) = {
    if (x < value) new NonEmpty(value, left incl x, right)
    else if (x > value) new NonEmpty(value, left, right incl x)
    else this
  }

  override def contains(x: Int) = {
    if(x > value) right contains x
    else if(x < value) left contains x
    else true
  }

  override def union(other: IntSet) = {
    ((left union right) union other) incl value
  }
}