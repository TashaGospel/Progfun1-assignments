abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def incl(x: Int) = new NonEmpty(x, Empty, Empty)

  def contains(x: Int) = false

  def union(other: IntSet) = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def incl(x: Int) =
    if (x < elem) new NonEmpty(elem, left.incl(x), right)
    else if (x > elem) new NonEmpty(elem, left, right.incl(x))
    else this

  def contains(x: Int) =
    if (x < elem) left.contains(x)
    else if (x > elem) right contains x
    else true

  def union(other: IntSet) = other union left union right incl elem

  override def toString: String = "{" + left + elem + right + "}"
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1.incl(15)
val t3 = new NonEmpty(5, Empty, Empty)
t2 union t3

//val a: Array[NonEmpty] = Array(t1)
//val b: Array[IntSet] = a
//b(0) = Empty
