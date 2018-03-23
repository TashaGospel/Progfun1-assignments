package example

trait List[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty = true

  def head = throw new NoSuchElementException("Nil.head")

  def tail = throw new NoSuchElementException("Nil.tail")
}

object List {
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))

  def apply[T](x1: T): List[T] = new Cons(x1, Nil)

  def apply[T](): List[T] = Nil
}

object test {
  val x: List[String] = Nil
}
