import example._
//import Thing.x

new Rational(1, 2)
example.Thing.x


trait Planar {
  val width: Double
  val height: Double
  val area: Double = width * height
}

class Rectangle(val width: Double, val height: Double) extends Planar {
  def getArea: Double = area
}

new Rectangle(5, 2).getArea

def error(msg: String) = throw new Error(msg)

//error("hello")

def getNull: String = null

if (true) 1 else false

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

singleton[Int](1).head
singleton("Hello").head

def nth[T](list: List[T], n: Int): T =
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) list.head
  else nth(list.tail, n - 1)

nth(new Cons(1, new Cons(2, singleton(3))), -1)

