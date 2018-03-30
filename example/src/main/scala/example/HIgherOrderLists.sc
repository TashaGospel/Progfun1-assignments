val xs = List(1, 2, -3, -4, 5, 6)

xs.map(x => x * x)

xs.filter(x => x > 3)
xs filterNot (x => x > 3)
xs partition (x => x > 3)

xs takeWhile (x => x > 0)
xs dropWhile (x => x > 0)
xs span (x => x > 0)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

pack(List("a", "a", "a", "b", "b", "c", "a", "a"))

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))

encode(List("a", "a", "a", "b", "b", "c", "a", "a"))


def sum(xs: List[Int]): Int = 0 :: xs reduceLeft (_ + _)
def sum2(xs: List[Int]) = xs.foldLeft(0)(_ + _)


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]()) (f(_) :: _)

mapFun(xs, (x: Int) => x * x)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0) ((_, res) => res + 1)

lengthFun(xs)

def reverseFun[T](xs: List[T]): List[T] =
  (xs foldLeft List[T]()) ((res, x) => x :: res)

reverseFun(xs)