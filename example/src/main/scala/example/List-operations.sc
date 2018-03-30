val xs = List(1, 3, 4, 5, 6)
xs.length
xs.last
xs.init
xs take 3
xs drop 2
xs(1)

val ys = List(2, 4, 6, 8)
xs ::: ys
xs ++ ys
xs.reverse
xs.updated(0, 2)

xs indexOf 3
xs indexOf 100
xs contains 3
xs contains 100

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty List")
  case List(x) => x
  case y :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("last of empty List")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => List()
  case y :: ys => reverse(ys) ::: List(y)
}

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
  case List() => List()
  case y :: ys => if (n == 0) ys else y :: removeAt(n - 1, ys)
}

def removeAt2[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)

removeAt(1, List(0, 1, 2))
removeAt(-1, List(0, 1, 2))

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case (y: List[Any]) :: ys => flatten(y) ::: flatten(ys)
  case y :: ys => y :: flatten(ys)
}

flatten(List(List(0, List(1))))
flatten(List(List(1, 1), 2, List(3, List(5, 8))))