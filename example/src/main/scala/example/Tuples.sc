def mergesort(xs: List[Int]): List[Int] =
  if (xs.length < 2) xs
  else {

    def merge(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
      case (Nil, xs) => xs
      case (xs, Nil) => xs
      case (x :: xs, y :: ys) =>
        if (x < y) x :: merge(xs, right)
        else y :: merge(left, ys)
    }

    val (left, right) = xs.splitAt(xs.length / 2)
    merge(mergesort(left), mergesort(right))
  }

mergesort(List(3, 1, -2, 9, 5))
mergesort(List())
mergesort(List(2, 1))



def mergesort2[T](xs: List[T])(lessThan: (T, T) => Boolean): List[T] =
  if (xs.length < 2) xs
  else {

    def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, xs) => xs
      case (xs, Nil) => xs
      case (x :: xs, y :: ys) =>
        if (lessThan(x, y)) x :: merge(xs, right)
        else y :: merge(left, ys)
    }

    val (left, right) = xs.splitAt(xs.length / 2)
    merge(mergesort2(left)(lessThan), mergesort2(right)(lessThan))
  }

mergesort2(List(3, 1, -2, 9, 5))((x, y) => x < y)
mergesort2(List(3, 1, -2, 9, 5))((x, y) => x > y)


import math.Ordering

def mergesort3[T](xs: List[T])(ord: Ordering[T]): List[T] =
  if (xs.length < 2) xs
  else {

    def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, xs) => xs
      case (xs, Nil) => xs
      case (x :: xs, y :: ys) =>
        if (ord.lt(x, y)) x :: merge(xs, right)
        else y :: merge(left, ys)
    }

    val (left, right) = xs.splitAt(xs.length / 2)
    merge(mergesort3(left)(ord), mergesort3(right)(ord))
  }

mergesort3(List(3, 1, -2, 9, 5))(Ordering.Int)



def mergesort4[T](xs: List[T])(implicit ord: Ordering[T]): List[T] =
  if (xs.length < 2) xs
  else {

    def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, xs) => xs
      case (xs, Nil) => xs
      case (x :: xs, y :: ys) =>
        if (ord.lt(x, y)) x :: merge(xs, right)
        else y :: merge(left, ys)
    }

    val (left, right) = xs.splitAt(xs.length / 2)
    merge(mergesort4(left), mergesort4(right))
  }

mergesort4(List(3, 1, -2, 9, 5))