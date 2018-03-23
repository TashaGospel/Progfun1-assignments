List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
1 :: 2 :: Nil

def insertionSort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, insertionSort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys =>
    if (x <= y) x :: xs
    else y :: insert(x, ys)
}

insertionSort(List(3, 2, 1))

insert(4, List(1, 3))