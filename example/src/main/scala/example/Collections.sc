1 +: List()
1 +: Vector()
Vector(1, 2, 3)
Vector(1, 2, 3) ++ List(4, 5)
Vector(1, 2, 3)


1 to 5
1 until 5
(1 to 10 by 2).to
(1 to 10 by -1).to

// Seq methods
val xs = 1 to 5

xs.exists(x => x % 2 == 0)
xs forall (x => x % 2 == 0)

xs zip (11 to 25)
(xs zip (11 to 25)).unzip

xs flatMap (x => List(x, x * x))

xs.sum
xs.product
xs.max
xs.min

def sum2: ((Int, Int)) => Int = {
  case (x, y) => x + y
}
sum2((1, 2))



val fruit = Set("apple", "banana", "pear")
val s = (1 to 6).toSet

s map (_ + 2)
fruit filter (_ startsWith "a")
s.nonEmpty

s map (_ / 2)

// Solution to the N-Queens problem
def queens(n: Int): Set[List[Int]] = {
  def isSafe(col: Int, queens: List[Int]): Boolean =
    !(queens contains col) && {
      val row = queens.length
      val queensWithRow = (row - 1 to 0 by -1) zip queens
      queensWithRow.forall {
        case (r, c) => row - r != math.abs(col - c)
      }
    }

  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else for {
      queens <- placeQueens(k - 1)
      col <- 0 until n
      if isSafe(col, queens)
    } yield col :: queens

  placeQueens(n)
}

queens(4)

def showQueens(queens: List[Int]) = {
  val lines = queens.reverse.map(Vector.fill(queens.length)("* ").updated(_, "X ").mkString)
  "\n" + lines.mkString("\n")
}

(queens(8) take 3 map showQueens) mkString "\n"