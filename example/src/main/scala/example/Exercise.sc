def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }

  loop(a, 0)
}

val sumSquares = sum((x: Int) => x * x) _
sumSquares(0, 4)

def sum2(f: Int => Int) = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)

  sumF _
}

sum2((x: Int) => x)(0, 10)

def sum3(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sum3(f)(a + 1, b)
}

val sumSquares3 = sum((x: Int) => x * x) _
sumSquares3(0, 4)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

product(x => x)(1, 5)

def fact(n: Int) = product(x => x)(1, n)
fact(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a: Int, b: Int): Int =
  if (a > b) unit
  else combine(f(a), mapReduce(f, combine, unit)(a + 1, b))

def product2(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

product2(x => x)(a = 1, b = 5)

def productOfSquares = product2(x => x * x) _

productOfSquares(v1 = 1, v2 = 6)
productOfSquares(0, 1)
productOfSquares(1, 5)
