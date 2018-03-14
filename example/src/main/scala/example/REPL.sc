//def loop: Int = loop
//
//def constOne(x: Int, y: => Int) = x
//
//constOne(1 + 2, loop)
//
//def and(x: Boolean, y: => Boolean) = if (!x) x else y
//def or(x: Boolean, y: => Boolean) = if (x) x else y
//
//and(false, false)

def abs(x: Double) = if (x < 0) -x else x

def sqrt(x: Double): Double = {

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x) / x < 0.001
  //  abs(guess * guess - x) < 0.001

  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2

  sqrtIter(1, x)
}

sqrt(4)
sqrt(1e-60)
sqrt(1e60)

def foo(x: Int) = x + 1
val foo1 = (x: Int) => x + 1
val foo2: Int => Int = _ + 1

foo(3)

def factorial(n: Int) = {
  def step(res: Int, n: Int): Int =
    if (n == 0) res
    else step(res * n, n - 1)
  step(1, n)
}

factorial(10)