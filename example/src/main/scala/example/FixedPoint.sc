def abs(x: Double) = if (x < 0) -x else x



def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) < 0.0001

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def step(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, f(guess))) guess
    else step(next)
  }

  step(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

fixedPoint(x => 1 + x / 2)(1)

fixedPoint(averageDamp(y => 2 / y))(1)

val foo = averageDamp(2 / _) _


