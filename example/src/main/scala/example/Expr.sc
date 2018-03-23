trait Expr {
  def isNumber: Boolean

  def isSum: Boolean

  def numValue: Int

  def leftOp: Expr

  def rightOp: Expr
}

class Num(n: Int) extends Expr {
  def isNumber = true

  def isSum = false

  def numValue = n

  def leftOp = throw new Error("Not a Sum")

  def rightOp = throw new Error("Not a Sum")
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  def isNumber: Boolean = false

  def isSum: Boolean = true

  def numValue: Int = throw new Error("Not a Num")

  def leftOp: Expr = e1

  def rightOp: Expr = e2
}

def eval(e: Expr): Int = {
  if (e.isNumber) e.numValue
  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error("Unknown expression")
}

eval(new Sum(new Num(1), new Num(4)))