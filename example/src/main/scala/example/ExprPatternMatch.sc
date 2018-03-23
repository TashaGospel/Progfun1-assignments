trait Expr

case class Num(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

case class Var(s: String) extends Expr

def eval(e: Expr): Int = e match {
  case Num(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

eval(Sum(Num(1), Num(2)))

def show(e: Expr): String = e match {
  case Num(n) => n.toString
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
  case Var(s) => s
  case Prod(e1, e2) =>
    def wrapParen(e1: Expr) = e1 match {
      case Sum(_, _) => "(" + show(e1) + ")"
      case x => show(x)
    }

    wrapParen(e1) + " * " + wrapParen(e2)
}

show(Prod(Sum(Num(3), Var("x")), Var("y")))


trait Expr2 {
  def eval: Int = this match {
    case Num2(n) => n
    case Sum2(e1, e2) => e1.eval + e2.eval
  }
}

case class Num2(n: Int) extends Expr2

case class Sum2(e1: Expr2, e2: Expr2) extends Expr2

Sum2(Num2(3), Num2(2)).eval

