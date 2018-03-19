package example

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  val numer = x
  val denom = y

  def +(that: Rational) =
    new Rational(
      numer * that.denom + denom * that.numer,
      denom * that.denom
    )

  def unary_- = new Rational(-numer, denom)

  def -(that: Rational) = this + -that

  def <(that: Rational) = numer * that.denom < denom * that.numer

  def max(that: Rational) = if (<(that)) that else this

  override def toString: String = {
    val g = gcd(x, y)
    numer / g + "/" + denom / g
  }
}
