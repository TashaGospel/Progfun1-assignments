1 -> 2
val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
romanNumerals("V")
//romanNumerals("C")
romanNumerals get "C"
romanNumerals get "V"

romanNumerals + ("IV" -> 4)

val fruit = List("apple", "pear", "orange", "pineapple")
fruit sortBy (_.length)
fruit.sortBy(_.length)(Ordering[Int].reverse)
fruit sortWith (_.length < _.length)
fruit.sorted

fruit groupBy (_.head)

case class Poly(terms0: Map[Int, Double]) {

  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

  def +(other: Poly) =
    Poly(other.terms.foldLeft(terms)((terms, term) => {
      val (exp, coeff) = term
      terms.updated(exp, coeff + terms(exp))
    }))

  //    Poly(terms ++ (other.terms map {
  //      case (exp, coeff) => exp -> (coeff + terms(exp))
  //    }))

  override def toString: String =
    (for ((exp, coeff) <- terms.toList.sorted.reverse)
      yield s"${coeff}x^$exp") mkString " + "
}

object Poly {
  def apply(terms: Map[Int, Double]) = new Poly(terms)

  def apply(bindings: (Int, Double)*) = new Poly(bindings.toMap)
}

val p1 = Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2