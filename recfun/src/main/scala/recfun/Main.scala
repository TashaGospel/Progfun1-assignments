package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def step(nOpenParen: Int, chars: List[Char]): Boolean = {
        if (nOpenParen < 0) false
        else if (chars.isEmpty) nOpenParen == 0
        else step(nOpenParen + (if (chars.head == '(') 1 else if (chars.head == ')') -1 else 0), chars.tail)
      }
      step(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (money == 0) 1
      else (if (money < coins.head) 0 else countChange(money - coins.head, coins)) + // Choose this coin
        countChange(money, coins.tail) // Don't choose this coin
    }
  }
