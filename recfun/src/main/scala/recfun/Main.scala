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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(begin: List[Char], end: List[Char]): Boolean = {
      if (end.isEmpty)
        true
      else {
        if (end.head == ')')
          begin.nonEmpty && balanceIter(begin.dropRight(1), end.tail)
        else if (end.head == '(')
          balanceIter(begin :+ '(', end.tail)
        else
          balanceIter(begin, end.tail)
      }
    }
    balanceIter("".toList, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money > 0 && coins.nonEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }
}
