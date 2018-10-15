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
    if(c == 0 || c == r) 1
    else pascal(c-1,r-1) +  pascal(c,r-1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], balanceNum: Int): Boolean = chars match {
      case Nil => balanceNum == 0
      case ')' :: _ if balanceNum - 1 < 0 => false
      case '(' :: tail => loop(tail, balanceNum + 1)
      case ')' :: tail if balanceNum - 1 >= 0 => loop(tail, balanceNum - 1)
      case _ :: tail => loop(tail, balanceNum)
    }

    loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty) 0
      else if (money < 0 && coins.nonEmpty) 0
      else loop(money - coins.head, coins) + loop(money, coins.tail)
    }

    if (coins.isEmpty || money <= 0) 0
    else loop(money, coins)
  }
}
