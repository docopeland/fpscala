package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =  {
    if (c == 0 || c == r) 1
    else {
      val ans = pascal(c-1, r-1) + pascal(c,r-1)
      ans
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper(n: Int, phrase: List[Char]): Int ={
      if ((n < 0 || phrase.isEmpty) == true)  n
      else {
        if (phrase.head.equals('(')) helper(n+1,phrase.tail)
        else {
          if (phrase.head.equals(')')) helper(n - 1, phrase.tail)
          else helper(n, phrase.tail)
        }
      }
    }
    if (helper(0,chars) == 0) true else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else {
      if(money > 0 && !coins.isEmpty) {
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
      else 0
    }
  }
}
