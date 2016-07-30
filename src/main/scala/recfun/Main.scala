package recfun

import scala.annotation.tailrec

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
    if (c == 0 || c == r) return 1
    if (c == 1 || c == r - 1) return r

    pascal(c - 1, r - 1) + pascal(c + 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def loop(remainingChars: IndexedSeq[Char], leftParenCount: Int): Boolean = {
      if (remainingChars.isEmpty) {
        return leftParenCount == 0
      }

      val char = remainingChars.head
      if (char == ')' && leftParenCount <= 0) {
        return false
      }

      val newLeftParenCount = if (char == '(') {
        leftParenCount + 1
      } else if (char == ')') {
        leftParenCount - 1
      } else {
        leftParenCount
      }

      loop(remainingChars.tail, newLeftParenCount)
    }

    loop(chars.toIndexedSeq, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      return 0
    }

    def loop(acc: Int, sum: Int, remainingCoins: List[Int]): Int = {
      if (sum == money) {
        return acc + 1
      }

      if (sum > money || remainingCoins.isEmpty) {
        return acc
      }

      val newSum = sum + remainingCoins.head

      loop(loop(acc, sum, remainingCoins.tail), newSum, remainingCoins)
    }

    loop(0, 0, coins)
  }
}
