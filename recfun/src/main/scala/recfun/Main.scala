package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if (c == 0)
      1
    else if (r == 0)
      1
    else if (r == c)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def f(str: List[Char], i: Int): Boolean = {
      if (i < 0)
        false
      else if (str.isEmpty && i != 0)
        false
      else if (str.isEmpty && i == 0)
        true
      else {
        if (str.head == '(')
          f(str.tail, i + 1)
        else if (str.head == ')')
          f(str.tail, i - 1)
        else
          f(str.tail, i)
      }
    }

    f(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var rstCnt = 0;
    def changeCnt(money: Int, coins: List[Int]) {
      if (money == 0) {
        rstCnt = rstCnt + 1
      } else if (money < 0) {
    	return
      } else {
        var list = coins
        while (!list.isEmpty) {
          changeCnt(money - list.head, list)
          list = list.tail
        }
      }
    }
    changeCnt(money, coins.toSet.toList)
    rstCnt
  }
}
