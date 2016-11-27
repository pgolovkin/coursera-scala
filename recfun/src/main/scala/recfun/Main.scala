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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1);
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def calculateParanthesis(chars: List[Char], opened: Int): Boolean = {
      if (chars.isEmpty || opened < 0) {
        opened == 0
      } else {
        def head = chars.head;
        if (head == '(') calculateParanthesis(chars.tail, opened + 1)
        else {
          if (head == ')') calculateParanthesis(chars.tail, opened - 1)
          else calculateParanthesis(chars.tail, opened)
        }
      }
    }
    calculateParanthesis(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countFunction(restMoney: Int, coins: List[Int]): Int = {
      if (restMoney < 0 || coins.isEmpty) {
        0
      } else {
        if (restMoney == 0) {
          1
        } else {
          countFunction(restMoney, coins.tail) + countFunction(restMoney - coins.head, coins)
        }
      }
    }
    countFunction(money, coins)
  }
}
