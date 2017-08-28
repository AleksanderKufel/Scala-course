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
    def pascal(c: Int, r: Int): Int = if(c==0 || r==0 || c==r) 1 else pascal(c-1,r-1) + pascal(c,r-1)
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean =
    {
      def changeCount (head:Char) = if (head == '(') 1 else if (head == ')') -1 else 0
      def check(chars: List[Char], count:Int): Boolean =
        if (chars.isEmpty) count == 0 else if (count<0) false else check(chars.tail,count+changeCount(chars.head))
      check(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
    {
      def add(cond:Boolean) = if(cond) 1 else 0
      if(coins.isEmpty) add(money==0) else if(money>0) countChange(money-coins.head,coins) +
        countChange(money,coins.tail) else add(money==0)
    }
  }
