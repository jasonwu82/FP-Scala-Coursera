package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(countChange(1,List(1)))
    println(countChange(2,List(1)))
    println(countChange(2,List(2,1)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c==0 || c==r) 1
      else pascal(c-1,r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance_helper(num: Int,remain: List[Char]): Boolean ={
        if (num==0 && remain.isEmpty) true
        else if(num!=0 && remain.isEmpty) false
        else{
          if(remain.head == '(') balance_helper(num + 1,remain.tail)
          else if(remain.head ==')') if(num<=0) false else balance_helper(num-1,remain.tail)
          else balance_helper(num, remain.tail)
        }
      }
      balance_helper(0,chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(coins.isEmpty) 0
      else if(money<0) 0
      else if(money==0) 1
      else countChange(money-coins.head,coins) + countChange(money,coins.tail)
    }
  }
