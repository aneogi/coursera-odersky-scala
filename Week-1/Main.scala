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
  def pascal ( c: Int, r: Int ): Int ={
    if (c == 0) return 1
    else if (c == r) return 1
    else {
      return pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def charCount ( chars: List[ Char ], char: Char, count: Int ): Int ={
      if (chars.isEmpty) return 0
      if (chars.head == char) return count + 1 + charCount(chars.tail, char, count)
      else return count + charCount(chars.tail, char, count)
    }

    val openCount = charCount(chars, '(', 0)

    val closeCount = charCount(chars, char = ')', 0)

    if (openCount == closeCount) return true
    else return false

  }

  /**
    * Exercise 3
    */
  /*
    def countChange(money: Int, coins: List[Int]): Int = {
      var count = 0
      def getChange ( money: Int, coins: List[ Int ]): Int ={
        if (coins.isEmpty) return count
        if (money == 0) return count
        val coin = coins.head
        print("money: " + money + ", " + " coin: " + coin)
        if (money%coin != 0) {
          println(" count: " + count)
          getChange (money%coin, coins.tail)
        } else{
          count = count + 1
          println(" count: " + count)
          getChange ((money - coin), coins.tail)
        }
        return getChange (money, coins.tail)
      }

      var valid_coins =  decreaseSortFromLimit(decreaseSort(coins), money)
      return getChange(money, valid_coins)

    }
    */

  def countChange(money: Int, coins: List[Int]): Int = {
    var count = 0
    var remMoney = 0

    def getChange (money1: Int, coin: Int, num: Int, coins: List[Int]): Int ={

     // if (money1 == money && coins.isEmpty) return count

      remMoney = money1 - (num*coin)

      if (remMoney == 0) count = count+1

      if (remMoney == 0 && coins.nonEmpty) {

          println("First ...", " money1: " + money1, " coin: " + coin, "num: " + num, " count: " + count, " coins: ")
          coins.foreach(println)

          for (idx <- num-1 to 0 ){
            getChange(money1, coin, idx, coins)
          }

          return getChange(money1, coins.head, money/coins.head, coins.tail)

      } else if (remMoney != 0 && coins.nonEmpty) {
        println("Second ..."," money1: "+money1," coin: "+coin, "num: "+num," count: "+count, " coins: ")
        coins.foreach(println)
        return getChange(remMoney, coins.head, remMoney/coins.head, coins.tail)
      } else {
        println("Third ..."," money1: "+money1," coin: "+coin, "num: "+num," count: "+count, " coins: ")
        coins.foreach(println)
        return count
      }

    }
    var valid_coins =  decreaseSortFromLimit(decreaseSort(coins), money)
    return getChange(money, valid_coins.head, money/valid_coins.head ,valid_coins.tail)
  }

  def decreaseSort ( coins: List[ Int ] ): List[ Int ] ={

    var sortedCoins = coins.toArray

    def sort ( outIndex: Int ) ={
      for (inIndex <- (outIndex + 1) until sortedCoins.length) {
        if (sortedCoins(outIndex) < sortedCoins(inIndex)) {
          var temp = sortedCoins(outIndex)
          sortedCoins(outIndex) = sortedCoins(inIndex)
          sortedCoins(inIndex) = temp
        }
      }
    }

    for (outIndex <- 0 until sortedCoins.length) sort(outIndex)
    return sortedCoins.toList
  }

  def decreaseSortFromLimit (coins: List[Int], money: Int): List[Int] = {
    return coins.filter(_ <= money)
  }

}
