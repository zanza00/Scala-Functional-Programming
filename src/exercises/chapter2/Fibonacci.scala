package exercises.chapter2

/**
  * Created by Zanza on 21/12/2015.
  */

object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFibonacci(x: Int) = {
    val msg = "The Fibonacci value of %d is %d"
    msg.format(x, fib(x))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  /*
  * 0,1,1,2,3,5,8
  * */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def calcFibonacci(firstNumber: Int, secondNumber: Int, n: Int): Int =
      if (n == 0) firstNumber
      else calcFibonacci(secondNumber, firstNumber + secondNumber, n - 1)

    calcFibonacci(0, 1, n)
  }

  private def formatResult(x: Int, what: String, funzione: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(what, x, funzione(x))
  }


  /*
   *
   * due bianchi per uno verde
   * tre verdi per uno blu
   * quattro blu per uno azzurro
   * cinque azzurri per uno viola
   *
   * quanti bianchi per uno viola?
   *
   * 1	2  <--
   * 2  6
   * 3	24
   * 4	120
   * 5	720
   * 6	5040
   * 7	40320
   * 8	362880
   * 9	3628800
   * 10	39916800
   */
  def colorProgression(n: Int): Int = {
    @annotation.tailrec
    def loop(count: Int, current: Int, total: Int): Int =
      if (count == n) total
      else loop(count + 1, current + 1,total * (2 + current))
    loop(1, 1, 2)
  }


  def main(args: Array[String]): Unit = {
    println(formatResult(-42, "Absolute", abs))
    println(formatResult(7, "Fibonnaci", fib))
    println(formatResult(5, "color progression", colorProgression))
  }

}

class Fibonacci {

}



