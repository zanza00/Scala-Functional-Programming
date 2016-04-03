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
  * Exercise 2.1
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
   * This method accept only an array of string
   */
  def findFirstArStr(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) false
      else if (ordered(as(n + 1), as(n))) true
      else loop(n + 1)

    loop(0)
  }

  //follow the types
  def partial1[A, BO, C](a: A, f: (A, BO) => C): BO => C = {
    b => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
//    f compose g
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
      else loop(count + 1, current + 1, total * (2 + current))
    loop(1, 1, 2)
  }


  def main(args: Array[String]): Unit = {
    //    println(formatResult(-42, "Absolute", abs))
    //    println(formatResult(7, "Fibonnaci", fib))
    //    println(formatResult(5, "color progression", colorProgression))
    //    List(1, 2, 3, 4, 5, 6, 7).foreach { x => println(formatResult(x, "Fibonnaci", fib)) }
    println("-----")
    for (a <- 1 to 10) {
      println(formatResult(a, "Fibonnaci", fib))
    }
    println("-----")
    println(findFirst(Array(7, 9, 13), (x: Int) => x == 9))
  }

}

class Fibonacci {

}



