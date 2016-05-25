package exercises.chapter3.datastructures

/**
  * Created by zanza on 04/04/2016.
  */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0 // empty list sum is zero
    case Cons(x, xs) =>
      x + sum(xs) //sum of first plus sum of the rest of the list
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], n: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(n, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(l, n - 1)
      }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  //3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  //inferred types
  val xs: List[Int] = List(1, 2, 3, 4, 5, 6)
  val ex1 = dropWhile(xs, (x: Int) => x < 4)
  //ex1 = List(4, 5, 6)

  def dropWhileInfer[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhileInfer(t)(f)
      case _ => as
    }
  val xsi: List[Int] = List(1, 2, 3, 4, 5, 6)
  val exi1 = dropWhileInfer(xs)(x => x < 4)
  //exi1 = List(4, 5, 6)
}
