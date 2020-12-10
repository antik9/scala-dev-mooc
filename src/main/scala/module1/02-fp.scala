package module1

import module1.list.List.Cons

import scala.annotation.tailrec

/**
 *  Реализуем тип Option
 */


object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  sealed trait Option[+A]{
    /**
     *
     * Реализовать метод isEmpty, который будет возвращать true если Option не пуст и false в противном случае
     */
    def isEmpty: Boolean = this match {
      case Option.Some(_) => false
      case Option.None => true
    }

    /**
     *
     * Реализовать метод get, который будет возвращать значение
     */
    def get: A = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("get on empty Option")
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny: Unit = if(!isEmpty) println(this.get)

    /**
     *
     * реализовать метод orElse который будет возвращать другой Option, если данный пустой
     */
    def orElse[B >: A](default: Option[B]): Option[B] = this match {
      case o @ Option.Some(_) => o
      case _                  => default
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[B, C](first: Option[B], second: Option[C]): Option[(B, C)] = {
      if (first.isEmpty || second.isEmpty) Option.None
      else Option.Some((first.get, second.get))
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(p: A => Boolean): Option[A] = {
      if (this.isEmpty || !p(this.get)) Option.None
      else this
    }
  }

  object Option {
    case class Some[A](v: A) extends Option[A]
    case object None extends Option[Nothing]
  }
}

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Long = {
    var _n = 1L
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }

  def !!(n: Int): Long = {
    if(n <= 1) 1
    else n * !!(n - 1)
  }

  def !(n: Int): Long = {
    @tailrec
    def loop(n1: Int, acc: Long): Long = {
      if(n <= 1) acc
      else loop(n1 - 1, n1 * acc)
    }
    loop(n, 1)
  }

}

object list {
  /**
   *
   * Реализовать односвязанный имутабельный список List
   */

  sealed trait List[+A]{
    /**
     *
     * Реализовать метод конс :: который позволит добавлять элемент в голову списка
     */
    def ::[AA >: A](head: AA): List[AA] = Cons(head, this)

    /**
     *
     * Реализовать метод mkString который позволит красиво представить список в виде строки
     */
    def mkString(sep: String): String = {
      import List._

      def loop(l: List[A], acc: StringBuilder): StringBuilder = {
        l match {
          case List.Nil => acc
          case h :: Nil => acc.append(s"$h")
          case h :: t => loop(t, acc.append(s"$h$sep"))
        }
      }
      loop(this.reverse, new StringBuilder()).toString()
    }

    /**
     *
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse(): List[A] = {
      import list.List._

      def loop(l: List[A], acc: List[A]): List[A] =
        l match {
          case List.Nil     => acc
          case head :: tail => loop(tail, head :: acc)
        }
      loop(this, Nil)
    }

    /**
     *
     * Реализовать метод для списка который будет применять некую ф-цию к элементам данного списка
     */
    def map[B](f: A => B): List[B] = {
      import list.List._

      def loop(l: List[A]): List[B] =
        l match {
          case List.Nil     => Nil
          case head :: tail => f(head) :: loop(tail)
        }
      loop(this)
    }
  }

  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */
  def incList(l: List[Int]): List[Int] = l.map(_ + 1)

  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */
  def shoutString(l: List[String]): List[String] = l.map(_ + "!")

  object List{
    case object Nil extends List[Nothing]
    case class ::[A](head: A, tail: List[A]) extends List[A]
    val Cons = ::

    /**
     *
     * Реализовать конструктор, для создания списка n элементов
     */
    def apply[T](arg: T*): List[T] = {
      var l: List[T] = List.Nil
      arg.foreach(el => l = el :: l)
      l
    }
  }
}
