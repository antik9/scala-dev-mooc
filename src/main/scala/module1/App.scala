package module1

import module1.list._
import module1.opt._

object App {
  def main(args: Array[String]): Unit = {
    val l = List(1, 4, 5)
    val lPlusOne = incList(l)
    println(lPlusOne.mkString(", "))

    val s = List("you", "can", "scream", "it")
    val sShout = shoutString(s)
    println(sShout.mkString(" "))

    val optSomeWord = Option.Some("word")
    optSomeWord.printIfAny

    val none = Option.None
    none.printIfAny

    println(optSomeWord.filter(_.startsWith("w")))
    println(optSomeWord.filter(_.startsWith("m")))
  }
}
