// Author: Daniel Vera Nieto
import java.io._

import scala.collection.mutable.ListBuffer


object EjerciciosScala {
   /**
   * write a `List[String]` to the `filename` (overrides).
   */
  def writeFile(filename: String, lines: ListBuffer[String]): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- lines) {
      bw.write(line)
      bw.write('\n')
    }
    bw.close()
  }



  /**
   * Return true if object o is the first element of list l. Else, false.
   * @param o
   * @param l
   */
  def firstp(o: Any, l : List[Any]) : Boolean =
    if (l.isEmpty)  false
    else if (l.head == o) true
    else false

  def duplicar(l : List[Any]): List[Any] = {
    if (l.isEmpty) l
    else List(l.head, l.head) :: duplicar(l.tail)
  }

  def countdown(N : Int) : List[Int] = {
    if (N > 0) N :: countdown(N - 1)
    else List()
  }

  def reverse(list : List[Any]) : List[Any] = {
    if (list.isEmpty) list
    else reverse(list.tail)::List(list.head)
  }

  def substitute(x : Any, y : Any, ls : List[Any]) : List[Any] = {
    if (!ls.contains(y)) ls
    else if (y == ls.head) x :: substitute( x, y, ls.tail)
    else ls.head :: substitute(x,y,ls.tail)
  }

  def setequal(l1 : List[Any], l2 : List[Any]) : Boolean = {
    l1.forall(x => l2.contains(x)) && l2.forall(x => l1.contains(x))
  }

  def impares(ls : List[Any]) : List[Any] = {
   if (ls.isEmpty || ls.tail.isEmpty) ls
   else ls.head :: impares(ls.splitAt(2)._2)
  }

  def main(args: Array[String]): Unit = {
    var results = new ListBuffer[String]()


    results+= "Author: Daniel Vera Nieto"


    results += ("\n")
    results += ("============= firstp tests =================== */")
    results += ("\n")
    results += ("firstp 'a' List('a','b','c'): " + firstp ('a', List('a', 'b', 'c')).toString)
    results += ("firstp(\"martes\", List(\"lunes\", \"martes\", \"miercoles\")): " + firstp("martes", List("lunes", "martes", "miercoles")).toString)
    results += ("firstp(1, List()): " + (firstp(1, List())).toString)
    results += ("firstp(1, List(2,3)): " + firstp(1, List(2,3)).toString)
    results += ("firstp(1, List(1,2,3)): " + firstp(1, List(1,2,3)).toString)

    results += ("\n")
    results += ("============= duplicar tests =================== */")
    results += ("\n")
    results += ("duplicar(List(1,2,3)): " + duplicar(List(1,2,3)).toString)
    results += ("duplicar(List('a'): " + duplicar(List('a')))
    results += ("duplicar(List('a', 'b', 'c'): " + duplicar(List('a', 'b', 'c')))
    results += ("duplicar(List()): " + duplicar(List()))

    results += ("\n")
    results += ("============= countdown tests =================== */")
    results += ("\n")
    results += ("countdown(10): " + countdown(10).toString)
    results += ("countdown(-1): " + countdown(-1).toString)

    results += ("\n")
    results += ("============= reverse tests =================== */")
    results += ("\n")
    results += ("reverse(List(1,2,3)): " + reverse(List(1,2,3)).toString)
    results += ("reverse(List()): " + reverse(List()).toString)

    results += ("\n")
    results += ("============= substitute tests =================== */")
    results += ("\n")
    results += ("substitute('a', 'b', List('a', 'b', 'c')): " +
      substitute('a', 'b', List('a', 'b', 'c')).toString)
    results += ("substitute('a', 'd', List('a', 'b', 'c')): " +
      substitute('a', 'd', List('a', 'b', 'c')).toString)
    results += ("substitute(0, 5, List(1,2,3,4,5,6,7,8,9)): " +
      substitute(0, 5, List(1,2,3,4,5,6,7,8,9)).toString)
    results += ("substitute(0, 5, List(1,2,3,4,5,6,7,8,9)): " +
      substitute(0, 5, List()).toString)
    results += ("substitute(0, 5, List(1,2,3,4,5,6,7,8,9)): " +
      substitute(0, 5, List(6,7,8,9)).toString)


    results += ("\n")
    results += ("============= setequal tests =================== */")
    results += ("\n")
    results += ("setequal(List(1,1,2), List(1,2)): " + setequal(List(1,1,2), List(1,2)).toString)
    results += ("setequal(List(1,1,2), List(1,2,3)): " + setequal(List(1,1,2), List(1,2,3)).toString)
    results += ("setequal(List(1,2,3), List(1,2)): " + setequal(List(1,2,3), List(1,2)).toString)
    results += ("setequal(List('a', 1), List(1,'a')): " + setequal(List('a',1), List(1,'a')).toString)
    results += ("setequal(List(3,4), List(3,3,4,4,4,4)): " + setequal(List(3,4), List(3,3,4,4,4,4)).toString)
    results += ("setequal(List(3,3,3,3,4), List(3,3,4,4,4,4)): " + setequal(List(3,3,3,3,4), List(3,3,4,4,4,4)).toString)
    results += ("setequal(List(3,4), List(3,3,4,4,4,4)): " + setequal(List(3,3,3,3,4,4,4,4), List(3,3,4,4,4,4)).toString)
    results += ("setequal(List(), List(3,3,4,4,4,4)): " + setequal(List(), List(3,3,4,4,4,4)).toString)
    results += ("setequal(List(), List()): " + setequal(List(), List()).toString)
    results += ("setequal(List(3,4), List(3,3,4,4,4,4)): " + setequal(List(3,3,3,4,5), List(1,2,3,4,5)).toString)

    results += ("\n")
    results += ("============= setequal tests =================== */")
    results += ("\n")
    results += ("impares(List(1)): " + impares(List(1)).toString())
    results += ("impares(List(1,2,3,4,5,6)): " + impares(List(1,2,3,4,5,6)).toString())
    results += ("impares(List(1,2)): " + impares(List(1,2)).toString())
    results += ("impares(List()): " + impares(List()).toString())


    writeFile("resultado.txt", results)
    println("Fichero escrito")

  }
}
