import utils.InputReader
import scala.compiletime.ops.boolean
object day02 {
  def main(arsgs: Array[String]): Unit = {
    val input = InputReader.readInput("day02.txt")
    val result1 = solvept1(input)
    println(s"Result pt1: $result1")
    val result2 = solvept2(input)
    println(s"Result pt2: $result2")
  }
  def solvept1(input: List[String]): Int = {
    input.map(_.split(" ").map(_.toInt))
        .filter(list => list.size == list.distinct.size)
        .map(_.sliding(2).
            map(_.reduce(_-_)).toList)
        .map(list => List(list.max, list.min))
        .map(list => list.reduce(_*_) >= 1 && list.map(_.abs).max <=3)
        .foldLeft(0)((acc, curr) => (if (curr) acc + 1 else acc ))
  }
  def solvept2(input: List[String]):Int ={
    input.map(_.split(" ").map(_.toInt)).map(getPermeutations(_)).map(_.map(_.sliding(2).
            map(_.reduce(_-_)).toList)).map(_.map(list => List(list.max, list.min)).map(list => list.reduce(_*_) >= 1 && list.map(_.abs).max <=3)
        .foldLeft(false)((acc, curr) => (acc|curr))).foldLeft(0)((acc, curr) => (if (curr) acc + 1 else acc ))
  }
  def getPermeutations(input: Array[Int]): List[List[Int]] = {
    if (input.isEmpty) Nil
    else input.indices.toList.map { i => (input.take(i) ++ input.drop(i +1)).toList} :+ input.toList
  }
}