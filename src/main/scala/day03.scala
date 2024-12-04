import utils.InputReader
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
object  day03 {
  def main(arsgs: Array[String]): Unit = {
    val input = InputReader.readInput("day03.txt")
    println(s"Result pt1: ${solvept1(input, """mul\((\d*),(\d*)\)""".r)}")
    println(s"Result pt2: ${solvept2(input, """do\(\)|don't\(\)|mul\((\d*),(\d*)\)""".r)}")
  }
  def solvept1(input: List[String], regex: Regex): Int ={
    conditionalAdd(regex.findAllMatchIn(input.mkString).toList, true, 0)
    // input.map(regex.findAllMatchIn(_)).map(_.toList.map(matching => (matching.group(1).toInt * matching.group(2).toInt))).map(_.reduce(_+_)).reduce(_+_) <- Stupid but fun original solution
  }
  def solvept2(input: List[String], regex: Regex): Int = {
    conditionalAdd(regex.findAllMatchIn(input.mkString).toList, true, 0)
  }
  def conditionalAdd(input: List[Match], toDo: Boolean, Sum: Int): Int = {
    input match
      case Nil => Sum
      case head :: tail => {
        if (head.toString() == "do()")  conditionalAdd(tail, true, Sum)
        else if (head.toString == "don't()") conditionalAdd(tail, false, Sum)
        else if (toDo) conditionalAdd(tail, toDo, Sum + ("""mul\((\d*),(\d*)\)""".r.findAllMatchIn(head.toString()).toList.foldLeft(1)((acc,matching) => acc * matching.group(1).toInt*matching.group(2).toInt)))
        else conditionalAdd(tail, toDo, Sum)
      }
  }
}
//Things that could have been cool but couldn't figure it out
// regex.findAllMatchIn(input.mkString).toList.sliding(2).map {
//   case List(previous, current) => 
//     if ((previous.toString() == "don't()" || previous.toString() == "do()") && (current.toString() == "don't()" || current.toString() == "do()")) List(current) else List(previous, current)
// }.toList.flatten.foldLeft(List()){ 
//   (result, current)=>  if (result.lastOption.contains(current)) result else result:+current 
// }.do what conditional add does