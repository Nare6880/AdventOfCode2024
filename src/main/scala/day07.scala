import utils.InputReader
object day07 {
        def main(arsgs: Array[String]): Unit = {
        val input = InputReader.readInput("day07.txt")
        println(f"Result pt1: ${solvept1(input)}")
        println(f"Result pt2: ${solvept2(input)}")
    }
    def solvept1(input: List[String]): Long = {
        input.foldLeft(0.toLong)((acc:Long, element) => acc + helper(element.split(" ")(0).toLong, element.split(" ").drop(1).map(_.toLong).toList, None, false))
    }
    def solvept2(input: List[String]): Long = {
        input.foldLeft(0.toLong)((acc:Long, element) => acc + helper(element.split(" ")(0).toLong, element.split(" ").drop(1).map(_.toLong).toList, None, true))
    }
    def helper(target: Long, terms: List[Long], curr: Option[Long], concatenation: Boolean): Long = {
        terms match
            case head :: tail => {
                curr match {
                    case None => 
                        helper(target, tail, Some(head), concatenation)
                    case Some(value) => {
                        // println(curr.toString())
                        if (value > target) 0
                        if (concatenation){
                            if (helper(target,tail,Some(head*value), concatenation) == target || helper(target,tail,Some(value+head), concatenation) == target || helper(target,tail,Some((value.toString() + head.toString()).toLong), concatenation) == target)
                                target
                            else{
                                0
                            }
                        }
                        else {
                            if (helper(target,tail,Some(head*value), concatenation) == target || helper(target,tail,Some(value+head), concatenation) == target)
                                target
                            else{
                                0
                            }
                        }
                    }
                }
            }
            case Nil => {
                curr match {
                    case None => 0
                    case Some(value) => {
                        // println(value)
                        if (value == target){
                            target
                        }
                        else{
                            0
                        }
                    }
                }
            }
    }
}
