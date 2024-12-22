import utils.InputReader
import scala.compiletime.ops.boolean
object day06 {
      def main(arsgs: Array[String]): Unit = {
        val input = InputReader.readInput("day06.txt")
        println(f"Result pt1: ${solvept1(input)}")
        // println(f"Result pt2: ${solvept2(input)}")
    }
    def solvept1(input: List[String]): Int = {
        val row = input.indexWhere(string => string.indexOf("^") != -1 || string.indexOf("<") != -1 || string.indexOf(">") != -1 || string.indexOf("^") != -1)
        val col = input(row).indexWhere(char => char == '^' || char == '<' || char == '>' || char == 'V')
        input(row)(col) match 
            case '^' => Traverser(input.transpose.map(_.reverse), (col,input.size-row),false)
            case '>' => Traverser(input.map(_.toList), (row, col),false)
            case '<' => Traverser(input.map(_.reverse.toList), (row, input.size-col),false)
            case 'V' => Traverser(input.transpose , (row,col),false)
        // println(input.transpose.mkString("\n"))
        // print("\n")
        // println(input.transpose.map(_.reverse).mkString("\n"))
        // print((row,col))

    }
    def Traverser(input: List[List[Char]], pos: (Int,Int), clockwise: Boolean): Int = {
        println(pos)
        input.foreach(element => println(element.mkString(" ")))

        val indexOf = input(pos(0)).slice(pos(1), input.size).indexOf('#')
        println(input(pos(0)).slice(pos(1), input.size))
        // print(indexOf)
        if (indexOf == -1)
            return input.size - pos(1)
        else if (!clockwise)
            return indexOf + Traverser(input.transpose, (pos(1) + indexOf - 1,pos(1)+1), true)
        else (return indexOf + Traverser(input.transpose.map(_.reverse), (pos(1) + indexOf - 1, input.size- pos(0)), false))
    }
}
