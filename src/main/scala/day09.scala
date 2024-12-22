import utils.InputReader
import scala.compiletime.ops.int
import scala.compiletime.ops.long
object day09 {
    def main(arsgs: Array[String]): Unit = {
        val input = InputReader.readInput("day09.txt")
        // println(f"Result pt1: ${solvept1(input(0))}")
        println(f"Result pt2: ${solvept2(input(0))}")
    }
    def solvept1(input: String):Long = {
        val blocks = input.toCharArray().map(_.asDigit)
        println(blocks.mkString(", "))
        val yeet =(for {
            index <- 0 to blocks.size-1
        } yield {
            if (index % 2 == 0)
                // print((index/2).toString() * blocks(index))
                // println(blocks(index))
                List.fill(blocks(index))(index/2)
            else
                List.fill(blocks(index))(-1)
        }).toList.reduce(_++_)
        println(yeet)
        helper(yeet, 0, yeet.size-1).zipWithIndex.foldLeft(0.toLong)((acc, element) => if (element(0) != -1) acc + element(0) * element(1) else acc)  
        // 0
    }
    def helper(in: List[Int], head:Int, tail: Int): List[Int] = {
        println(tail - head)
        if (head >= tail) 
            in
        else{
            if (in(head) == -1){
                if (in(tail) == -1){
                    helper(in, head, tail - 1)
                }
                else{
                    helper(in.slice(0,head) ++ List(in(tail)) ++ in.slice(head + 1, tail) ++ in.slice(tail+1, in.size) ++ List(-1), head+1, tail-1)
                }
            }
            else{
                if (in(tail) == -1){
                    helper(in, head +1, tail -1 )
                }
                else{
                    helper(in,head+1,tail)
                }
            }
        }
    }
    def solvept2(input: String):Long = {
        val blocks = input.toCharArray().map(_.asDigit)
        println(blocks.mkString(", "))
        val yeet =(for {
            index <- 0 to blocks.size-1
        } yield {
            if (index % 2 == 0)
                // print((index/2).toString() * blocks(index))
                // println(blocks(index))
                ((index/2),blocks(index))
            else
                (-1,blocks(index))
        }).toList
        println(yeet)
        val yeet2 = helper2(yeet, yeet.size-1)
        yeet2.map(elem => List.fill(elem(1))(elem(0))).flatten.zipWithIndex.foldLeft(0.toLong)((acc, element) => if (element(0) != -1) acc + element(0) * element(1) else acc)  
    }
    def helper2(input: List[(Int,Int)], tail:Int): List[(Int,Int)]= {
        println(tail)
        if ( tail == 0 ) input
        else if (input(tail)(0) == -1){
            helper2(input, tail-1)
        }
        else {
            helper2(helper3(input, 0, tail), tail-1)
        }
    }
    def helper3(input: List[(Int,Int)], head:Int, tail:Int): List[(Int,Int)] = {
        if (head >=tail)
            input
        else{
            if (input(head)(0) == -1 && input(head)(1) > input(tail)(1)){
                input.slice(0,head) ++ List(input(tail)) ++ List((-1,input(head)(1) - input(tail)(1))) ++ input.slice(head + 1, tail) ++ List((-1, input(tail)(1))) ++ input.slice(tail+1, input.size)
            }
            else if (input(head)(0) == -1 && input(head)(1) == input(tail)(1)){
                input.slice(0,head) ++ List(input(tail)) ++ input.slice(head + 1, tail) ++ List((-1, input(tail)(1))) ++ input.slice(tail+1, input.size)
            }
            else{
                helper3(input, head+1, tail)
            }
        }
    }
}
