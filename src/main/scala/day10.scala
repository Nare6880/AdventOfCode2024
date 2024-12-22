import utils.InputReader
object day10 {
    def main(arsgs: Array[String]): Unit = {
        val input = InputReader.readInput("day10.txt")
        println(f"Result pt1: ${solvept1(input)}")
    }
    def solvept1(input: List[String]):Int = {
        val antenaeLocations = List('0').map(char => input.zipWithIndex.flatMap {
            case (string, stringIndex) =>
                string.zipWithIndex.collect {
                    case (c, charIndex) if c == char => (stringIndex, charIndex)
                }
        }).flatten
        print(antenaeLocations)
        antenaeLocations.foldLeft(0)((acc, element) => Traverser(input.map(ele => ele.toCharArray().map(char => char.asDigit).toList),element).size + acc)//add distinct for part 1

    }
    def Traverser(input: List[List[Int]],StartingPosition: (Int,Int)): List[(Int,Int)] = {

        
        val nextSteps = checkPositions(input, StartingPosition)
        println(nextSteps)
        if (nextSteps.size == 0){
            if (input(StartingPosition(0))(StartingPosition(1)) == 9) 
                List(StartingPosition)  
            else List()
        }
        else {
            nextSteps.foldLeft(List())((acc,element) => Traverser(input, element) ++ acc)
        }
    }
    def checkPositions(input: List[List[Int]], pos: (Int,Int)): List[(Int,Int)] = {
        val adjacents = List((pos(0)-1,pos(1)),(pos(0)+1,pos(1)), (pos(0), pos(1)+1), (pos(0),pos(1)-1))
        adjacents.filter(adj => adj(0) >= 0 && adj(0) < input.size && adj(1) >= 0 && adj(1) < input.size && input(pos(0))(pos(1)) + 1 == input(adj(0))(adj(1)))
    }
}
