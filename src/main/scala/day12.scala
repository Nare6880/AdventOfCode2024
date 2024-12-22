import utils.InputReader
object day12 {
    def main(arsgs: Array[String]): Unit = {
        val input = InputReader.readInput("day12.txt")
        println(f"Result pt1: ${solvept1(input)}")
    }
    def solvept1(input: List[String]): Int = {
        val antenae = input.reduce((acc, line) => acc + line).distinct.toCharArray().toList.filter(element => element != '.')
        println(antenae)
        val antenaeLocations = antenae.map(char => char -> input.zipWithIndex.flatMap {
            case (string, stringIndex) =>
                string.zipWithIndex.collect {
                    case (c, charIndex) if c == char => (stringIndex, charIndex)
                }
        }).toSet.toMap
        println(antenaeLocations)
        val antenaeLocationsToPerimiters = antenaeLocations.keySet.map(char => char -> getAdjacentcies(antenaeLocations(char))).toMap
        println(antenaeLocationsToPerimiters('S'))
        0
    }
    def getAdjacentcies(input: List[(Int,Int)]): Map[(Int,Int), List[(Int,Int)]] ={
        val adjacentcies = List((1,0),(-1,0),(0,1),(0,-1))
        input.map(cur => cur -> adjacentcies.foldLeft(List())((Acc, element) => {
                if (input.indexOf((cur(0) + element(0), cur(1)+element(1))) != -1) 
                    Acc++List((cur(0) + element(0), cur(1)+element(1)))
                else Acc})
        ).toMap
    }
    def GetRegions(input: Map[(Int,Int), List[(Int,Int)]],toVisit:List[(Int, Int)],  unvisited:List[(Int,Int)]): List[List[(Int,Int)]] = {
        
        
        
        
        
        List(List((0,0)))
    }
}
