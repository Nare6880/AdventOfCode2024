import utils.InputReader
implicit class BooleanOps (val b: Boolean) extends AnyVal {
    def toInt: Int = if (b) 1 else 0
}
object day08 {
    def main(arsgs: Array[String]): Unit = {
        val input = InputReader.readInput("day08.txt")
        println(f"Result pt1: ${solvept1(input)}")
        println(f"Result pt2: ${solvept2(input)}")
    }
    def solvept1(input: List[String]): Int = {
        val antenae = input.reduce((acc, line) => acc + line).distinct.toCharArray().toList.filter(element => element != '.')
        print(antenae)
        val antenaeLocations = antenae.map(char => char -> input.zipWithIndex.flatMap {
            case (string, stringIndex) =>
                string.zipWithIndex.collect {
                    case (c, charIndex) if c == char => (stringIndex, charIndex)
                }
        }).toSet.toMap
        // println(antenaeLocations)
        val antenaecombinations = antenae.map((key) => 
            antenaeLocations(key).combinations(2).toList)
        // antenaecombinations.foreach(element => println(element.mkString(", ")))
        antenaecombinations.map(list => list.map(combination => {
            val a = combination(0)
            val b = combination(1)
            val aToB = (b(0) - a(0), b(1) - a(1))
            val bToA = (a(0) - b(0), a(1) - b(1))
            List((a(0) + bToA(0), a(1)+bToA(1)), (b(0) + aToB(0), b(1) +aToB(1)))
        })).flatten.flatten.distinct.filter(element => element(0) >= 0 && element(0) < input.size && element(1) >= 0 && element(1) < input.size).size
    }
    def solvept2(input: List[String]): Int = {
        val antenae = input.reduce((acc, line) => acc + line).distinct.toCharArray().toList.filter(element => element != '.')
        print(antenae)
        val antenaeLocations = antenae.map(char => char -> input.zipWithIndex.flatMap {
            case (string, stringIndex) =>
                string.zipWithIndex.collect {
                    case (c, charIndex) if c == char => (stringIndex, charIndex)
                }
        }).toSet.toMap
        // println(antenaeLocations)
        val antenaecombinations = antenae.map((key) => 
            antenaeLocations(key).combinations(2).toList)
        // antenaecombinations.foreach(element => println(element.mkString(", ")))
        antenaecombinations.map(list => list.map(combination => {
            val a = combination(0)
            val b = combination(1)
            val aToB = (b(0) - a(0), b(1) - a(1))
            val bToA = (a(0) - b(0), a(1) - b(1))
            (for {
                iterations <- 0 to input.size
            } yield {
                List((a(0) + bToA(0) * iterations, a(1)+bToA(1) * iterations), (b(0) + aToB(0) * iterations, b(1) +aToB(1) * iterations))
            }).toList
        })).flatten.flatten.flatten.distinct
        .filter(element => element(0) >= 0 && element(0) < input.size && element(1) >= 0 && element(1) < input.size).size
    }
}
