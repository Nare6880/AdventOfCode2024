import utils.InputReader
object Day01 {
  def main(arsgs: Array[String]): Unit = {
    val input = InputReader.readInput("day01.txt")
    val result1 = solvept1(input)
    println(s"Result pt1: $result1")
    val result2 = solvept2(input)
    println(s"Result pt2: $result2")
  }
  def solvept1(input: List[String]): Int = {
    input.map(_.split("   ").map(_.toInt)).transpose.map(_.sorted).transpose.map(_.reduce(_-_).abs).sum
  }
  def solvept2(input: List[String]): Int = {
    input.map(_.split("   ")
        .map(_.toInt))
        .transpose
        .map(
            _.groupBy(identity)
            .mapValues(_.size))
            .reduce {
                (m1, m2) => 
                    m1.keySet.intersect(m2.keySet).map(key => (key, m2(key))).toMap.view
            }.toMap.map {
                case(key, value) => key * value
            }.reduce(_+_)
  }
}
