import utils.InputReader
object day04 {
    def main(arsgs: Array[String]): Unit = {
        val input = InputReader.readInput("day04.txt")
        println(f"Result pt1: ${solvept1(input)}")
        println(f"Result pt2: ${solvept2(input)}")
    }
    def solvept1(input :List[String]): Int = {
        val horizontal = input.foldRight(0)((list, acc) =>  acc + (list.sliding(4).count(substring => substring =="XMAS" || substring =="SAMX")))
        val vertical = input.transpose.map(list => list.mkString).foldRight(0)((list, acc) =>  acc +list.sliding(4).count(substring => substring =="XMAS" || substring =="SAMX"))
        val TopRight = input.indices.toList.map { i => input(i).drop(i) ++ "." * (i)}.transpose.map(_.mkString).foldRight(0)((list, acc) =>  acc + list.sliding(4).count(substring => substring =="XMAS" || substring =="SAMX"))
        val TopLeft = input.indices.toList.map { i => input(i).reverse.drop(i) ++ "." * (i)}.transpose.map(_.mkString).foldRight(0)((list, acc) =>  acc +list.sliding(4).count(substring => substring =="XMAS" || substring =="SAMX"))
        val BottomRight = input.indices.toList.map { i => input.reverse(i).drop(i+1) ++ "." * (i+1) }.transpose.map(_.mkString).foldRight(0)((list, acc) =>  acc + list.sliding(4).count(substring => substring =="XMAS" || substring =="SAMX"))
        val BottomLeft = input.indices.toList.map { i => input.reverse(i).reverse.drop(i+1) ++ "." * (i+1) }.transpose.map(_.mkString).foldRight(0)((list, acc) => acc + list.sliding(4).count(substring => substring =="XMAS" || substring =="SAMX"))
        horizontal + vertical + TopLeft + TopRight + BottomLeft + BottomRight
    }
    def solvept2(input: List[String]): Int = {
        (for {
            row <- 0 to input.size - 3
            col <- 0 to input.size - 3
        } yield {
            input.slice(row, row + 3).map(_.slice(col, col + 3))
        }).toList.map(list => List(
            list.indices.toList.map { i => list(i).reverse.drop(i) ++ "." * (i)}.transpose.map(_.mkString)(0), //One diagonal of chunk
            list.indices.toList.map { i => list(i).drop(i) ++ "." * (i)}.transpose.map(_.mkString)(0)) //Other diagonal of chunk
        ).filter(list => List("MAS","SAM").contains(list(0)) && List("MAS","SAM").contains(list(1))).size
    }
}
