import utils.InputReader
object day05 {
    def main(arsgs: Array[String]): Unit = {
        val input = InputReader.readInput("day05.txt")
        println(f"Result pt1: ${solvept1(input)}")
        println(f"Result pt2: ${solvept2(input)}")
    }
    def solvept1(Input: List[String]): Int = {
        val rules = Input.take(Input.indexOf("")).map(rule => rule.split(" ").map(num=>num.toInt))
        val books = Input.drop(Input.indexOf("") + 1).map(book => book.split(",").map(num => num.toInt))
        val adherence = books.map(book => (for {
            ruleNum <- 0 to rules.size - 1
        } yield {
            val indexOfBefore = book.indexOf(rules(ruleNum)(0))
            val indexOfAfter = book.indexOf(rules(ruleNum)(1))
            indexOfBefore == -1 || indexOfAfter == -1 || indexOfBefore < indexOfAfter
        }).reduce((acc, curr) => acc && curr))
        // println(adherence.mkString("\n"))
        books.zip(adherence).filter(tupple => tupple(1)).map{case (arr, bool ) => arr }.foldLeft(0)((acc, list) => (list((list.length/2.0).toInt) + acc))
        
        
    }
    def solvept2(Input: List[String]): Int = {
        val rules = Input.take(Input.indexOf("")).map(rule => rule.split(" ").map(num=>num.toInt))
        val books = Input.drop(Input.indexOf("") + 1).map(book => book.split(",").map(num => num.toInt))
        val adherence = books.map(book => (for {
            ruleNum <- 0 to rules.size - 1
        } yield {
            val indexOfBefore = book.indexOf(rules(ruleNum)(0))
            val indexOfAfter = book.indexOf(rules(ruleNum)(1))
            indexOfBefore == -1 || indexOfAfter == -1 || indexOfBefore < indexOfAfter
        }))
        val failures = (for {
            bookNum <- 0 to adherence.size-1
        } yield {
            if (adherence(bookNum).contains(false))
                Some(books(bookNum))
            else
                None
        }).toList.flatten//.filter(element => element != None)//.toArray.asInstanceOf[Array[Array[Int]]]
        // failures.foreach(element => println(element.mkString(", ")))
        val ruleToBooks = failures.map(book => (book, rules.filter(rule => book.indexOf(rule(0)) != -1 && book.indexOf(rule(1)) != -1)))
        println(ruleToBooks(0)(0).mkString(", "))
        ruleToBooks(0)(1).foreach(element => println(element.mkString(", ")))
        ruleToBooks.foldLeft(0)((acc, element) => acc + pt2Helper(element))
    }
    def pt2Helper(input: (Array[Int], List[Array[Int]])): Int = {
        val perms = input(0).permutations.toList
        val adherence = perms.map(book => (for {
            ruleNum <- 0 to input(1).size - 1
        } yield {
            val indexOfBefore = book.indexOf(input(1)(ruleNum)(0))
            val indexOfAfter = book.indexOf(input(1)(ruleNum)(1))
            indexOfBefore == -1 || indexOfAfter == -1 || indexOfBefore < indexOfAfter
        }))
        perms(adherence.indexOf(true))(input(0).size/2)
    }
}
