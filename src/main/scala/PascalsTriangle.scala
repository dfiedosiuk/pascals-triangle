
object PascalsTriangle extends App {

  def create(n: Int): Seq[IndexedSeq[Int]] = {
    val triangle = {
      for {
        row <- 0 until n
      } yield {
        val line = for {
          order <- 0 to row
        } yield {
          countValue(row, order)
        }
        line
      }
    }
    triangle
  }

  def countValue(row: Int, order: Int): Int = {
    var value = 1
    for (i <- 0 until order) {
      value *= (row - i)
      value /= (i + 1)
    }
    value
  }

  def printTriangle(triangle: Seq[IndexedSeq[Int]]) = {
    val maxLength = triangle.last.max.toString.length
    triangle.foldLeft(triangle) { (triangle, row) =>
      val rowToPrint = row
        .map {
          case c if (c.toString.length < maxLength) =>
            " " * (maxLength - c.toString.length) + c.toString
          case c => c.toString
        }
      val rowCentered = " " * (triangle.last.length - rowToPrint.length) +: rowToPrint
      rowCentered.mkString(" ").foreach(print)
      println()
      triangle
    }
  }

}
