
object PascalsTriangle extends App {

  def create(n: Int): Seq[IndexedSeq[Int]] = {
    val triangle =
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

}
