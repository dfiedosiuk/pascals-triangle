import org.scalatest._
import flatspec._
import matchers._
import PascalsTriangle._


class PascalsTriangleSpec extends AnyFlatSpec with should.Matchers {

  behavior of "A PascalsTriangle.create "

  it should "return Vector of Vectors consisting of values which create Pascal Triangle " in {
    create(3) shouldBe Vector(Vector(1), Vector(1, 1), Vector(1, 2, 1))
    create(5) shouldBe Vector(Vector(1), Vector(1, 1), Vector(1, 2, 1), Vector(1, 3, 3, 1), Vector(1, 4, 6, 4, 1))
  }

}