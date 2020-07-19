package de.sciss.lap

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.{Seq => CSeq}

abstract class AlgoSpec extends AnyFlatSpec with Matchers {
  def algorithm: Algorithm

  def cost(mat: Array[Array[Float]], map: CSeq[Int]): Double =
    map.iterator.zipWithIndex.map { case (ci, ri) =>
      mat(ri)(ci).toDouble
    } .sum

  // cf. https://de.wikipedia.org/wiki/Ungarische_Methode
  "example-1" should "work" in {
    val colLb = Seq("A", "B", "C", "D")
    val rowLb = Seq("E", "K", "P", "Z")
    val prio = Array(
      Array[Float](1, 1, 1, 2),
      Array[Float](3, 2, 4, 1),
      Array[Float](4, 4, 2, 4),
      Array[Float](2, 3, 3, 3)
    )
    val rowM = new Array[Int](4)
    val colM = new Array[Int](4)
    algorithm.solveLAP(prio, rowMap = rowM, colMap = colM)
//    println(rowM.mkString("row: [", ", ", "]"))
//    println(colM.mkString("col: [", ", ", "]"))

    val _prio     = prio
    val brute     = (0 until 4).permutations.minBy(cost(_prio, _))
    val costExp   = cost(_prio, brute)
    val costCalc  = cost(_prio, rowM)

    assert(costCalc === costExp)

    val expected = Map(
      "A" -> "Z", "B" -> "E", "C" -> "P", "D" -> "K"
    )

    rowM.zipWithIndex.foreach { case (ci, ri) =>
      assert(expected(colLb(ci)) == rowLb(ri))
    }

    colM.zipWithIndex.foreach { case (ri, ci) =>
      assert(expected(colLb(ci)) == rowLb(ri))
    }
  }

  "example-2" should "work" in {
    def prio = Array(
      Array[Float](0, 0, 0, 0),
      Array[Float](0, 1, 3, 3),
      Array[Float](0, 5, 5, 9),
      Array[Float](0, 1, 3, 7)
    )
    val rowM = new Array[Int](4)
    val colM = new Array[Int](4)
    algorithm.solveLAP(prio, rowMap = rowM, colMap = colM)
//    println(rowM.mkString("row: [", ", ", "]"))
//    println(colM.mkString("col: [", ", ", "]"))

    val _prio     = prio
    val brute     = (0 until 4).permutations.minBy(cost(_prio, _))
    val costExp   = cost(_prio, brute)
    val costCalc  = cost(_prio, rowM)

    assert(costCalc === costExp)

//    assert((rowM === Array(3, 2, 0, 1)) || (rowM === Array(2, 3, 0, 1)))
  }

  "large random example" should "work" in {
    def mkRandomMatrix(M: Int, N: Int = 100): (Array[Array[Float]], Array[Int]) = {
      def prio = {
        val r = new util.Random(0L)
        Array.fill(M)(Array.fill(M)(r.nextInt(N).toFloat))
      }
      val rowM = new Array[Int](M)
      val colM = new Array[Int](M)
      algorithm.solveLAP(prio, rowMap = rowM, colMap = colM)
      (prio, rowM)
    }

    val LARGE = 10000
    val (_, rowLarge) = mkRandomMatrix(LARGE)  // can't verify this using brute search, but at least check that it returns
    assert(rowLarge.sorted === (0 until LARGE))

    val (m10, row10) = mkRandomMatrix(10)

    val hCost = cost(m10, row10)
    val brute = (0 until 10).permutations.minBy(cost(m10, _))
    val bCost = cost(m10, brute)
    assert(hCost == bCost)
    assert(row10 === brute)
  }
}