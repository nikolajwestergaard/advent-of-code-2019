package nikolaj.westergaard

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import AoCCommon._

class Dec09Test extends AnyFlatSpec with Matchers {

  "Part 01" should "calc first instruction" in {
    val out = bq[BigInt]()
    Dec09.process((List(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99) ++: List.fill(1000)(0)).map(_.toLong), bq(), out = out)._2.toArray.toList should be(List(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99))

    out.clear()
    Dec09.process(List(1102, 34915192, 34915192, 7, 4, 7, 99, 0), bq(), out = out)._2.take().toString.length should be(16)

    out.clear()
    Dec09.process(List[BigInt](104, BigInt(1125899906842624L), 99), bq(), out = out)._2.take() should be(1125899906842624L)
  }

}