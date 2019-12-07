package nikolaj.westergaard

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Dec08Test extends AnyFlatSpec with Matchers {

  "Part 01" should "create graph" in {
    Dec08.getLayers("123456789012".toList, 3, 2).length should be(2)
  }

  "Part 02" should "run in loops" in {

  }


}