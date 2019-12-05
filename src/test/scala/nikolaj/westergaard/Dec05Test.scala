package nikolaj.westergaard

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Dec05Test extends AnyFlatSpec with Matchers {

  "Part 01" should "calc first instruction" in {
    Dec05.process(List(3,0,4,0,99), List(1)).head should be(1)
    Dec05.process(List(1001,4,3,4,96), List(1))(4) should be(99)
    Dec05.process(List(1002,4,3,4,33), List(1))(4) should be(99)
    Dec05.process(List(104,1,99), List(1))(2) should be(99)
    //Dec05.process(List(104,4,99), List(1))(2) should be(99)
  }

}