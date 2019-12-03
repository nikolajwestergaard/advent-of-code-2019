package nikolaj.westergaard

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Dec02Test extends AnyFlatSpec with Matchers {

  "Part 01" should "calc first instruction" in {
    Computer.process(List(1,9,10,3,2,3,11,0,99,30,40,50)).head should be(3500)
  }

}