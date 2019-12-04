package nikolaj.westergaard

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Dec04Test extends AnyFlatSpec with Matchers {

  "Part 01" should "get passwords by conventions" in {
    val check: String => Boolean = Dec04.check(onlyPairs = false)
    check("111111") should be(true)
    check("223450") should be(false)
    check("123789") should be(false)
    Dec04.getPasswords(156218 to 652527).length should be(1694)
  }

  "Part 02" should "get passwords by conventions, only pairs accepted" in {
    val check: String => Boolean  = Dec04.check(onlyPairs = true)
    check("112233") should be(true)
    check("123444") should be(false)
    check("111122") should be(true)
    check("111222") should be(false)
    check("224422") should be(false)
    check("123355") should be(true)
    Dec04.getPasswords(156218 to 652527, true).length should be(1148)
  }


}