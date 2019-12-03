package nikolaj.westergaard

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Dec01Test extends AnyFlatSpec with Matchers {

  "Part 01" should "calc without fuel for fuel" in {
    FuelCalculator.calculateFuel(12) should be(2)
    FuelCalculator.calculateFuel(14) should be(2)
    FuelCalculator.calculateFuel(1969) should be(654)
    FuelCalculator.calculateFuel(100756) should be(33583)
  }

  "Part 02" should "calc with fuel for fuel" in {
    FuelCalculator.calculateTotalFuel(14) should be(2)
    FuelCalculator.calculateTotalFuel(1969) should be(966)
    FuelCalculator.calculateTotalFuel(100756) should be(50346)
  }
}