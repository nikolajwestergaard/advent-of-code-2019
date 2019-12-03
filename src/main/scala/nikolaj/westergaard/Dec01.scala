package nikolaj.westergaard

import scala.math.BigDecimal.RoundingMode

object FuelCalculator {
  def calculateFuel(bd: BigDecimal): BigDecimal =
    bd./(3).setScale(0, RoundingMode.FLOOR) - 2

  def calculateTotalFuel(bd: BigDecimal): BigDecimal = {
    val fuel = calculateFuel(bd)
    if (fuel > 0)
      return fuel + calculateTotalFuel(fuel)
    0
  }
}

object Dec01_01 extends App {
  println(AoCCommon.loadBigDecimal("dec01.txt").map(FuelCalculator.calculateFuel).sum)
}

object Dec01_02 extends App {
  println(AoCCommon.loadBigDecimal("dec01.txt").map(FuelCalculator.calculateTotalFuel).sum)
}
