package nikolaj.westergaard

import nikolaj.westergaard.AoCCommon._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Dec10Test extends AnyFlatSpec with Matchers {

  val s1 =
    """......#.#.
      |#..#.#....
      |..#######.
      |.#.#.###..
      |.#..#.....
      |..#....#.#
      |#..#....#.
      |.##.#..###
      |##...#..#.
      |.#....####""".stripMargin

  "Part 01" should "find astroids" in {
    val map = Dec10.loadMap(s1)
    map.slice(0, 3) should be(List((6, 0), (8, 0), (0, 1)))
  }

  "Part 01" should "best path" in {
    val map = Dec10.loadMap(s1)
    Dec10.getBestLineOfSight(map) should be(((5,8), 33))
  }

}