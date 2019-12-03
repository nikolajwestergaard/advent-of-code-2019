package nikolaj.westergaard

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Dec03Test extends AnyFlatSpec with Matchers {

  "Part 01" should "calc shortest Manhattan Distance " in {
    Dec03.pathIntersections("R75,D30,R83,U83,L12,D49,R71,U7,L72".split(","),
      "U62,R66,U55,R34,D71,R55,D58,R83".split(","))
      .map(p => p._2.head._1.abs + p._2.head._2.abs).min should be(159)

    Dec03.pathIntersections("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(","),
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(","))
      .map(p => p._2.head._1.abs + p._2.head._2.abs).min should be(135)
  }

  "Part 02" should "calc shortest circuit path" in {
    Dec03.pathIntersections("R75,D30,R83,U83,L12,D49,R71,U7,L72".split(","),
      "U62,R66,U55,R34,D71,R55,D58,R83".split(","))
      .map(p => p._2.map(_._3).sum).min should be(610)

    Dec03.pathIntersections("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(","),
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(","))
      .map(p => p._2.map(_._3).sum).min should be(410)
  }

}