package nikolaj.westergaard

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Dec06Test extends AnyFlatSpec with Matchers {

  "Part 01" should "create graph" in {
    val input = List(("COM", "B"), ("B", "C"), ("C", "D"), ("D", "E"), ("E", "F"), ("B", "G"), ("G", "H"), ("D", "I"), ("E", "J"), ("J", "K"), ("K", "L"))
    val graph = Dec06.createGraph(input)
    Dec06.getRoots(input) should be(List("COM"))
    Dec06.getTotalOrbits(graph, "COM") should be(42)
    //Dec06.getOrbits(graph) should be(7)
  }


  "Part 02" should "give shortest path" in {
    val input = List(("COM", "B"), ("B", "C"), ("C", "D"), ("D", "E"), ("E", "F"), ("B", "G"), ("G", "H"), ("D", "I"), ("E", "J"), ("J", "K"), ("K", "L"), ("K", "YOU"), ("I", "SAN"))
    val graph = Dec06.createGraph(input)
    Dec06.travel("YOU", "SAN", Some("YOU"), graph, input) should contain(4)
  }

}