package nikolaj.westergaard

import scala.util.Try

object Dec07 {
  var out1: List[Int] = List()

  def process(ins: List[Int], testinst: List[Int], pos: Int = 0, m1: Int = 0, m2: Int = 0, out: List[Int] = List()): (List[Int], List[Int]) = {
    lazy val v1 = if (m1 == 0) ins(ins(pos + 1)) else ins(pos + 1)
    lazy val v2 = if (m2 == 0) ins(ins(pos + 2)) else ins(pos + 2)
    ins(pos) match {
      case p if p > 99 =>
        val s = p.abs.formatted("%05d")
        process(ins.updated(pos, s.substring(3).toInt), testinst, pos, s(2).asDigit, s(1).asDigit, out)
      case 1 => process(ins.updated(ins(pos + 3), v1 + v2), testinst, pos + 4, out = out)
      case 2 => process(ins.updated(ins(pos + 3), v1 * v2), testinst, pos + 4, out = out)
      case 3 => process(ins.updated(ins(pos + 1), testinst.head), testinst.drop(1), pos + 2, out = out)
      case 4 =>
        val output = ins(ins(pos + 1))
        out1 = out1 :+ output
        process(ins, testinst, pos + 2, out = out :+ output)
      case 5 => process(ins, testinst, if (v1 != 0) v2 else pos + 3, out = out)
      case 6 => process(ins, testinst, if (v1 == 0) v2 else pos + 3, out = out)
      case 7 => process(ins.updated(ins(pos + 3), if (v1 < v2) 1 else 0), testinst, pos + 4, out = out)
      case 8 => process(ins.updated(ins(pos + 3), if (v1 == v2) 1 else 0), testinst, pos + 4, out = out)
      case 99 => (ins, out)
    }
    (ins, out)
  }

  def runChain(input: List[Int], p1: Int, p2: Int, p3: Int, p4: Int, p5: Int): Try[List[Int]] = {
    Try {
      Dec07.out1 = List()
      val out1 = Dec07.process(input, List(p1, 0))

      var i = p2 +: Dec07.out1
      Dec07.out1 = List()
      val out2 = Dec07.process(input, i)

      i = p3 +: Dec07.out1
      Dec07.out1 = List()
      val out3 = Dec07.process(input, i)

      i = p4 +: Dec07.out1
      Dec07.out1 = List()
      val out4 = Dec07.process(input, i)

      i = p5 +: Dec07.out1
      Dec07.out1 = List()
      Dec07.process(input, i)
      Dec07.out1
    }
  }
}

object Dec07_01 extends App {
  val pars = 0.to(5)
    .flatMap(i => 0.to(4).map(in => (i, in)))
    .flatMap(i => 0.to(4).map(in => (i._1, i._2, in)))
    .flatMap(i => 0.to(4).map(in => (i._1, i._2, i._3, in)))
    .flatMap(i => 0.to(4).map(in => (i._1, i._2, i._3, i._4, in)))
    .filter(p => List(p._1, p._2, p._3, p._4, p._5).distinct.length == 5)

  val input = AoCCommon.loadIntList("dec07.txt")
  val fin = pars.map(p => {
    Dec07.runChain(input, p._1, p._2, p._3, p._4, p._5).toOption
  }).toList.flatten.flatten.max
  println(fin)
}

object Dec07_02 extends App {
  Dec05.process(AoCCommon.loadIntList("dec07.txt"), List(5))

  val pars = 5.to(9)
    .flatMap(i => 5.to(9).map(in => (i, in)))
    .flatMap(i => 5.to(9).map(in => (i._1, i._2, in)))
    .flatMap(i => 5.to(9).map(in => (i._1, i._2, i._3, in)))
    .flatMap(i => 5.to(9).map(in => (i._1, i._2, i._3, i._4, in)))
    .filter(p => List(p._1, p._2, p._3, p._4, p._5).distinct.length == 5)

  val input = AoCCommon.loadIntList("dec07.txt")
  val fin = pars.map(p => {
    Dec07.runChain(input, p._1, p._2, p._3, p._4, p._5).toOption
  }).toList.flatten.flatten.max
  println(fin)
}