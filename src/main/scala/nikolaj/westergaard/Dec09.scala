package nikolaj.westergaard

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

object Dec09 {

  def getVal(ins: List[BigInt], rb: Int, pos: Int, m: Int): BigInt = m match {
    case 0 => ins(ins(pos).toInt)
    case 1 => ins(pos)
    case 2 => ins(rb + ins(pos).toInt)
  }

  @scala.annotation.tailrec
  def process(ins: List[BigInt], ti: LinkedBlockingQueue[Int], pos: Int = 0, m1: Int = 0, m2: Int = 0, m3: Int = 0,
              out: BlockingQueue[BigInt], rb: Int = 0, oi: Option[BigInt] = None): (List[BigInt], BlockingQueue[BigInt], Boolean) = {
    lazy val v1 = getVal(ins, rb, pos + 1, m1)
    lazy val v2 = getVal(ins, rb, pos + 2, m2)
    lazy val p1 = (if (m1 == 2) ins(pos + 1) + rb else ins(pos + 1)).toInt
    lazy val p3 = (if (m3 == 2) ins(pos + 3) + rb else ins(pos + 3)).toInt

    val instruction = oi.getOrElse(ins(pos))
    instruction.toInt match {
      case p if p > 99 =>
        val s = p.abs.formatted("%05d")
        process(ins, ti, pos, s(2).asDigit, s(1).asDigit, s(0).asDigit, out, rb = rb, oi = Some(s.substring(3).toInt))
      case 1 => process(ins.updated(p3, v1 + v2), ti, pos + 4, out = out, rb = rb)
      case 2 => process(ins.updated(p3, v1 * v2), ti, pos + 4, out = out, rb = rb)
      case 3 =>
        val inst = ti.take()
        process(ins.updated(p1, inst), ti, pos + 2, out = out, rb = rb)
      case 4 =>
        out.add(v1)
        process(ins, ti, pos + 2, out = out, rb = rb)
      case 5 => process(ins, ti, if (v1 != 0) v2.toInt else pos + 3, out = out, rb = rb)
      case 6 => process(ins, ti, if (v1 == 0) v2.toInt else pos + 3, out = out, rb = rb)
      case 7 => process(ins.updated(p3, if (v1 < v2) 1 else 0), ti, pos + 4, out = out, rb = rb)
      case 8 => process(ins.updated(p3, if (v1 == v2) 1 else 0), ti, pos + 4, out = out, rb = rb)
      case 9 => process(ins, ti, pos + 2, out = out, rb = (rb + v1).toInt)
      case 99 => (ins, out, true)
    }
  }
}

object Dec09_01 extends App {
  val out = AoCCommon.bq[BigInt]()
  val input = AoCCommon.loadBigIntList("dec09.txt", 1000)
  Dec09.process(input, AoCCommon.bq(List(1)), out = out)
  println(out.toString)
}

object Dec09_02 extends App {
  val out = AoCCommon.bq[BigInt]()
  val input = AoCCommon.loadBigIntList("dec09.txt", 500)
  Dec09.process(input, AoCCommon.bq(List(2)), out = out)
  println(out.toString)
}