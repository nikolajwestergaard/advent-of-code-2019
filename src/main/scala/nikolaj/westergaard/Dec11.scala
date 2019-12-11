package nikolaj.westergaard


import AoCCommon._

import scala.collection.mutable
import scala.concurrent.duration.DurationDouble
import scala.concurrent.{Await, Future}

object Dec11 {


  def getVal(ins: List[BigInt], rb: Int, pos: Int, m: Int): BigInt = m match {
    case 0 => ins(ins(pos).toInt)
    case 1 => ins(pos)
    case 2 => ins(rb + ins(pos).toInt)
  }

  @scala.annotation.tailrec
  def process(ins: List[BigInt], ti: BQ[Int], pos: Int = 0, m1: Int = 0, m2: Int = 0, m3: Int = 0,
              out: BQ[Int], rb: Int = 0, oi: Option[BigInt] = None): (List[BigInt], BQ[Int], Boolean) = {
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
        out.add(v1.toInt)
        process(ins, ti, pos + 2, out = out, rb = rb)
      case 5 => process(ins, ti, if (v1 != 0) v2.toInt else pos + 3, out = out, rb = rb)
      case 6 => process(ins, ti, if (v1 == 0) v2.toInt else pos + 3, out = out, rb = rb)
      case 7 => process(ins.updated(p3, if (v1 < v2) 1 else 0), ti, pos + 4, out = out, rb = rb)
      case 8 => process(ins.updated(p3, if (v1 == v2) 1 else 0), ti, pos + 4, out = out, rb = rb)
      case 9 => process(ins, ti, pos + 2, out = out, rb = (rb + v1).toInt)
      case 99 => (ins, out, true)
    }
  }

  def robot(map: mutable.HashMap[(Int, Int), Int], in: BQ[Int], out: BQ[Int], first: Int = 0) = {
    var ld = 0
    var lp = (0, 0)
    while (true) {
      out.put(if (map.isEmpty) first else map.getOrElse(lp, 0))
      val c = in.take()
      val d = in.take()
      map.put(lp, c)
      ld = if (d == 0) ld + 3 else ld + 1
      lp = ld.abs % 4 match {
        case 0 => lp.copy(_2 = lp._2 + 1)
        case 1 => lp.copy(_1 = lp._1 + 1)
        case 2 => lp.copy(_2 = lp._2 - 1)
        case 3 => lp.copy(_1 = lp._1 - 1)
      }
    }
  }
}

object Dec11_01 extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  val inst = AoCCommon.loadBigIntList("dec11.txt", 1000)
  val in = bq[Int]()
  val out = bq[Int]()
  val map: mutable.HashMap[(Int, Int), Int] = mutable.HashMap()
  Future(Dec11.robot(map, in, out))
  Await.result(Future(Dec11.process(inst, ti = out, out = in)), 30.seconds)
  println(map.size)
}

object Dec11_02 extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  val inst = AoCCommon.loadBigIntList("dec11.txt", 1000)
  val in = bq[Int]()
  val out = bq[Int]()
  val map: mutable.HashMap[(Int, Int), Int] = mutable.HashMap()

  Future(Dec11.robot(map, in, out, 1))
  Await.result(Future(Dec11.process(inst, ti = out, out = in)), 30.seconds)
  0.to(100).foreach(i => {
    0.to(100).foreach(ii => {
      print(if (map.getOrElse((i - 50, ii - 50), 0) == 1) "#" else " ")
    })
    println()
  })
}
