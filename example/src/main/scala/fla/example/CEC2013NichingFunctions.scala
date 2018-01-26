package fla
package example

import scalaz._
import shapeless._
import spire.implicits._
import spire.math.Interval

import benchmarks.cec.cec2013.niching.Benchmarks
import benchmarks.dimension._
import benchmarks.implicits._
import cilib._

object CEC2013NichingFunctions {

  val f1: Dimension[nat._1,Double] => Double = Benchmarks.f1[Double] _
  val f2: Dimension[nat._1,Double] => Double = Benchmarks.f2[Double] _
  val f3: Dimension[nat._1,Double] => Double = Benchmarks.f3[Double] _
  val f4: Dimension[nat._2,Double] => Double = Benchmarks.f4[Double] _
  val f5: Dimension[nat._2,Double] => Double = Benchmarks.f5[Double] _
  val f6: Dimension[nat._2,Double] => Double = Benchmarks.f6[nat._2,Double] _
  val f6_3: Dimension[nat._3,Double] => Double = Benchmarks.f6[nat._3,Double] _
  val f7: Dimension[nat._2,Double] => Double = Benchmarks.f7[nat._2,Double] _
  val f7_3: Dimension[nat._3,Double] => Double = Benchmarks.f7[nat._3,Double] _
  val f8: Dimension[nat._2,Double] => Double = Benchmarks.f8[nat._2,Double] _
  val f9: Dimension[nat._2,Double] => Double = Benchmarks.f9[nat._2,Double] _
  val f10: Dimension[nat._2,Double] => Double = Benchmarks.f10[nat._2,Double] _
  val f11_2: Dimension[nat._2,Double] => Double = Benchmarks.f11[nat._2,Double] _
  val f11_3: Dimension[nat._3,Double] => Double = Benchmarks.f11[nat._3,Double] _
  val f11_5: Dimension[nat._5,Double] => Double = Benchmarks.f11[nat._5,Double] _
  val f11_10: Dimension[nat._10,Double] => Double = Benchmarks.f11[nat._10,Double] _
  val f12: Dimension[nat._3,Double] => Double = Benchmarks.f12[nat._3,Double] _
  val f12_5: Dimension[nat._5,Double] => Double = Benchmarks.f12[nat._5,Double] _
  val f12_10: Dimension[nat._10,Double] => Double = Benchmarks.f12[nat._10,Double] _
  val f12_20: Dimension[nat._20,Double] => Double = Benchmarks.f12[nat._20,Double] _

  val functions1 = List(
    ("f1-1", f1, Interval(0.0, 30.0)^1),
    ("f2-1", f2, Interval(0.0, 1.0)^1),
    ("f3-1", f3, Interval(0.0, 1.0)^1)
  )

  val functions2 = List(
    ("f4-2", f4, Interval(-6.0, 6.0)^2),
    ("f5-2", f5, NonEmptyList(Interval(-1.9, 1.9), Interval(-1.1, 1.1))),
    ("f6-2", f6, Interval(-10.0, 10.0)^2),
    ("f7-2", f7, Interval(0.25, 10.0)^2),
    ("f8-2", f8, Interval(0.0, 1.0)^2),
    ("f9-2", f9, Interval(-5.0, 5.0)^2),
    ("f10-2", f10, Interval(-5.0, 5.0)^2),
    ("f11-2", f11_2, Interval(-5.0, 5.0)^2)
  )

  val functions3 = List(
    ("f6-3", f6_3, Interval(-10.0, 10.0)^3),
    ("f7-3", f7_3, Interval(0.25, 10.0)^3),
    ("f11-3", f11_3, Interval(-5.0, 5.0)^3),
    ("f12-3", f12, Interval(-5.0, 5.0)^3)
  )

  val functions5 = List(
    ("f11-5", f11_5, Interval(-5.0, 5.0)^5),
    ("f12-5", f12_5, Interval(-5.0, 5.0)^5)
  )

  val functions10 = List(
    ("f11-10", f11_10, Interval(-5.0, 5.0)^10),
    ("f12-10", f12_10, Interval(-5.0, 5.0)^10)
  )

  val functions20 = List(
    ("f12-20", f12_20, Interval(-5.0, 5.0)^20)
  )

}
