package fla
package example

import scalaz._
import Scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn

import eu.timepit.refined.auto._

import shapeless._
import shapeless.ops.nat._
import spire.math.{Interval,sqrt}
import spire.implicits._

import cilib._
import benchmarks.cec.cec2013.niching.Benchmarks
import benchmarks.implicits._
import benchmarks.dimension._
import metrics.Dispersion

object DispersionCEC2013Example extends SafeApp {

  val f1: Dimension[nat._1,Double] => Double = Benchmarks.f1[Double] _
  val f2: Dimension[nat._1,Double] => Double = Benchmarks.f2[Double] _
  val f3: Dimension[nat._1,Double] => Double = Benchmarks.f3[Double] _
  val f4: Dimension[nat._2,Double] => Double = Benchmarks.f4[Double] _
  val f5: Dimension[nat._2,Double] => Double = Benchmarks.f5[Double] _
  val f6: Dimension[nat._2,Double] => Double = Benchmarks.f6[nat._2,Double] _
  val f7: Dimension[nat._2,Double] => Double = Benchmarks.f7[nat._2,Double] _
  val f8: Dimension[nat._2,Double] => Double = Benchmarks.f8[nat._2,Double] _
  val f9: Dimension[nat._2,Double] => Double = Benchmarks.f9[nat._2,Double] _
  val f10: Dimension[nat._2,Double] => Double = Benchmarks.f10[nat._2,Double] _
  val f11: Dimension[nat._2,Double] => Double = Benchmarks.f11[nat._2,Double] _
  val f12: Dimension[nat._3,Double] => Double = Benchmarks.f12[nat._3,Double] _

  type FunctionWithInfo[N<:Nat] = (String, Dimension[N,Double] => Double, NonEmptyList[Interval[Double]])

  val functions1 = List(
    ("f1", f1, Interval(0.0, 30.0)^1),
    ("f2", f2, Interval(0.0, 1.0)^1),
    ("f3", f3, Interval(0.0, 1.0)^1)
  )

  val functions2 = List(
    ("f4", f4, Interval(-6.0, 6.0)^2),
    ("f5", f5, NonEmptyList(Interval(-1.9, 1.9), Interval(-1.1, 1.1))),
    ("f6", f6, Interval(-10.0, 10.0)^2),
    ("f7", f7, Interval(0.25, 10.0)^2),
    ("f8", f8, Interval(0.0, 1.0)^2),
    ("f9", f9, Interval(-5.0, 5.0)^2),
    ("f10", f10, Interval(-5.0, 5.0)^2),
    ("f11", f11, Interval(-5.0, 5.0)^2)
  )

  val functions3 = List(
    ("f12", f12, Interval(-5.0, 5.0)^3)
  )

  def runDispersion(name: String, env: Environment[Double]) = {
    val domain = env.bounds
    val points = Position.createPositions(domain, 100)

    val dispersion = for {
      ps        <- Step.pointR(points)
      solutions <- ps traverseU Step.evalP[Double]
      metric    <- Dispersion(.1)(solutions)
    } yield metric

    val samples = 100
    val rng = RNG init 1
    val repeats: String \/ List[Double] = dispersion
      .run(env)
      .replicateM(samples)
      .eval(rng)
      .sequenceU

    repeats.map { values =>
      val avg = values.sum / samples
      val dev = sqrt(values.mapSum(vi => (vi - avg) ** 2) / samples)
      (name, avg, dev)
    }
  }

  def getResults[N<:Nat:ToInt](fs: List[FunctionWithInfo[N]]) = {
    fs
      .map { case (name, f, domain) =>
        (name, Environment(
          cmp = Comparison dominance cilib.Min,
          eval = f.unconstrained.eval,
          bounds = domain
        ))
      }
      .traverseU { env => runDispersion(env._1,env._2) }
  }

  override val runc: IO[Unit] = {
    val results = getResults(functions1) +++ getResults(functions2) +++ getResults(functions3)
    val resultsString = results.toOption.get.map(_.toString).mkString("\n")
    putStrLn(resultsString)
  }
}
