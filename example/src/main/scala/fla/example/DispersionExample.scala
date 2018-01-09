package fla
package example

import scalaz._
import Scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn

import eu.timepit.refined.auto._

import shapeless._
import spire.math.Interval
import spire.implicits._

import cilib._
import benchmarks.Benchmarks
import metrics.Dispersion

object DispersionExample extends SafeApp {
  val domain = Interval(-10.0, 10.0)^2
  val points = Position.createPositions(domain, 100)

  val dispersion = for {
    ps        <- Step.pointR(points)
    solutions <- ps traverseU Step.evalP[Double]
    metric    <- Dispersion(.1)(solutions)
  } yield metric

  val f: D2[Double] => Double = Benchmarks.spherical[nat._2,Double] _

  val env = Environment(
    cmp = Comparison dominance Min,
    eval = Eval.unconstrained(f).eval,
    bounds = domain
  )

  override val runc: IO[Unit] = {
    val result = dispersion.run(env) eval RNG.fromTime
    putStrLn(result.toString)
  }
}
