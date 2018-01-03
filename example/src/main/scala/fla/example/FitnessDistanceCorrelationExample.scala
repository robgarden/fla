package fla
package example

import scalaz._
import Scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn

import eu.timepit.refined.auto._

import spire.math.Interval
import spire.implicits._

import cilib._
import cilib.benchmarks.Benchmarks
import metrics.FitnessDistanceCorrelation

object FitnessDistanceCorrelationExample extends SafeApp {
  val domain = Interval(-10.0, 10.0)^2
  val points = Position.createPositions(domain, 100)

  val fdc = for {
    ps        <- Step.pointR(points)
    solutions <- ps traverseU Step.evalP[Double]
    metric    <- FitnessDistanceCorrelation(solutions)
  } yield metric

  val env =
    Environment(
      cmp = Comparison dominance Min,
      eval = Eval.unconstrained(Benchmarks.spherical[NonEmptyList, Double]).eval,
      bounds = domain)

  override val runc: IO[Unit] = {
    val result = fdc.run(env) eval RNG.fromTime
    putStrLn(result.toString)
  }
}
