package fla
package example

import scalaz._
import Scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn

import shapeless._
import spire.math.Interval
import spire.implicits._

import cilib._
import benchmarks.Benchmarks
import metrics.Gradient
import walks.RandomProgressiveManhattanWalk

object GradientExample extends SafeApp {
  val domain = Interval(-10.0, 10.0)^2
  val stepSize = 1.0
  val points = RandomProgressiveManhattanWalk(domain, 100, stepSize)

  val gradients = for {
    ps        <- Step.pointR(points)
    solutions <- ps traverseU Step.evalP[Double]
    avg       <- Gradient.avg(stepSize)(solutions)
    max       <- Gradient.max(stepSize)(solutions)
    dev       <- Gradient.dev(stepSize)(solutions)
  } yield (avg, dev, max)

  val f: D2[Double] => Double = Benchmarks.spherical[nat._2,Double] _

  val env = Environment(
    cmp = Comparison dominance Min,
    eval = Eval.unconstrained(f).eval,
    bounds = domain
  )

  override val runc: IO[Unit] = {
    val result = gradients.run(env) eval RNG.fromTime
    putStrLn(result.toString)
  }
}
