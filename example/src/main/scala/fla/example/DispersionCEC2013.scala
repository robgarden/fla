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
import benchmarks.dimension._
import benchmarks.implicits._
import metrics.Dispersion
import CEC2013NichingFunctions._

object DispersionCEC2013Example extends SafeApp {

  type FunctionWithInfo[N<:Nat] = (String, Dimension[N,Double] => Double, NonEmptyList[Interval[Double]])

  def runDispersion(name: String, env: Environment[Double]) = {
    val domain = env.bounds
    val points = Position.createPositions(domain, 100)

    val dispersion = for {
      ps        <- Step.pointR(points)
      solutions <- ps traverseU Step.evalP[Double]
      metric    <- Dispersion(.1)(solutions)
    } yield metric

    val samples = 30
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
    val results = for {
      r1  <- getResults(functions1)
      r2  <- getResults(functions2)
      r3  <- getResults(functions3)
      r5  <- getResults(functions5)
      r10 <- getResults(functions10)
      r20 <- getResults(functions20)
    } yield r1 ++ r2 ++ r3 ++ r5 ++ r10 ++ r20
    val resultsString = results.toOption.get.map(_.toString).mkString("\n")
    putStrLn(resultsString)
  }
}
