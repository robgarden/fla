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
import metrics.Gradient
import CEC2013NichingFunctions._
import fla.walks._

object GradientCEC2013Example extends SafeApp {

  type FunctionWithInfo[N<:Nat] = (String, Dimension[N,Double] => Double, NonEmptyList[Interval[Double]])

  def runGradient(name: String, env: Environment[Double]) = {
    val domain = env.bounds
    val stepSize = 1.0
    val points = RandomProgressiveManhattanWalk(domain, 100, stepSize)

    val gradients = for {
      ps        <- Step.pointR(points)
      solutions <- ps traverseU Step.evalP[Double]
      avg       <- Gradient.avg(stepSize)(solutions)
      max       <- Gradient.max(stepSize)(solutions)
      dev       <- Gradient.dev(stepSize)(solutions)
    } yield (avg |@| dev |@| max) { (_, _, _) }

    val samples = 30
    val rng = RNG init 1
    val repeats: String \/ List[(Double,Double,Double)] = gradients
      .run(env)
      .replicateM(samples)
      .eval(rng)
      .sequenceU

    repeats.map { values =>
      val avgAvg = values.map(_._1).sum / samples
      val avgDev = sqrt(values.map(_._1).mapSum(vi => (vi - avgAvg) ** 2) / samples)
      val maxAvg = values.map(_._2).sum / samples
      val maxDev = sqrt(values.map(_._2).mapSum(vi => (vi - maxAvg) ** 2) / samples)
      val devAvg = values.map(_._3).sum / samples
      val devDev = sqrt(values.map(_._3).mapSum(vi => (vi - devAvg) ** 2) / samples)
      (name, avgAvg, avgDev, maxAvg, maxDev, devAvg, devDev)
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
      .traverseU { env => runGradient(env._1,env._2) }
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

    import java.io._
    val pw = new PrintWriter(new File("/Users/robertgarden/Desktop/gradient.csv" ))

    results.foreach { r =>
      r.foreach { ri =>
        pw.println(s"${ri._1},${ri._2},${ri._3},${ri._4},${ri._5},${ri._6},${ri._7}")
      }
    }

    pw.close

    putStrLn("Done")
  }
}
