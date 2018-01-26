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
import metrics.FirstEntropicMeasure
import CEC2013NichingFunctions._
import fla.walks._

object FEMCEC2013Example extends SafeApp {

  type FunctionWithInfo[N<:Nat] = (String, Dimension[N,Double] => Double, NonEmptyList[Interval[Double]])

  def runFEM(name: String, env: Environment[Double]) = {
    val domain = env.bounds
    def stepSize(percent: Double, i: Interval[Double]) = percent * (i.upperValue - i.lowerValue)

    def femFromStepSize(ss: Double) =
      for {
        ps        <- Step pointR RandomProgressiveWalk(domain, 100, stepSize(ss, domain.head))
        solutions <- ps traverseU Step.evalP[Double]
        fem       <- FirstEntropicMeasure(solutions)
      } yield fem

    val femMacro = femFromStepSize(.1)
    val femMicro = femFromStepSize(.01)

    val samples = 30
    val rng = RNG init 1
    val repeatsMacro: String \/ List[Double] =  femMacro
      .run(env)
      .replicateM(samples)
      .eval(rng)
      .sequenceU

    val repeatsMicro: String \/ List[Double] =  femMicro
      .run(env)
      .replicateM(samples)
      .eval(rng)
      .sequenceU

    (repeatsMacro |@| repeatsMicro) { (mac, micro) =>
      val macroAvg = mac.sum / samples
      val macroDev = sqrt(mac.mapSum(vi => (vi - macroAvg) ** 2) / samples)
      val microAvg = micro.sum / samples
      val microDev = sqrt(micro.mapSum(vi => (vi - microAvg) ** 2) / samples)
      (name, macroAvg, macroDev, microAvg, microDev)
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
      .traverseU { env => runFEM(env._1,env._2) }
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
    val pw = new PrintWriter(new File("/Users/robertgarden/Desktop/fem.csv" ))

    results.foreach { r =>
      r.foreach { ri =>
        pw.println(s"${ri._1},${ri._2},${ri._3},${ri._4},${ri._5}")
      }
    }

    pw.close

    putStrLn("Done")
  }
}
