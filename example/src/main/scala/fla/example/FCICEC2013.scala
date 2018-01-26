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
import metrics.FitnessCloudIndex
import CEC2013NichingFunctions._

object FitnessCloudIndexCEC2013Example extends SafeApp {

  type FunctionWithInfo[N<:Nat] = (String, Dimension[N,Double] => Double, NonEmptyList[Interval[Double]])

  def runFCI(name: String, env: Environment[Double]) = {
    val domain = env.bounds
    val points = Position.createPositions(domain, 500)

    val fci = for {
      ps  <- Step.pointR(points)
      cog <- FitnessCloudIndex cognitive ps
      soc <- FitnessCloudIndex social ps
      // dev <- FitnessCloudIndex.meanOfStdDev(30)(ps)
    // } yield (cog |@| soc |@| dev) { (_, _, _) }
    } yield (cog |@| soc) { (_, _) }

    val samples = 1
    val rng = RNG init 1
    val repeats: String \/ List[(Double,Double)] = fci
      .run(env)
      .replicateM(samples)
      .eval(rng)
      .sequenceU

    repeats.map { values =>
      val cogAvg = values.map(_._1).sum / samples
      val cogDev = sqrt(values.map(_._1).mapSum(vi => (vi - cogAvg) ** 2) / samples)
      val socAvg = values.map(_._2).sum / samples
      val socDev = sqrt(values.map(_._2).mapSum(vi => (vi - socAvg) ** 2) / samples)
      (name, cogAvg, cogDev, socAvg, socDev)
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
      .traverseU { env => runFCI(env._1,env._2) }
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
    val pw = new PrintWriter(new File("/Users/robertgarden/Desktop/fci.csv"))

    results match {
      case -\/(v) => pw.println(v)
      case \/-(rs) => rs.foreach { ri =>
        pw.println(s"${ri._1},${ri._2},${ri._3},${ri._4},${ri._5}")
      }
    }

    pw.close

    putStrLn("Done")
  }
}
