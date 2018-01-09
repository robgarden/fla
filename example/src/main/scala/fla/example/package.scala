package fla

import scalaz.NonEmptyList
import scalaz.Scalaz._
import shapeless._

import benchmarks.dimension._
import cilib._

package object example {

  type D2[A] = Dimension[nat._2,A]
  implicit val d2Input: Input[D2] = new Input[D2] {
    def toInput[A](a: NonEmptyList[A]): D2[A] = {
      val sized = (a.index(0) |@| a.index(1)) { case (x, y) => Sized(x, y) }
      sized.getOrElse(sys.error("Invalid input"))
    }
  }

}
