package org.scalaongears

import org.scalaongears.DenavitHartenbergConvention._
import org.scalaongears.MathematicalExpression._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalaongears.operator.Sin
import org.scalaongears.operator.Matrix
import org.scalaongears.operator.Cos
import org.scalaongears.operator.Vector
import org.scalaongears.operator.Constant
import org.scalatest.PropSpec
import org.scalatest.prop.{ PropertyChecks, Checkers }
import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Prop._
import org.scalacheck._
import scala.collection.mutable.HashMap
import org.scalaongears.HomogeneousTransformation._

class MatrixInverseTests extends PropSpec with PropertyChecks with ShouldMatchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(maxSize = 5)
  val error = 0.0001;

  property("The inverse of ( (sin(x) ) should be ( 1 / (sin(x) ) ") {
    val testingMatix = Matrix(
      Vector(Sin("x")))
    val expectedDeterminant = Matrix(Vector(1.0 / Sin("x")))

    val xGen = for {
      x <- Gen.choose(-100.0, 100.0)
    } yield x

    forAll(xGen) {
      xGenerated =>
        {
          val env = Map("x" -> xGenerated)
          testingMatix.inverse.evaluate(env) should be(expectedDeterminant.evaluate(env))
        }
    }

  }

  property("The inverse of ( (sin(x), cos(x) ) (-sin(x), -cos(x) ) should be ( (-cos(x) / D * sin(x) / D) + ( -cos(x) / D, sin(x) / D ) where D = Sin('x') * -Cos('x')) + (Sin('x') * Cos('x')") {
    val testingMatix = Matrix(
      Vector(Sin("x"), Cos("x")),
      Vector(-Sin("x"), -Cos("x")))
    val expectedDeterminant = (Sin("x") * -Cos("x")) + (Sin("x") * Cos("x"))
    val expectedInverse = Matrix(
      Vector(-Cos("x") / expectedDeterminant, -Cos("x") / expectedDeterminant),
      Vector(Sin("x") / expectedDeterminant, Sin("x") / expectedDeterminant))

    val xGen = for {
      x <- Gen.choose(-100.0, 100.0)
    } yield x

    forAll(xGen) {
      xGenerated =>
        {
          val env = Map("x" -> xGenerated)
          testingMatix.inverse.evaluate(env) should be(expectedInverse.evaluate(env))
        }
    }

  }

  property("Having  A = ( (sin(x), cos(x) ) (sin(x), cos(x) ), norm(A*A^-1 - I) should be close to zero") {
    val testingMatix = Matrix(
      Vector(Cos("x"), -Sin("x")),
      Vector(Sin("x"), Cos("x")))
    val identity = Matrix.identity(testingMatix.rowNumber, testingMatix.columnNumber)
    val computedIndentity = testingMatix.inverse * testingMatix

    val xGen = for {
      x <- Gen.choose(-Math.PI / 2.0, Math.PI / 2.0)
    } yield x

    forAll(xGen) {
      xGenerated =>
        {
          val env = Map("x" -> xGenerated)
          ((testingMatix.inverse * testingMatix) - Matrix.identity(2, 2)).norm().evaluate(env) match {
            case Constant(x) => x should be < (error)
            case _           => fail
          }
        }
    }
  }

  property("Having  A = AngualRobot * transpose(AngularRobot), norm(A*A^-1 - I) should be close to zero") {
    val dh = AngularRobot()
    val testingMatix = dh().transpose() * dh();

    val identity = Matrix.identity(testingMatix.rowNumber, testingMatix.columnNumber)
    val computedIndentity = testingMatix.inverse * testingMatix

    val genVariables = for {
      alpha1: Double <- Gen.choose(0.0, Math.PI / 2.0)
      alpha2: Double <- Gen.choose(0.0, Math.PI / 2.0)
      alpha3: Double <- Gen.choose(0.0, Math.PI / 2.0)
      alpha4: Double <- Gen.choose(0.0, Math.PI / 2.0)
    } yield (alpha1, alpha2, alpha3, alpha4)

    forAll(genVariables) {
      randomVariables: (Double, Double, Double, Double) =>
        {
          val env = Map[String, Double]("alpha_1" -> randomVariables._1, "alpha_2" -> randomVariables._2, "alpha_3" -> randomVariables._3, "alpha_4" -> randomVariables._4)
          val testingMatixEvaluated = testingMatix.evaluate(env);
          val foundError = ((testingMatixEvaluated.inverse * testingMatixEvaluated) - Matrix.identity(4, 4)).norm().evaluate()
          foundError match {
            case Constant(x) => x should be < (error)
            case _           => fail
          }
        }
    }
  }

}

