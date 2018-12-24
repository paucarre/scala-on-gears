package org.scalaongears

import org.scalaongears.MathematicalExpression._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalaongears.operator.Sin
import org.scalaongears.operator.Matrix
import org.scalaongears.operator.Cos
import org.scalaongears.operator.Vector
import org.scalatest.PropSpec
import org.scalatest.prop.{ PropertyChecks, Checkers }
import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Prop._
import org.scalacheck._

class MatrixMultiplicationTests extends PropSpec with PropertyChecks with ShouldMatchers {

  property("The quadratic form of ( (sin(x), cos(x)) (-sin(x), -cos(x) ) should be properly computed") {
    val testingMatix = Matrix(
      Vector(Sin("x"), Cos("x")),
      Vector(-Sin("x"), -Cos("x")));
    val quadraticFormOfTestingMatrix = testingMatix * testingMatix;
    // expected value
    val expectedRow1column1 = (Sin("x") * Sin("x")) + (Cos("x") * -Sin("x"))
    val expectedRow2column1 = (-Sin("x") * Sin("x")) + (Cos("x") * Sin("x"))
    val expectedRow1column2 = (Sin("x") * Cos("x")) + (Cos("x") * -Cos("x"))
    val expectedRow2column2 = (-Sin("x") * Cos("x")) + (Cos("x") * Cos("x"))
    val expectedQuadraticFormOfTestingMatrix = Matrix(
      Vector(expectedRow1column1, expectedRow1column2),
      Vector(expectedRow2column1, expectedRow2column2))

    val xGen = for {
      x <- Gen.choose(-100.0, 100.0)
    } yield x

    forAll(xGen) {
      xGenerated => {
        val env = Map("x" -> xGenerated)
        quadraticFormOfTestingMatrix.evaluate(env) should be(expectedQuadraticFormOfTestingMatrix.evaluate(env))
      }
    }

  }

}