package org.scalaongears

import org.scalaongears.MathematicalExpression._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalaongears.operator.Sin
import org.scalaongears.operator.Matrix
import org.scalaongears.operator.Cos
import org.scalaongears.operator.Vector

class MatrixTransposeTests extends FlatSpec with Matchers {

  "The transpose of ( (sin(x), cos(x) ) (-sin(x), -cos(x) ) " should (
    "be ( (sin(x), -sin(x) ) (cos(x), -cos(x) )"
    ) in {
      val testingMatix = Matrix(
        Vector(Sin("x"), Cos("x")),
        Vector(-Sin("x"), -Cos("x"))
        )
      val expectedTestingMatrixTransposed = Matrix(
        Vector(Sin("x"), -Sin("x")),
        Vector(Cos("x"), -Cos("x"))
        )        
      testingMatix.transpose should be(expectedTestingMatrixTransposed)
    }

    "The transpose of ( (sin(x), cos(x), 1.0 ) (-sin(x), -cos(x), 6.0 ) " should (
    "be ( (sin(x), -sin(x) ) (cos(x), -cos(x) )"
    ) in {
      val testingMatix = Matrix(
        Vector(Sin("x"), Cos("x"), 1.0),
        Vector(-Sin("x"), -Cos("x"), 6.0)
        )
      val expectedTestingMatrixTransposed = Matrix(
        Vector(Sin("x"), -Sin("x")),
        Vector(Cos("x"), -Cos("x")),
        Vector(1.0, 6.0)
        )        
      testingMatix.transpose should be(expectedTestingMatrixTransposed)
    }

}