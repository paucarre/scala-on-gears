package org.scalaongears

import org.scalaongears.MathematicalExpression._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalaongears.operator.Sin
import org.scalaongears.operator.Matrix
import org.scalaongears.operator.Cos
import org.scalaongears.operator.Vector
import org.scalaongears.operator.Constant

class MatrixDeterminantTests extends FlatSpec with Matchers {

  "The determinant of ( (sin(x) )  " should ("be ( (sin(x) ) "
    ) in {
      val testingMatix = Matrix(
        Vector(Sin("x"))        
        )
      val expectedDeterminant = Sin("x")
      testingMatix.determinant.get.simplify should be(expectedDeterminant )
    }

    
 "The determinant of ( (sin(x), cos(x) ) (-sin(x), -cos(x) ) " should (
    "be ( (sin(x) * -cos(x)) + (sin(x), cos(x) )"
    ) in {
      val testingMatix = Matrix(
        Vector(Sin("x"), Cos("x")),
        Vector(-Sin("x"), -Cos("x"))
        )
      val expectedDeterminant = (Sin("x") * -Cos("x")) + (Sin("x") * Cos("x"))
      testingMatix.determinant.get.simplify should be(expectedDeterminant)
    }
  
   
 "The determinant of ( 0.0  20.0) (1.0 0.0) " should (
    "be ( -20.0 )"
    ) in {
      val testingMatix = Matrix(
        Vector(0.0, 20.0),
        Vector(1.0, 0.0)
        )
      val expectedDeterminant = -Constant(20.0)
      testingMatix.determinant.get.simplify should be(expectedDeterminant)
    }
 
  "The determinant of [ (0.0, 0.0, 20.0), (1.0, 0.0, 0.0), (0.0, 1.0, 0.0)]" should ( " be 20") in {
      val testingMatix = Matrix(
        Vector(0.0, 0.0, 20.0),
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0)
        )
  
      val expectedDeterminant = Constant(20.0)
      testingMatix.determinant.get.evaluate() should be(expectedDeterminant)
  }
 
}