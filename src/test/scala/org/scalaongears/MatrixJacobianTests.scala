package org.scalaongears

import org.scalaongears.operator._
import org.scalatest.PropSpec
import org.scalatest.prop.{ PropertyChecks, Checkers }
import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Prop._
import org.scalacheck._

class MatrixJacobianTests extends PropSpec with PropertyChecks with ShouldMatchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 5)

  property("The jacobian of (Sin(x), Cos(xy) ) over (x, y)  " +
    "should be ( (Cos(x), 0 ), (-Sin(xy)y, -Sin(xy)x) )") {
    
    val functions = Vector(Sin("x"), Cos(Variable("x") * Variable("y")))
    val variables: Vector[Variable] = Vector(List("x", "y"))
    val jabobianResult = Matrix.jacobian(functions, variables).simplify()
    val expectedJacobian = Matrix(
      Vector(Cos("x"), 0.0),
      Vector(
        -Sin(Variable("x") * Variable("y")) * Variable("y"),
        -Sin(Variable("x") * Variable("y")) * Variable("x")))

    val xyGen = for {
      x <- Gen.choose(-100.0, 100.0)
      y <- Gen.choose(-100.0, 100.0)
    } yield (x, y)

    forAll(xyGen) {
      xyGenerated =>
        {
           val env = Map("x" -> xyGenerated._1, "y" -> xyGenerated._2)
          jabobianResult.evaluate(env) should be(expectedJacobian.evaluate(env))
        }
    }

  }

}
