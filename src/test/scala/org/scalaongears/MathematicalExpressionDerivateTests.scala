package org.scalaongears

import scala.util.Random
import org.scalaongears.MathematicalExpression._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalaongears.operator._

class MathematicalExpressionDerivateTests extends FlatSpec with Matchers {

  val error = 0.0001

  "The derivate of ( (sin(x), cos(x)) (-sin(x), -cos(x) ) using x" should "be ( (cos(x), -sin(x)) (-cos(x), sin(x) )" in {
    Matrix(
      Vector(Sin("x"), Cos("x")),
      Vector(-Sin("x"), -Cos("x")))
     .derivate("x").simplify().simplify() should be(
      Matrix(
        Vector(Cos("x"), -Sin("x")),
        Vector(-Cos("x"), Sin("x")))      
     )
  }

  "The derivative of (cos(x), sin(x), x, 10)" should "be (-sin(x), cos(x), 1.0, 0)" in {
    Vector(Cos("x"), Sin("x"), "x", 10.0).derivate("x").simplify() should be(
      Vector(-Sin("x"), Cos("x"), 1.0, 0.0))
  }

 "The derivative of (cos(x), sin(x), x, 10)" should "not be (-sin(x), cos(x), 1.0, 1.0)" in {
    Vector(Cos("x"), Sin("x"), "x", 10.0).derivate("x").simplify() should not be(
      Vector(-Sin("x"), Cos("x"), 1.0, 1.0))
  }

  
  "The derivative of sin(x) / cos(x)" should " be ( diff(sin(x))*cos(x) - sin(x)*diff(cos(x)) ) / cos(x)^2" in {
    Divide(Sin("x"), Cos("x")).derivate("x").simplify() should be(
      Divide(
        Substract(
          Multiply(Sin("x").derivate("x").simplify(), Cos("x")),
          Multiply(Sin("x"), Cos("x").derivate("x").simplify())),
        Multiply(
          Cos("x"),
          Cos("x"))))
  }

  "The derivative of sin(x) * cos(x)" should " be diff(sin(x))*cos(x) + sin(x)*diff(cos(x))" in {
    Multiply(Sin("x"), Cos("x")).derivate("x") should be(
      Add(
        Multiply(Sin("x").derivate("x"), Cos("x")),
        Multiply(Sin("x"), Cos("x").derivate("x"))))
  }

  "The derivative of sin(x) - cos(x)" should " be diff(sin(x)) - diff(cos(x))" in {
    Substract(Sin("x"), Cos("x")).derivate("x") should be(
      Substract(Sin("x").derivate("x"), Cos("x").derivate("x")))
  }

  "The derivative of sin(x) + cos(x)" should " be diff(sin(x)) + diff(cos(x))" in {
    Add(Sin("x"), Cos("x")).derivate("x") should be(
      Add(Sin("x").derivate("x"), Cos("x").derivate("x")))
  }

  "Derivative of a constant" should "be zero" in {
    val rand: Double = Random.nextDouble()
    (rand).derivate("x").evaluate() match {
      case cte: Constant => cte.constantValue should be(0.0)
      case _ => fail
    }
  }

  "the derivative of -cos(sin(x))" should "be minus the derivative of cos(sin(x))" in {
    (-Cos(Sin("x"))).derivate("x") should be(-Cos(Sin("x")).derivate("x"))
  }

  "the derivative of -cos(sin(x))" should "not be the derivative of cos(sin(x))" in {
    (-Cos(Sin("x"))).derivate("x") should not be(Cos(Sin("x")).derivate("x"))
  }
  
  "The derivative of cosinus evaluated to half PI" should "be minus one" in {
    Cos("y").derivate("y").evaluate(Map("y" -> Math.PI / 2)) match {
      case cte: Constant => cte.constantValue should (be > -1.0 - error and be < -1.0 + error)
      case _ => fail
    }
  }

 "The derivative of cosinus evaluated to half PI" should "not be one" in {
    Cos("y").derivate("y").evaluate(Map("y" -> Math.PI / 2)) match {
      case cte: Constant => cte.constantValue should not (be < -1.0 - error and be > -1.0 + error)
      case _ => fail
    }
  }


  "The derivative of sinus evautated to zero" should "be one" in {
    Sin("y").derivate("y").evaluate(Map("y" -> 0)) match {
      case cte: Constant => cte.constantValue should (be > 1.0 - error and be < 1.0 + error)
      case _ => fail
    }
  }

}