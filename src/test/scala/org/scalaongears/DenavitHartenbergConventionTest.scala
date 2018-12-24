package org.scalaongears

import org.scalaongears.DenavitHartenbergConvention._
import org.scalaongears.MathematicalExpression._
import org.scalaongears.HomogeneousTransformation._
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalaongears.operator.Vector
import org.scalaongears.operator._
import org.scalaongears.operator.Variable
import org.scalaongears.operator.Constant

object AngularRobot {
  def apply(): DenavitHartenberg = {
    val dh1 = DenavitHartenbergTransformation(0.0, 0.0, "alpha_1", 0.0)
    val dh2 = DenavitHartenbergTransformation(-Math.PI / 2.0, 0.0, "alpha_2", 0.0)
    val dh3 = DenavitHartenbergTransformation(0.0, 20.0, "alpha_3", 0.0)
    val dh4 = DenavitHartenbergTransformation(0.0, 20.0, "alpha_4", 0.0)
    val dh5 = DenavitHartenbergTransformation(0.0, 20.0, 0.0, 0.0)
    val angularRobot: DenavitHartenberg = DenavitHartenbergConvention.DenavitHartenberg(
      dh1,
      dh2,
      dh3,
      dh4,
      dh5)
    angularRobot
  }

  def variables(): Vector[Variable] = {
    Vector(List(Variable("alpha_1"), Variable("alpha_2"), Variable("alpha_3"), Variable("alpha_4")))
  }

}

class DenavitHartenbergConventionTest extends FlatSpec with Matchers {

  "The DH of (0.0, 0.0, 'alpha_1', 0.0)" should "be a rotation in Z using 'alpa_1' " in {
    val dh = DenavitHartenbergTransformation(0.0, 0.0, "alpha_1", 0.0)
    dh().evaluate().simplify() should be(RotateInZ("alpha_1")())
  }

  "The DH of (-PI/2.0, 0.0, 'alpha_2', 0.0)" should "be a rotation in X using -PI/2.0 followed by a rotation in Z using 'alpa_2'" in {
    val dh = DenavitHartenbergTransformation(-Math.PI / 2.0, 0.0, "alpha_2", 0.0)
    dh().evaluate().simplify() should be((RotateInZ("alpha_2")() * RotateInX(-Math.PI / 2.0)()).evaluate().simplify())
  }

  "The DH of (0.0, 'distance_1', 'alpha_3', 0.0)" should "be a translation in X of 'distance_1' followed by a rotation in Z using 'alpa_3'" in {
    val dh = DenavitHartenbergTransformation(0.0, "distance_1", "alpha_3", 0.0)
    dh().evaluate().simplify() should be((RotateInZ("alpha_3")() * TranslateInX("distance_1")()).evaluate().simplify())
  }

  "The DH of (0.0, 'distance_3', 0.0, 0.0)" should "be a translation in X of 'distance_3'" in {
    val dh = DenavitHartenbergTransformation(0.0, "distance_3", 0.0, 0.0)
    dh().evaluate().simplify() should be((TranslateInX("distance_3")()).evaluate().simplify())
  }

  "The expected end-effector positions of a simple robot " should " be the ones manually computed." in {
    val dh: DenavitHartenberg = DenavitHartenbergConvention.DenavitHartenberg(
      DenavitHartenbergTransformation(0.0, 0.0, "alpha_1", 0.0),
      DenavitHartenbergTransformation(-Math.PI / 2.0, 0.0, "alpha_2", 0.0),
      DenavitHartenbergTransformation(0.0, 20.0, "alpha_3", 0.0));
    val environment = Map("alpha_1" -> Math.PI / 4.0, "alpha_2" -> 0.0, "alpha_3" -> 0.0) 
    val vectorX = Vector(1.0, 0.0, 0.0, 1.0)
    val vectorY = Vector(0.0, 1.0, 0.0, 1.0)
    val vectorZ = Vector(0.0, 0.0, 1.0, 1.0)
    val expectedXResult = Vector(21 * Math.sin(Math.PI / 4.0), 21 * Math.cos(Math.PI / 4.0), 0.0, 1.0);
    val expectedYResult = Vector(20 * Math.cos(Math.PI / 4.0), 20 * Math.sin(Math.PI / 4.0), -1.0, 1.0);
    val expectedZResult = Vector((20 * Math.sin(Math.PI / 4.0)) - Math.sin(Math.PI / 4.0), (20 * Math.cos(Math.PI / 4.0)) + Math.cos(Math.PI / 4.0), 0.0, 1.0);    
    val error = 0.0000000000001;
    getMeanSquaredError(dh, vectorX, expectedXResult, environment) should be < (error)
    getMeanSquaredError(dh, vectorY, expectedYResult, environment) should be < (error)
    getMeanSquaredError(dh, vectorZ, expectedZResult, environment) should be < (error)
  }
  
  private def getMeanSquaredError(
      dh: DenavitHartenberg, 
      vector :  Vector[MathematicalExpression], 
      expectedFinalPosition : Vector[MathematicalExpression],
      environment: Map[String, Double]) : Double = {
    val errorDiff : Vector[MathematicalExpression] = ((dh() * vector) - expectedFinalPosition).evaluate(environment)
    val errorDiffSum = errorDiff.mathematicalExpressions.flatMap(error => error match {
      case Constant(x) => List(x)
      case _ => throw new IllegalStateException  
    }).foldLeft(0.0)((mse, error) => mse + (error * error))
    errorDiffSum
  }

  "The vector alpha_i=1 and distance_i = 20 on the DH of (0.0, 0.0, 'alpha_1', 0.0),  (-PI/2.0, 0.0, 'alpha_2', 0.0), (0.0, 'distance_1', 'alpha_3', 0.0), (0.0, 'distance_2', 'alpha_4', 0.0), (0.0, 'distance_3', 0.0, 0.0)" should "be (60.0, 0.0, 0.0, 1.0)" in {
    val environment = Map(
      "alpha_1" -> 0.0, "alpha_2" -> 0.0, "alpha_3" -> 0.0, "alpha_4" -> 0.0)
    val vector = Matrix(Vector(0.0, 0.0, 0.0, 1.0)).transpose()
    val expectedResult = Matrix(Vector(60.0, 0.0, 0.0, 1.0)).transpose()
    AngularRobot()().evaluate(environment) * vector should be(expectedResult)
  }

}