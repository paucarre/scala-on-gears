package org.scalaongears

import org.scalaongears.operator._
import org.scalaongears.MathematicalExpression._
import org.scalatest.PropSpec
import org.scalatest.prop.{ PropertyChecks, Checkers }
import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalaongears.operator.Matrix
import org.scalaongears.DenavitHartenbergConvention.DenavitHartenberg
import org.scalaongears.DenavitHartenbergConvention.DenavitHartenbergTransformation
import org.scalaongears.HomogeneousTransformation._
import org.scalatest.prop.Configuration._

class PseudoinverseInverseKinematicsTest extends PropSpec with PropertyChecks with ShouldMatchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 3)

  property("The pseudoinverse has the property that the matrix (I − J+J) performs a projection onto the nullspace of J. Which implies that for all v J(I − J+J)v = 0.") {
    val error = 0.0001;
    val angularRobot = AngularRobot()()
    val position = DenavitHartenberg.positionVector(angularRobot);
    val jacobian = Matrix.jacobian(position, AngularRobot.variables());
    val pseudoinverse = PseudoinverseInverseKinematics.getPseudoinverse(jacobian);

    val genVectors = for {
      x: Double <- Gen.choose(1.0, 10.0)
      y: Double <- Gen.choose(1.0, 10.0)
      z: Double <- Gen.choose(1.0, 10.0)
      w: Double <- Gen.choose(1.0, 10.0)
    } yield (x, y, z, w)

    val genVariables = for {
      alpha1: Double <- Gen.choose(0.0, Math.PI / 4.0)
      alpha2: Double <- Gen.choose(0.0, Math.PI / 4.0)
      alpha3: Double <- Gen.choose(0.0, Math.PI / 4.0)
      alpha4: Double <- Gen.choose(0.0, Math.PI / 4.0)
    } yield (alpha1, alpha2, alpha3, alpha4)

    forAll(genVariables, genVectors) {
      (randVariable: (Double, Double, Double, Double), randVector: (Double, Double, Double, Double)) =>
        {
          val vector = Vector(randVector._1, randVector._2, randVector._3, randVector._4)
          val env = Map[String, Double]("alpha_1" -> randVariable._1, "alpha_2" -> randVariable._2, "alpha_3" -> randVariable._3, "alpha_4" -> randVariable._4)
          val jacobianEvaluated = jacobian.evaluate(env);
          val nullSpace: Vector[MathematicalExpression] = ((jacobianEvaluated * (HomogeneousTransformation.Identity()() - (pseudoinverse.evaluate(env) * jacobianEvaluated))) * vector)
          val foundError = nullSpace.mathematicalExpressions.foldLeft[MathematicalExpression](Constant(0.0))((sum, el) => sum + (el * el)).evaluate()
          foundError match {
            case Constant(x) => x should be < (error)
            case _           => fail
          }
        }
    }
  }

  property("Angular robots should be able to find the angles for a reacheable euclidean coordinate point when it is initialized at that point") {
    val angularRobotMatrix: DenavitHartenbergConvention.DenavitHartenberg = AngularRobot()
    val angularRobotVariables = AngularRobot.variables()
    val targetPosition: Tuple3[Double, Double, Double] = (60.0, 0.0, 0.0);
    val optVariableMapping = PseudoinverseInverseKinematics(targetPosition, angularRobotVariables, angularRobotMatrix)
    optVariableMapping._1.isDefined should be(true)
    // check forward kinematics
    val optEndEfferctorPosition = DenavitHartenbergConvention.DenavitHartenberg.positionVector(angularRobotMatrix()).evaluateToConstant(optVariableMapping._1.get)
    optEndEfferctorPosition.isDefined should be(true)
    val targetPositionVector: Vector[Constant] = Vector(List(targetPosition._1, targetPosition._2, targetPosition._3))
    val mse = (targetPositionVector.mathematicalExpressions, optEndEfferctorPosition.get.mathematicalExpressions).zipped.
      map((target: Constant, endEffector: Constant) => (target.constantValue - endEffector.constantValue)).
      map((error: Double) => error * error).
      foldLeft(0.0)((accError, quadraticError) => accError + quadraticError)
    mse should be < (PseudoinverseInverseKinematics.DEFAULT_TARGET_ERRROR)
  }

  property("Angular robots should be able to find the angles for a reacheable euclidean coordinate point when it is initialized at another point") {
    val angularRobot: DenavitHartenberg = AngularRobot()
    val angularRobotVariables: Vector[Variable] = AngularRobot.variables()
    val targetPosition: Tuple3[Double, Double, Double] = (30.0, 10.0, 10.0);
    val optVariableMapping = PseudoinverseInverseKinematics(targetPosition, angularRobotVariables, angularRobot,
      1.0, 400)
    optVariableMapping._1.isDefined should be(true)
    // check forward kinematics
    val optEndEfferctorPosition = DenavitHartenbergConvention.DenavitHartenberg.positionVector(angularRobot()).evaluateToConstant(optVariableMapping._1.get)
    optEndEfferctorPosition.isDefined should be(true)
    val targetPositionVector: Vector[Constant] = Vector(List(targetPosition._1, targetPosition._2, targetPosition._3))
    val mse = Math.sqrt(((targetPositionVector.mathematicalExpressions, optEndEfferctorPosition.get.mathematicalExpressions).zipped.
      map((target: Constant, endEffector: Constant) => (target.constantValue - endEffector.constantValue)).
      map((error: Double) => error * error).
      foldLeft(0.0)((accError, quadraticError) => accError + quadraticError) / targetPositionVector.mathematicalExpressions.size))
    mse should be < (PseudoinverseInverseKinematics.DEFAULT_TARGET_ERRROR)
  }

}
