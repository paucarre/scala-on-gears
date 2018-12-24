package org.scalaongears

import org.scalaongears.DenavitHartenbergConvention.DenavitHartenberg
import org.scalaongears.operator._
import org.scalaongears.MathematicalExpression._
import scala.collection.immutable.ListMap
import scala.util.Random

/**
 *
 * Inverse Kinematics sover based on classical
 * Pseudoinverse of the Jacobian matrix.
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
object PseudoinverseInverseKinematics {
  
  val DEFAULT_TARGET_ERRROR = 0.000001;
  val DEFAULT_MAX_ITERATIONS = 100;
  val DEFAULT_UPDATE_RATE = 0.0001;
  
  def  apply(targetPosition : (Double, Double, Double), variables : Vector[Variable], forwardKinematics  : DenavitHartenberg
      ,updateRate : Double =  DEFAULT_UPDATE_RATE
      ,maxIterations: Integer = DEFAULT_MAX_ITERATIONS
      ,targetError: Double = DEFAULT_TARGET_ERRROR
     ): (Option[Map[String, Double]], Option[Vector[Constant]]) = {
    var currentUpdateRate = updateRate; 
    val forwardKinematicsMatrix = forwardKinematics();
    val positionMathematicalExpression = DenavitHartenberg.positionVector(forwardKinematicsMatrix);
    val forwardKinematicsJacobian =  Matrix.jacobian(positionMathematicalExpression, variables).simplify();
    val jacobianPseudoinverse : Matrix[MathematicalExpression] =  getPseudoinverse(forwardKinematicsJacobian);
    var currentVariableValues : ListMap[String, Double]= ListMap(
        variables.mathematicalExpressions map ( mathExpr =>  mathExpr.variableName -> 0.1 ): _*)
    var currentError : Option[Vector[Constant]]=  getError(currentVariableValues, positionMathematicalExpression, targetPosition);
    var currentIteration = 0
    var currentQuadraticError = computeQuadraticError(currentError.get)
    while(currentIteration < maxIterations && currentError.isDefined && currentQuadraticError > targetError ){
      val variableDifference = (jacobianPseudoinverse.evaluate(currentVariableValues) * currentError.get).evaluateToConstant(currentVariableValues);
      if(variableDifference.isDefined){
        val newVariableValues = 
          (currentVariableValues, variableDifference.get.mathematicalExpressions).zipped.
          map((currentValue, diff) => currentValue._1 -> (currentValue._2 + (currentUpdateRate * diff.constantValue)));
        val newError = getError(newVariableValues, positionMathematicalExpression, targetPosition);
        val newQuadraticError = computeQuadraticError(newError.get);
        if(currentQuadraticError >= newQuadraticError){
          // improvement
          currentUpdateRate = currentUpdateRate * 2.0
          currentVariableValues = newVariableValues;
          currentError =  newError
          currentQuadraticError = newQuadraticError
        } else {
          // non-improvement
          if(currentUpdateRate > 1e-5){
            currentUpdateRate = currentUpdateRate / 2.0
          } else {
            currentUpdateRate = new Random().nextDouble();
          }
        }
      } else {
        var currentError = None
      }
      currentIteration = currentIteration + 1;
    }
    if(currentError.isDefined){
      (Some(currentVariableValues), currentError)
    } else {
      (None, currentError)
    }
  }
 
  def getPseudoinverse(matrix : Matrix[MathematicalExpression]) : Matrix[MathematicalExpression] = {
//    println((matrix * matrix.transpose()).simplify())
    matrix.transpose() * (matrix * matrix.transpose()).inverse();    
  }
  
  private def getError(      
      currentVariableValues : Map[String, Double], 
      positionMathematicalExpression: Vector[_],
      targetPosition : (Double, Double, Double)) : Option[Vector[Constant]] = {
      val currentPosition : Option[Vector[Constant]] = positionMathematicalExpression.evaluateToConstant(currentVariableValues);
      if(currentPosition.isDefined){
        val targetPositionList : List[Double] = List(targetPosition._1, targetPosition._2, targetPosition._3);
        val error = (targetPositionList, currentPosition.get.mathematicalExpressions).zipped.map(
            (targetPos, currentPos) => Constant(targetPos - currentPos.constantValue))
        Some(Vector(error))
      } else {
        None
      }
  }
  
  private def computeQuadraticError(
      error : Vector[Constant]) : Double = {
      val quadraticError = Math.sqrt(error.mathematicalExpressions
        .foldLeft(0.0)((quadraticError, currentError) => quadraticError + (currentError.constantValue * currentError.constantValue)) / error.mathematicalExpressions.size);
      quadraticError
  }
    
}