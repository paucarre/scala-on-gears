package org.scalaongears.operator
import org.scalaongears.MathematicalExpression._


/**
 * Division of two MathematicalExpressions
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
case class Divide(leftMathExpression: MathematicalExpression,
  rightMathExpression: MathematicalExpression) extends MathematicalExpression {

  def evaluate(environment: VariableNameEvalutor): MathematicalExpression = {
    val leftInternalMathExprEvaluated = leftMathExpression.evaluate(environment)
    val rightInternalMathExprEvaluated = rightMathExpression.evaluate(environment)
    (leftInternalMathExprEvaluated, rightInternalMathExprEvaluated) match {
      case (Constant(leftConstant), Constant(rightConstant)) => Constant(leftConstant / rightConstant)
      case _ => Divide(leftInternalMathExprEvaluated, rightInternalMathExprEvaluated)
    }
  }

  def derivate(variableToDerivate: Variable): MathematicalExpression = {
    Divide(
      Substract(
        Multiply(
          leftMathExpression.derivate(variableToDerivate),
          rightMathExpression),
        Multiply(
          leftMathExpression,
          rightMathExpression.derivate(variableToDerivate))), Multiply(
        rightMathExpression,
        rightMathExpression))
  }

  override def toString(): String = {
    "( " + leftMathExpression.toString() + " / " + rightMathExpression.toString() + " )";
  }
  
  def simplify(): MathematicalExpression = {
    val simplifiedLeftMathExpression: MathematicalExpression = leftMathExpression.simplify()
    val simplifiedRightMathExpression: MathematicalExpression = rightMathExpression.simplify()
    if (simplifiedLeftMathExpression == Constant(0.0)
      &&
      simplifiedRightMathExpression != Constant(0.0)) {
      Constant(0.0);
    } else {
      Divide(simplifiedLeftMathExpression, simplifiedRightMathExpression)
    }

  }

}

