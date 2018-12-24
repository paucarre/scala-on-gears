package org.scalaongears.operator
import org.scalaongears.MathematicalExpression._


/**
 * Multiplication of two MathematicalExpressions
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
case class Multiply(leftMathExpression: MathematicalExpression,
  rightMathExpression: MathematicalExpression) extends MathematicalExpression {

  def evaluate(environment: VariableNameEvalutor): MathematicalExpression = {
    val leftInternalMathExprEvaluated = leftMathExpression.evaluate(environment)
    val rightInternalMathExprEvaluated = rightMathExpression.evaluate(environment)
    (leftInternalMathExprEvaluated, rightInternalMathExprEvaluated) match {
      case (Constant(leftConstant), Constant(rightConstant)) => Constant(leftConstant * rightConstant)
      case _ => Multiply(leftInternalMathExprEvaluated, rightInternalMathExprEvaluated)
    }
  }

  def derivate(variableToDerivate: Variable): MathematicalExpression = {
    Add(
      Multiply(
        leftMathExpression.derivate(variableToDerivate), rightMathExpression), 
      Multiply(
        leftMathExpression, rightMathExpression.derivate(variableToDerivate)))
  }

  def simplify(): MathematicalExpression = {
    val simplifiedLeftMathExpression: MathematicalExpression = leftMathExpression.simplify()
    val simplifiedRightMathExpression: MathematicalExpression = rightMathExpression.simplify()
    if (simplifiedLeftMathExpression == Constant(0.0)
      ||
      simplifiedRightMathExpression == Constant(0.0)) {
      Constant(0.0); ;
    } else if (simplifiedLeftMathExpression == Constant(1.0)) {
      simplifiedRightMathExpression
    } else if (simplifiedRightMathExpression == Constant(1.0)) {
      simplifiedLeftMathExpression
    } else if (simplifiedLeftMathExpression == Constant(-1.0)) {
      -simplifiedRightMathExpression
    } else if (simplifiedRightMathExpression == Constant(-1.0)) {
      -simplifiedLeftMathExpression
    } else {
      (simplifiedLeftMathExpression, simplifiedRightMathExpression) match {
        case (Minus(minusLeftMathExpression), Minus(minusRightMathExpression)) => Multiply(minusLeftMathExpression, minusRightMathExpression)
        case _ => Multiply(simplifiedLeftMathExpression, simplifiedRightMathExpression)
      }
    }

  }

  override def equals(that: Any) = {
    that match {
      case Multiply(thatLeftMathExpression, thatRightMathExpression) => (
        (thatLeftMathExpression == leftMathExpression && thatRightMathExpression == rightMathExpression) || (thatLeftMathExpression == rightMathExpression && thatRightMathExpression == leftMathExpression))
      case _ => false
    }
  }

  override def toString(): String = {
    "( " + leftMathExpression.toString() + " * " + rightMathExpression.toString() + " )";
  }

}
