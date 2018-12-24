package org.scalaongears.operator
import org.scalaongears.MathematicalExpression._

/**
 * Addition of two MathematicalExpression[Any]
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
case class Add(leftMathExpression: MathematicalExpression,
  rightMathExpression: MathematicalExpression) extends MathematicalExpression {

  def evaluate(environment: VariableNameEvalutor): MathematicalExpression = {
    val leftInternalMathExprEvaluated = leftMathExpression.evaluate(environment)
    val rightInternalMathExprEvaluated = rightMathExpression.evaluate(environment)
    (leftInternalMathExprEvaluated, rightInternalMathExprEvaluated) match {
      case (Constant(leftConstant), Constant(rightConstant)) => Constant(leftConstant + rightConstant)
      case _ => Add(leftInternalMathExprEvaluated, rightInternalMathExprEvaluated)
    }
  }

  def derivate(variableToDerivate: Variable): MathematicalExpression = {
    Add(
      leftMathExpression.derivate(variableToDerivate), rightMathExpression.derivate(variableToDerivate))
  }

  def simplify(): MathematicalExpression = {
    val simplifiedLeftMathExpression: MathematicalExpression = leftMathExpression.simplify()
    val simplifiedRightMathExpression: MathematicalExpression = rightMathExpression.simplify()
    if (simplifiedLeftMathExpression == Constant(0.0)
      &&
      simplifiedRightMathExpression == Constant(0.0)) {
      Constant(0.0); ;
    } else if (simplifiedLeftMathExpression == Constant(0.0)) {
      simplifiedRightMathExpression
    } else if (simplifiedRightMathExpression == Constant(0.0)) {
      simplifiedLeftMathExpression
    } else {
      Add(simplifiedLeftMathExpression, simplifiedRightMathExpression)
    }

  }

  override def equals(that: Any) = {
    that match {
      case Add(thatLeftMathematicalExpression, thatRightMathematicalExpression) => (
        thatLeftMathematicalExpression == leftMathExpression && thatRightMathematicalExpression == rightMathExpression
        ||
        thatRightMathematicalExpression == leftMathExpression && thatLeftMathematicalExpression == rightMathExpression)
      case _ => false
    }
  }

  override def toString(): String = {
    "( " + leftMathExpression.toString() + " + " + rightMathExpression.toString() + " )";
  }
}
