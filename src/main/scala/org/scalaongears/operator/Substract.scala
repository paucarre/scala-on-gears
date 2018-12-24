package org.scalaongears.operator
import org.scalaongears.MathematicalExpression._


/**
 * Substraction of two MathematicalExpression
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
case class Substract(leftMathExpression: MathematicalExpression,
  rightMathExpression: MathematicalExpression) extends MathematicalExpression {

  def evaluate(environment: VariableNameEvalutor): MathematicalExpression = {
    val leftInternalMathExprEvaluated = leftMathExpression.evaluate(environment)
    val rightInternalMathExprEvaluated = rightMathExpression.evaluate(environment)
    (leftInternalMathExprEvaluated, rightInternalMathExprEvaluated) match {
      case (Constant(leftConstant), Constant(rightConstant)) => Constant(leftConstant - rightConstant)
      case _ => Substract(leftInternalMathExprEvaluated, rightInternalMathExprEvaluated)
    }
  }

  def derivate(variableToDerivate: Variable): MathematicalExpression = {
    Substract(
      leftMathExpression.derivate(variableToDerivate), rightMathExpression.derivate(variableToDerivate))
  }

  def simplify(): MathematicalExpression = {
    val simplifiedLeftMathExpression: MathematicalExpression = leftMathExpression.simplify()
    val simplifiedRightMathExpression: MathematicalExpression = rightMathExpression.simplify()

    if (simplifiedLeftMathExpression == Constant(0.0)
      &&
      simplifiedRightMathExpression == Constant(0.0)) {
      Constant(0.0)
    } else if (simplifiedLeftMathExpression == Constant(0.0)) {
      Minus(simplifiedRightMathExpression)
    } else if (simplifiedRightMathExpression == Constant(0.0)) {
      simplifiedLeftMathExpression
    } else {
      Substract(simplifiedLeftMathExpression, simplifiedRightMathExpression)
    }

  }

  override def equals(that: Any) = {
    that match {
      case thatMathematicalExpression: Substract => thatMathematicalExpression.leftMathExpression == leftMathExpression && thatMathematicalExpression.rightMathExpression == rightMathExpression
      // A - B == -A + B
      case Add(leftExpression, rightExpression) => {
        leftExpression match {
          case Minus(thatLeftMinus) => thatLeftMinus == leftMathExpression && rightExpression == rightMathExpression
          case _ => false
        }
      }
      case _ => false
    }
  }

  override def toString(): String = {
    leftMathExpression.toString() + " - " + rightMathExpression.toString();
  }
}
