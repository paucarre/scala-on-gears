package org.scalaongears.operator
import org.scalaongears.MathematicalExpression._


/**
 * Cosine of a Mathematical Expression7
 * 
 *  @author Pau Carré Cardona ( pau.carre@gmail.com )
 *  
 */
case class Cos(mathematicalExpression: MathematicalExpression) extends MathematicalExpression {

  def evaluate(environment: VariableNameEvalutor): MathematicalExpression = {
    val evaluatedInternalValExpr = mathematicalExpression.evaluate(environment)
    evaluatedInternalValExpr match {
      case Constant(x) => Constant(Math.cos(x))
      case _ => Cos(evaluatedInternalValExpr)
    }
  }

  def derivate(variableToDerivate: Variable): MathematicalExpression = {
    Multiply(
      Minus(
        Sin(mathematicalExpression)),
      mathematicalExpression.derivate(variableToDerivate))
  }

  def simplify(): MathematicalExpression = {
    val simplifiedMathematicalExpression: MathematicalExpression = mathematicalExpression.simplify()
    Cos(simplifiedMathematicalExpression)
  }

  override def toString(): String = {
    "cos( " + mathematicalExpression.toString() + " )"
  }

}
