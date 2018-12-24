package org.scalaongears.operator
import org.scalaongears.MathematicalExpression._


/**
 * Sine of a MathematicalExpression
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
case class Sin(mathematicalExpression: MathematicalExpression) extends MathematicalExpression {

  def evaluate(environment: VariableNameEvalutor): MathematicalExpression = {
    val evaluatedInternalValExpr = mathematicalExpression.evaluate(environment)
    evaluatedInternalValExpr match {
      case Constant(x) => Constant(Math.sin(x))
      case _ => Sin(evaluatedInternalValExpr)
    }
  }

  def derivate(variableToDerivate: Variable): MathematicalExpression = {
    Multiply(
      Cos(mathematicalExpression),
      mathematicalExpression.derivate(variableToDerivate))
  }

  def simplify(): MathematicalExpression = {
    val simplifiedMathematicalExpression: MathematicalExpression = mathematicalExpression.simplify()
    Sin(simplifiedMathematicalExpression)
  }

  override def toString(): String = {
    "sin( " + mathematicalExpression.toString() + " )"
  }

}
