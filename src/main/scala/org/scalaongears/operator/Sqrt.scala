package org.scalaongears.operator
import org.scalaongears.MathematicalExpression._


/**
 * Square root of a MathematicalExpression
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
case class Sqrt(mathematicalExpression: MathematicalExpression) extends MathematicalExpression {

  def evaluate(environment: VariableNameEvalutor): MathematicalExpression = {
    val evaluatedInternalValExpr = mathematicalExpression.evaluate(environment)
    evaluatedInternalValExpr match {
      case Constant(x) => Constant(Math.sqrt(x))
      case _ => Sqrt(evaluatedInternalValExpr)
    }
  }

  def derivate(variableToDerivate: Variable): MathematicalExpression = {
    mathematicalExpression.derivate(variableToDerivate) / (Constant(2.0) * Sqrt(mathematicalExpression))
  }

  def simplify(): MathematicalExpression = {
    val simplifiedMathematicalExpression: MathematicalExpression = mathematicalExpression.simplify()
    Sqrt(simplifiedMathematicalExpression)
  }

  override def toString(): String = {
    "sqrt( " + mathematicalExpression.toString() + " )"
  }

}

