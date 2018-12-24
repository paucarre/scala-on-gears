package org.scalaongears.operator

import org.scalaongears.MathematicalExpression._


/**
 * Representation of "-" <code>mathematicalExpression</code> mathematical expression
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
case class Minus(mathematicalExpression: MathematicalExpression) extends MathematicalExpression {
  def evaluate(environment: VariableNameEvalutor): MathematicalExpression = {
    val evaluatedInternalValExpr = mathematicalExpression.evaluate(environment)
    evaluatedInternalValExpr match {
      case Constant(x) => Constant(-x)
      case _ => Minus(evaluatedInternalValExpr)
    }
  }

  def derivate(variableToDerivate: Variable): MathematicalExpression = {
    Minus(mathematicalExpression.derivate(variableToDerivate))
  }

  def simplify(): MathematicalExpression = {
    mathematicalExpression match {
      case Minus(internalMinus) => internalMinus.simplify() // -- == +
      case _ => Minus(mathematicalExpression.simplify())
    }

  }

  override def toString(): String = {
    "-" + mathematicalExpression.toString()
  }

}