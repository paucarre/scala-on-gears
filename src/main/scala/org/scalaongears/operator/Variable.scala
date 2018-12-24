package org.scalaongears.operator
import org.scalaongears.MathematicalExpression._;


/**
 * Expression based on a variable (i.e. a symbol)
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
case class Variable(variableName: String) extends MathematicalExpression {

  def derivate(variableNameToDerive: Variable): MathematicalExpression = {
    if (this == variableNameToDerive) {
      Constant(1.0)
    } else {
      Constant(0.0)
    }
  }

  def evaluate(environment: VariableNameEvalutor): MathematicalExpression = {
    val optionalDouble: Option[Double] = environment.get(variableName)
    if (optionalDouble.isDefined) {
      Constant(optionalDouble.get)
    } else {
      this
    }
  }

  override def toString(): String = {
    variableName
  }

  def simplify(): MathematicalExpression = {
    this
  }

}
