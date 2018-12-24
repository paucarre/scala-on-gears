package org.scalaongears.operator
import org.scalaongears.MathematicalExpression._


/**
 * Mathematical expression of a constant
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
case class Constant(constantValue: Double) extends MathematicalExpression  {

  
  def derivate(variableName: Variable): MathematicalExpression = {
    Constant(0.0)
  }

  def evaluate(environment: VariableNameEvalutor): Constant = {
    this
  }

  override def toString(): String = {
    constantValue.toString()
  }

  def constantMap2(rightPart: Constant)(f: (Double, Double) => Double): Constant = {
    Constant(f(this.constantValue, rightPart.constantValue))
  }

  def constantMap(f: (Double) => Double): Constant = {
    Constant(f(this.constantValue))
  }

  def simplify(): MathematicalExpression = {
    this
  }

}

