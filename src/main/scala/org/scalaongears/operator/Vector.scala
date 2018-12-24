package org.scalaongears.operator

import org.scalaongears.MathematicalExpression._;

/**
 * Companion object to create vectors from Seq of MathematicalExpressions
 * (it adds syntactic sugar)
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
object Vector {
  def apply(mathematicalExpressions: MathematicalExpression*) = new Vector(mathematicalExpressions.toList)
}

/**
 * Vector of Mathematical Expressions
 */
case class Vector[+M <: MathematicalExpression](mathematicalExpressions: List[M]) extends MathematicalExpression {

  def length(): Integer = mathematicalExpressions.size

  def derivate(variableName: Variable): Vector[MathematicalExpression] = {
    Vector(mathematicalExpressions.map((e: M) => e.derivate(variableName)))
  }
 
  def evaluate(environment: VariableNameEvalutor): Vector[MathematicalExpression] = {
    Vector(mathematicalExpressions.map(e => e.evaluate(environment)))
  }

  def evaluateToConstant(environment: VariableNameEvalutor): Option[Vector[Constant]] = {
    val evalutaion = evaluate(environment);
    val constantVector : Vector[Constant] = Vector(evalutaion.mathematicalExpressions.map(e => e match {
      case Constant(x) => Constant(x)
      case _ => {
        return None
      }
    }))
    Some(constantVector)
  }

  
  /**
   * @param mathExprBinaryOperator - function that takes two mathematical expressions and
   * returns one (e.g. + , - , *, / ...)
   * @param rightMathExpresion  - right Matrix
   * @return The Vector whose elements are the ones resulting from applying the function
   * <code>mathExprBinaryOperator</code> to the elements of this (left side) and the elements
   * of <code>rightMathExpression</code> right side.
   * The <code>rightMathExpression</code> must be a Vector with the same dimensions as
   * <code>this</code>
   */
  def vectorBinaryOperator(
    mathExprBinaryOperator: (MathematicalExpression, MathematicalExpression) => MathematicalExpression,
    rightMathExpresion: Vector[MathematicalExpression]): Vector[MathematicalExpression] = {
    assert(this.rows.length == rightMathExpresion.rows.length)
    val rows: List[Int] = List.range(0, this.rowNumber())
    Vector(
      rows.map((currentRow: Int) =>
          mathExprBinaryOperator(this(currentRow), rightMathExpresion(currentRow))))
  }
  
  def -(rightMathExpresion: Vector[MathematicalExpression]): Vector[MathematicalExpression] = {
    vectorBinaryOperator((left, right) => left - right, rightMathExpresion)
  }

  def +(rightMathExpresion: Vector[MathematicalExpression]): Vector[MathematicalExpression] = {
    vectorBinaryOperator((left, right) => left + right, rightMathExpresion)
  }

  def /(rightMathExpresion: Vector[MathematicalExpression]): Vector[MathematicalExpression] = {
    vectorBinaryOperator((left, right) => left / right, rightMathExpresion)
  }
  
  def *(rightMathExpresion: Vector[MathematicalExpression]):  Vector[MathematicalExpression]  = {
    vectorBinaryOperator((left, right) => left * right, rightMathExpresion)
  }
  
  /**
   * @param column - column number
   * @return return the MathematicalExprssion in the
   * column number <code>column</code>
   */
  def apply(column: Int): M = {
    mathematicalExpressions(column)
  }

  override def toString(): String = {
    mathematicalExpressions match {
      case head :: tail => tail.foldLeft("[" + head)(_ + ", " + _) + "]"
      case Nil => "[]"
    }
  }

  def simplify(): Vector[MathematicalExpression] = {
    Vector(mathematicalExpressions.map(e => e.simplify()))
  }

}
