package org.scalaongears

import scala.collection.mutable.WrappedArray
import com.sun.org.apache.xalan.internal.xsltc.compiler.ForEach
import org.scalaongears.operator.Divide
import org.scalaongears.operator.Sqrt
import org.scalaongears.operator.Multiply
import org.scalaongears.operator.Substract
import org.scalaongears.operator.Minus
import org.scalaongears.operator.Matrix
import org.scalaongears.operator.Vector
import org.scalaongears.operator.Constant
import org.scalaongears.operator.Variable
import org.scalaongears.operator.Add

/**
 * Symbolic mathematical expression
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 *
 */
object MathematicalExpression {

  /**
   * Map from the variable name to the constant
   * Double value that should be use to
   * evaluate a mathematical expression
   */
  type VariableNameEvalutor = Map[String, Double]

  /**
   * @param constantValue - the value of the Constant
   * @return A Constant with value <code>constantValue</code>
   */
  implicit def doubleToConstant(constantValue: Double) = Constant(constantValue)
  /**
   * @param variableName - name of the Variable
   * @return A Variable with name <code>variableName</code>
   */
  implicit def stringToVariable(variableName: String) = Variable(variableName)
  /**
   * @param vector - Vector to transform into Matrix
   * @return A Matrix with <code>vector</code> as its only row
   */
  implicit def vectorToMatrix(vector: Vector[MathematicalExpression]) = Matrix(vector)

  /**
   * Trait for the case classes to generate mathematical expressions
   */
  trait MathematicalExpression {

    /**
     * variableName - name of the variable to derivate
     *
     * @return derivation of the current MathematicalExpresison with
     * the variable  {@code variableName}
     */
    def derivate(variableName: Variable): MathematicalExpression

    /**
     * environment - {@code VariableNameEvaluator} to resolve variables
     *
     * @return the MathematicalExpression evaluated with the variables
     * present in {@code environment}
     */
    def evaluate(environment: VariableNameEvalutor = Map()): MathematicalExpression

    /**
     * @return simplified expression (if possible)
     */
    def simplify(): MathematicalExpression

    /**
     * @param rightMathExpresion - right mathematical expression
     * @return the addition of two MathematicalExpressions
     */
    def +(rightMathExpresion: MathematicalExpression): MathematicalExpression = {
      Add(this, rightMathExpresion)
    }

    /**
     * @param rightMathExpresion - right mathematical expression
     * @return the substraction of two MathematicalExpressions
     */
    def -(rightMathExpresion: MathematicalExpression): MathematicalExpression = {
      Substract(this, rightMathExpresion)
    }

    /**
     * @param rightMathExpresion - right mathematical expression
     * @return the negation of a MathematicalExpressions
     */
    def unary_- : MathematicalExpression = {
      Minus(this)
    }

    /**
     * @param rightMathExpresion - right mathematical expression
     * @return the multiplication of two MathematicalExpressions
     */
    def *(rightMathExpresion: MathematicalExpression): MathematicalExpression = {
      Multiply(this, rightMathExpresion)
    }

    /**
     * @param rightMathExpresion - right mathematical expression
     * @return the division of two MathematicalExpressions
     */
    def /(rightMathExpresion: MathematicalExpression): MathematicalExpression = {
      Divide(this, rightMathExpresion)
    }

    /**
     * @return the 2-norm of the mathematical expression
     */
    def norm(): MathematicalExpression = {
      Sqrt(this * this)
    }
  }

}