package org.scalaongears.operator

import org.scalaongears.MathematicalExpression._

/**
 * Companion object of Matrix of mathematical expressions
 * to create a Matrix from a Seq of Vectors of MathematicalExpressions
 * instead of a List (it adds syntactic sugar)
 *
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 *
 */
object Matrix {

  def apply[M <: MathematicalExpression](rows: Vector[M]*) = new Matrix[M](rows.toList)

  def identity(rowNumber: Int, columnNumber: Int): Matrix[Constant] = {
    val rows: List[Int] = List.range(0, rowNumber)
    val columns: List[Int] = List.range(0, columnNumber)
    Matrix(
      rows.map((currentRow: Int) =>
        Vector(
          columns.map((currentColumn: Int) =>
            if (currentRow == currentColumn) {
              Constant(1.0)
            } else {
              Constant(0.0)
            }))))
  }

  def jacobian[M <: MathematicalExpression](functionVector: Vector[M], variableVector: Vector[Variable]): Matrix[MathematicalExpression] = {
    val rows: List[Int] = List.range(0, functionVector.length)
    val columns: List[Int] = List.range(0, variableVector.length)
    Matrix(
      rows.map((currentRow: Int) =>
        Vector(
          columns.map(
            (currentColumn: Int) =>
              {
                val currentVariable: Variable = variableVector.mathematicalExpressions(currentColumn)
                val currentFunction: MathematicalExpression = functionVector.mathematicalExpressions(currentRow)
                currentFunction.derivate(currentVariable)
              }))))
  }

}

/**
 * Matrix of mathematical expressions
 */
case class Matrix[+M <: MathematicalExpression](rows: List[Vector[M]]) extends MathematicalExpression {

  /**
   * Ensures that all the rows have the same amount of columns
   */
  assert(rows.forall(v => v.length == rows(0).length))

  def rowNumber(): Integer = rows.length

  def columnNumber(): Integer = {
    if (rows.length == 0) {
      0
    } else {
      rows(0).length
    }
  }

  def derivate(variableName: Variable): Matrix[MathematicalExpression] = {
    Matrix(rows.map((e: Vector[_]) => e.derivate(variableName)))
  }

  def evaluate(environment: VariableNameEvalutor): Matrix[MathematicalExpression] = {
    Matrix(rows.map(e => e.evaluate(environment)))
  }

  override def toString(): String = {
    val vectorsAsString = rows.map(v => v.toString()).mkString(", ");
    "matrix (" + vectorsAsString + ")"
  }

  /**
   * @return transposed Matrix
   */
  def transpose(): Matrix[MathematicalExpression] = {
    val columns: List[Int] = List.range(0, this.columnNumber())
    val rows: List[Int] = List.range(0, this.rowNumber())
    Matrix(columns.map((currentColumn: Int) => Vector(rows.map((currentRow: Int) => apply(currentRow)(currentColumn)))))
  }

  /**
   * @param rightMatrix
   * @return Matrix multiplication of "this x rightMatix"
   */
  def *[T <: MathematicalExpression](rightMatrix: Matrix[T]): Matrix[MathematicalExpression] = {
    assert(this.columnNumber == rightMatrix.rowNumber)
    val leftRows: List[Int] = List.range(0, this.rowNumber())
    val rightRows: List[Int] = List.range(0, rightMatrix.rowNumber())
    val rightColumns: List[Int] = List.range(0, rightMatrix.columnNumber())
    Matrix(
      leftRows.map((currentLeftRow: Int) =>
        Vector(
          rightColumns.map((currentRightColumn: Int) =>
            rightRows.foldLeft[MathematicalExpression](Constant(0.0))(
              (computedElement: MathematicalExpression, currentRightRow: Int) =>
                Add(
                  computedElement,
                  Multiply(
                    this(currentLeftRow)(currentRightRow),
                    rightMatrix(currentRightRow)(currentRightColumn)))).simplify()))))
  }

  def *(rightVector: Vector[MathematicalExpression]): Vector[MathematicalExpression] = {
    assert(this.columnNumber == rightVector.length())
    val rows: List[Int] = List.range(0, this.rowNumber())
    val columns: List[Int] = List.range(0, rightVector.length())
    Vector(
      rows.map((currentRow: Int) =>
        columns.foldLeft[MathematicalExpression](Constant(0.0))(
          (computedElement: MathematicalExpression, currentColumn: Int) =>
            Add(
              computedElement,
              Multiply(
                this(currentRow)(currentColumn),
                rightVector(currentColumn))))))
  }

  /**
   * @return norm of the Matrix
   */
  override def norm(): MathematicalExpression = {
    val rows: List[Int] = List.range(0, this.rowNumber())
    val columns: List[Int] = List.range(0, this.columnNumber())
    Sqrt(
      rows.map((currentRow: Int) =>
        columns.map((currentColumn) =>
          this(currentRow)(currentColumn) * this(currentRow)(currentColumn)).foldLeft[MathematicalExpression](Constant(0.0))(
          (computedNorm: MathematicalExpression, currentMathExpr: MathematicalExpression) => (
            computedNorm + currentMathExpr))).foldLeft[MathematicalExpression](Constant(0.0))(
        (computedNorm: MathematicalExpression, currentMathExpr: MathematicalExpression) => (
          computedNorm + currentMathExpr)))
  }

  def -(rightMathExpresion: Matrix[MathematicalExpression]): Matrix[MathematicalExpression] = {
    matrixBinaryOperator((left, right) => left - right, rightMathExpresion)
  }

  def +(rightMathExpresion: Matrix[MathematicalExpression]): Matrix[MathematicalExpression] = {
    matrixBinaryOperator((left, right) => left + right, rightMathExpresion)
  }

  def /(rightMathExpresion: Matrix[MathematicalExpression]): Matrix[MathematicalExpression] = {
    matrixBinaryOperator((left, right) => left / right, rightMathExpresion)
  }

  /**
   * @param mathExprBinaryOperator - function that takes two mathematical expressions and
   * returns one (e.g. + , - , *, / ...)
   * @param rightMathExpresion  - right Matrix
   * @return The Matrix whose elements are the ones resulting from applying the function
   * <code>mathExprBinaryOperator</code> to the elements of this (left side) and the elements
   * of <code>rightMathExpression</code> right side.
   * The <code>rightMathExpression</code> must be a Matrix with the same dimensions as
   * <code>this</code>
   */
  def matrixBinaryOperator(
    mathExprBinaryOperator: (MathematicalExpression, MathematicalExpression) => MathematicalExpression,
    rightMathExpresion: Matrix[MathematicalExpression]): Matrix[MathematicalExpression] = {
    assert(this.rowNumber == rightMathExpresion.rowNumber && this.columnNumber == rightMathExpresion.columnNumber)
    val rows: List[Int] = List.range(0, this.rowNumber())
    val columns: List[Int] = List.range(0, this.columnNumber())
    Matrix(
      rows.map((currentRow: Int) =>
        Vector(columns.map((currentColumn) =>
          mathExprBinaryOperator(this(currentRow)(currentColumn), rightMathExpresion(currentRow)(currentColumn))))))
  }

  /**
   * @return determinant of the Matrix
   */
  def determinant(): Option[MathematicalExpression] = {
    if (rowNumber != columnNumber) {
      Option.empty
    } else if (this.rowNumber == 0) {
      Option(Constant(1.0))
    } else {
      // compute determinant though the first column
      val rowIndexes: List[Int] = List.range(0, this.rowNumber())
      Option(
        rowIndexes.map((currentRow: Int) =>
          // we can guaratee that the submatrix of a square matrix is also square 
          cofactor(currentRow, 0) * this(currentRow)(0)).foldLeft[MathematicalExpression]((Constant(0.0)))(
          (currentDeterminant: MathematicalExpression, currentMathExpr: MathematicalExpression) =>
            currentDeterminant + currentMathExpr))
    }

  }

  /**
   * @param row - row of the element cofactor
   * @param column - column of the element cofactor
   * @return the cofactor of the matrix in the element (row, column)
   */
  def cofactor(row: Integer, column: Integer): MathematicalExpression = {
    cofactorSign(row, column) * minor(row, column).determinant().get
  }

  /**
   * @param row - row of the element cofactor
   * @param column - columsn of the element cofactor
   * @return the sign that should be applied to the
   * matrix element (row, column) when computing
   * its cofactor
   */
  def cofactorSign(row: Integer, column: Integer): Constant = {
    if (column % 2 == 0) {
      if (row % 2 == 0) {
        Constant(1.0)
      } else {
        Constant(-1.0)
      }
    } else {
      if (row % 2 == 0) {
        Constant(-1.0)
      } else {
        Constant(1.0)
      }
    }
  }

  /**
   * @param row - row of the element of the minor
   * @param column -column of the element of the minor
   * @return the minor of the matrix element (row, column)
   */
  def minor(row: Integer, column: Integer): Matrix[MathematicalExpression] = {
    val rows: List[Int] = List.range(0, this.rowNumber())
    val columns: List[Int] = List.range(0, this.columnNumber())
    Matrix(
      rows.filter((currentFilterRow: Int) => currentFilterRow != row).map((currentRow: Int) =>
        Vector(
          columns.filter((currentFilterColumn: Int) => currentFilterColumn != column).map((currentColumn) =>
            this(currentRow)(currentColumn)))))
  }

  /**
   * @return the inverse of the Matrix
   */
  def inverse(): Matrix[MathematicalExpression] = {
    assert(this.rowNumber == this.columnNumber)
    // get the determinant 
    val computedDeterminant = determinant().get;
    // get the matrix of cofactors
    val rows: List[Int] = List.range(0, this.rowNumber())
    val columns: List[Int] = List.range(0, this.columnNumber())
    val matrixOfCofactors = Matrix(
      rows.map((currentRow: Int) =>
        Vector(
          columns.map((currentColumn) =>
            cofactor(currentRow, currentColumn)))))
    // divide every element by the determinant
    matrixOfCofactors.transpose.forEach((currentMathExpr: MathematicalExpression) => currentMathExpr / computedDeterminant)
  }

  /**
   * @param f - function to apply to every element of the Matrix
   * @return the Matrix result of applying <code>f</code> to every element
   * of the matrix
   */
  def forEach(f: (MathematicalExpression) => MathematicalExpression): Matrix[MathematicalExpression] = {
    val rows: List[Int] = List.range(0, this.rowNumber())
    val columns: List[Int] = List.range(0, this.columnNumber())
    Matrix(
      rows.map((currentRow: Int) =>
        Vector(
          columns.map((currentColumn) =>
            f(this(currentRow)(currentColumn))))))
  }

  /**
   * @param row -Matrix row number
   * @param column - Matrix column number
   * @return the element of the matrix in the row <code>row</code>
   * and the column <code>column</code>
   */
  def apply(row: Int)(column: Int): MathematicalExpression = {
    rows(row)(column)
  }

  def simplify(): Matrix[MathematicalExpression] = {
    Matrix(rows.map(e => e.simplify()))
  }

}
