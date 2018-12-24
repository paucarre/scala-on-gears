package org.scalaongears

import org.scalaongears.HomogeneousTransformation._
import org.scalaongears.MathematicalExpression._
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalaongears.operator.Matrix
import org.scalaongears.operator.Vector
import org.scalaongears.operator.Constant

class HomogenousTransformationTest extends FlatSpec with Matchers {

    val error = 0.0001
    
   "The translation 2.0 in X in (1.0, 1.0, 1.0, 1.0) " should "be (3.0, 1.0, 1.0, 1.0) " in {
    val oneVector = Vector(1.0, 1.0, 1.0, 1.0)
    val translationOf2inX = TranslateInX(2.0)
    val expectedVector = Vector(3.0, 1.0, 1.0, 1.0)
    (translationOf2inX() * Matrix(oneVector).transpose).evaluate() should be (
        Matrix(expectedVector).transpose()
    )
  }

  "The rotation of PI in X in (1.0, 0.0, 0.0, 1.0) " should "be (1.0, 0.0, 0.0, 1.0) " in {
    val oneVector = Vector(1.0, 0.0, 0.0, 1.0)
    val translationOf2inX = RotateInX(Math.PI)
    val expectedVector = Vector(1.0, 0.0, 0.0, 1.0)
    (translationOf2inX() * Matrix(oneVector).transpose).evaluate() should be (
        Matrix(expectedVector).transpose()
    )
  }

  "The rotation of PI/2 in X in (0.0, 1.0, 0.0, 1.0) " should "be (0.0, 0.0, 1.0, 1.0) " in {
    val oneVector = Vector(0.0, 1.0, 0.0, 1.0)
    val translationOf2inX = RotateInX(Math.PI/2)
    val rotatedVector = (translationOf2inX() * Matrix(oneVector).transpose).evaluate()
    rotatedVector(0)(0) match {
      case Constant(e) => e should (be > 0.0 - error and be < 0.0 + error)
      case _ => fail
    }   
    rotatedVector(1)(0) match {
      case Constant(e) => e should (be > 0.0 - error and be < 0.0 + error)
      case _ => fail
    }   
    rotatedVector(2)(0) match {
      case Constant(e) => e should (be > 1.0 - error and be < 1.0 + error)
      case _ => fail
    }   
    rotatedVector(3)(0) match {
      case Constant(e) => e should (be > 1.0 - error and be < 1.0 + error)
      case _ => fail
    }   
  }
   
  "The translation 2.0 in Z in (1.0, 1.0, 1.0, 1.0) " should "be (1.0, 1.0, 3.0, 1.0) " in {
    val oneVector = Vector(1.0, 1.0, 1.0, 1.0)
    val translationOf2inX = TranslateInZ(2.0)
    val expectedVector = Vector(1.0, 1.0, 3.0, 1.0)
    (translationOf2inX() * Matrix(oneVector).transpose).evaluate() should be (
        Matrix(expectedVector).transpose()
    )
  }

  "The rotation of -PI/2 in Z in (0.0, 1.0, 0.0, 1.0) " should "be (1.0, 0.0, 0.0, 1.0) " in {
    val oneVector = Vector(0.0, 1.0, 0.0, 1.0)
    val translationOf2inZ = RotateInZ(-Math.PI/2)
    val rotatedVector = (translationOf2inZ() * Matrix(oneVector).transpose).evaluate()
    rotatedVector(0)(0) match {
      case Constant(e) => e should (be > 1.0 - error and be < 1.0 + error)
      case _ => fail
    }   
    rotatedVector(1)(0) match {
      case Constant(e) => e should (be > 0.0 - error and be < 0.0 + error)
      case _ => fail
    }   
    rotatedVector(2)(0) match {
      case Constant(e) => e should (be > 0.0 - error and be < 0.0 + error)
      case _ => fail
    }   
    rotatedVector(3)(0) match {
      case Constant(e) => e should (be > 1.0 - error and be < 1.0 + error)
      case _ => fail
    }   
  }

}