package org.scalaongears

import scala.collection.mutable.WrappedArray
import org.scalaongears.MathematicalExpression._
import org.scalaongears.operator.Sin
import org.scalaongears.operator.Matrix
import org.scalaongears.operator.Cos
import org.scalaongears.operator.Vector
import org.scalaongears.operator.Constant

/**
 * Classical 3D affine transformations using 4x4 Matrices 
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 *
 */
object HomogeneousTransformation {

 /**
 * Trait that defines an Homogeneous transformation:
 * a function that returns a Matrix ( it should be 
 * a 4x4 one!)
 */
trait HomogeneousTransformation {
    def apply(): Matrix[MathematicalExpression]
}

/**
 * Identity Matrix
 */
case class Identity() extends HomogeneousTransformation {
    def apply(): Matrix[MathematicalExpression] = {
      Matrix(
        Vector(1.0, 0.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    }
  }

 /**
 * Translation of <code>distance</code> in the Z-Axis
 */
case class TranslateInZ(distance: MathematicalExpression) extends HomogeneousTransformation {
    def apply(): Matrix[MathematicalExpression] = {
      Matrix(
        Vector(1.0, 0.0, 0.0, 0.0     ),
        Vector(0.0, 1.0, 0.0, 0.0     ),
        Vector(0.0, 0.0, 1.0, distance),
        Vector(0.0, 0.0, 0.0, 1.0     )
      )
    }
  }

/**
 * Rotation of <code>angle</code> degrees around the Z-Axis 
 */
case class RotateInZ(angle: MathematicalExpression) extends HomogeneousTransformation {
    def apply(): Matrix[MathematicalExpression] = {
      Matrix(
        Vector(Cos(angle), -Sin(angle), 0.0, 0.0),
        Vector(Sin(angle), Cos(angle) , 0.0, 0.0),
        Vector(0.0       , 0.0        , 1.0, 0.0),
        Vector(0.0       , 0.0        , 0.0, 1.0)
       )
    }
  }

/**
 * Translation of <code>distance</code> in the X-Axis 
 *
 */
case class TranslateInX(distance: MathematicalExpression) extends HomogeneousTransformation {
    def apply(): Matrix[MathematicalExpression] = {
      Matrix(
        Vector(1.0, 0.0, 0.0, distance),
        Vector(0.0, 1.0, 0.0, 0.0     ),
        Vector(0.0, 0.0, 1.0, 0.0     ),
        Vector(0.0, 0.0, 0.0, 1.0     )
      )
    }
  }

/**
 * Rotation of <code>angle</code> degrees in the X-axis 
 */
case class RotateInX(angle: MathematicalExpression) extends HomogeneousTransformation {
    def apply(): Matrix[MathematicalExpression] = {
      Matrix(
        Vector(1.0, 0.0       , 0.0        , 0.0),
        Vector(0.0, Cos(angle), -Sin(angle), 0.0),
        Vector(0.0, Sin(angle), Cos(angle) , 0.0),
        Vector(0.0, 0.0       , 0.0        , 1.0)
      )
    }
  }

}