package org.scalaongears

import org.scalaongears.MathematicalExpression._
import org.scalaongears.HomogeneousTransformation.Identity
import org.scalaongears.HomogeneousTransformation._
import org.scalaongears.operator.Matrix
import org.scalaongears.operator.Vector

/**
 *
 * Mechanical transformations following the Denavit-Hartenberg 
 * conventions.
 * 
 * The original coordinate system is:
 *     
 *      Z
 *      |
 *      |____Y
 *     /
 *    /
 *   X
 * 
 * @author Pau Carré Cardona ( pau.carre@gmail.com )
 * 
 */
object DenavitHartenbergConvention {

/**
 * Companion Object to define D-H transformations 
 * as sequences 
 */
object DenavitHartenberg {
    /**
     * @param denavitHartenbergTransformations - list of D-H transformations to apply
     * @return The D-H  system using the <code>denavitHartenbergTransformations</code> transformations
     */
    def apply(denavitHartenbergTransformations: DenavitHartenbergTransformation*) : DenavitHartenberg =  {
      new DenavitHartenberg(denavitHartenbergTransformations.toList)
    }
    
    def positionVector(forwardKinematicsMatrix : Matrix[MathematicalExpression]) : Vector[MathematicalExpression] = {
      Vector(
        forwardKinematicsMatrix(0)(3),
        forwardKinematicsMatrix(1)(3),
        forwardKinematicsMatrix(2)(3)
      )
    }
    
  }


/**
 * The D-H class that holds the transformations 
 * and the methods to compute them
 */
case class DenavitHartenberg(denavitHartenbergTransformations: List[DenavitHartenbergTransformation]) {
	  def apply() : Matrix[MathematicalExpression] = {
	    denavitHartenbergTransformations.foldLeft(Identity()())(
	        (matrix : Matrix[MathematicalExpression], dhTrans : DenavitHartenbergTransformation) => matrix * dhTrans() 
	    )
	  }
    
  }

/**
 * Single D-H transformation for one mechanical joint 
 * composed out of:
 * 
 * @arg structureAngleX - Angle around the X-Axis that describes the mechanical joint
 * @arg structureDistanceX - Distance along the X-Axis that describes the mechanical joint 
 * @arg actuatorAngleZ - Angle around the Z-Axis that describes the movement of an angular actuator 
 * @arg actuatorDistanceZ - Distance around the Z-Axis that describes the movement of a linear actuator
 * 
 */
case class DenavitHartenbergTransformation(
      structureAngleX: MathematicalExpression,
      structureDistanceX: MathematicalExpression,
      actuatorAngleZ: MathematicalExpression,
      actuatorDistanceZ: MathematicalExpression
      ) {
    def apply(): Matrix[MathematicalExpression] = {
      TranslateInZ(actuatorDistanceZ)() * RotateInZ(actuatorAngleZ)() * TranslateInX(structureDistanceX)() * RotateInX(structureAngleX)()
    }
    
  }

}