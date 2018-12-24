package org.scalaongears

import scala.util.Random
import org.scalaongears.MathematicalExpression._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalaongears.operator._

class MathematicalExpressionTests extends FlatSpec with Matchers {

  val error = 0.0001

  "The evaluation of ( (x, 10) (20, y) ) with x=PI,y=0" should "be ( (PI, 10) (20, 0) )" in {
    Matrix(
      Vector("x", 10.0),
      Vector(20, "y")
    ).evaluate(Map("x" -> Math.PI, "y" -> 0.0)) should be (
      Matrix(
        Vector(Math.PI, 10.0),
        Vector(20, 0.0)
      )
    )
  }
  
  "The evaluation of (x, 10) with x=PI" should "be (PI, 10)" in {
    Vector("x", 10.0).evaluate(Map("x" -> Math.PI)) should be(
      Vector(Math.PI, 10.0))
  }

  // divide evaluation   
  "The division of two random constants" should " be the division of the constants" in {
    val randLeft: Double = Random.nextDouble()
    var randRight: Double = Random.nextDouble()
    if (randRight.abs < error) {
      randRight = 1.0 // set 1.0 to avoid zero division
    }
    Divide(randLeft, randRight).evaluate() match {
      case cte: Constant => cte.constantValue should
        (be > (randLeft / randRight) - error and be < (randLeft / randRight) + error)
      case _ => fail
    }
  }

  // multipy evaluation   
  "The multiplication of two random constants" should " be the multiplication of the constants" in {
    val randLeft: Double = Random.nextDouble()
    val randRight: Double = Random.nextDouble()
    (randLeft * randRight).evaluate() match {
      case cte: Constant => cte.constantValue should
        (be > (randLeft * randRight) - error and be < (randLeft * randRight) + error)
      case _ => fail
    }
  }

  // substraction evaluation   
  "The substraction of two random constants" should " be the substraction of the constants" in {
    val randLeft: Double = Random.nextDouble()
    val randRight: Double = Random.nextDouble()
    (randLeft - randRight).evaluate() match {
      case cte: Constant => cte.constantValue should
        (be > (randLeft - randRight) - error and be < (randLeft - randRight) + error)
      case _ => fail
    }
  }

  // addition evaluation   
  "The addition of two random constants" should " be the addition of the constants" in {
    val randLeft: Double = Random.nextDouble()
    val randRight: Double = Random.nextDouble()
    (randLeft + randRight).evaluate() match {
      case cte: Constant => cte.constantValue should
        (be > (randLeft + randRight) - error and be < (randLeft + randRight) + error)
      case _ => fail
    }
  }

  // constant evaluation
  "The constant of a random number" should "be that random number" in {
    val rand: Double = Random.nextDouble()
    rand.evaluate() match {
      case cte: Constant => cte.constantValue should be(rand)
      case _ => fail
    }
  }

  // minus evaluation
  "The constant of minus a random number" should "be the negative of that random number" in {
    val rand: Double = Random.nextDouble()
    (-rand).evaluate() match {
      case cte: Constant => cte.constantValue should be(-rand)
      case _ => fail
    }
  }

  // cosinus evaluation  
  "The cosinus of zero" should "be one" in {
    Cos(0.0).evaluate() should be(Constant(1.0))
  }
  "The cosinus of half PI" should "be zero" in {
    Cos(Math.PI / 2).evaluate() match {
      case cte: Constant => cte.constantValue should (be > -error and be < error)
      case _ => fail
    }
  }

  // sinus evaluation 
  "The sinus of zero" should "be zero" in {
    Sin(0.0).evaluate() match {
      case cte: Constant => cte.constantValue should (be > -error and be < error)
      case _ => fail
    }
  }

  "The sinus of zero" should "not be different from zero" in {
    Sin(0.0).evaluate() match {
      case cte: Constant => cte.constantValue should not (be < -error and be > error)
      case _ => fail
    }
  }

  
  "The sinus of ninus PI" should "be zero" in {
    Sin(-Math.PI).evaluate() match {
      case cte: Constant => cte.constantValue should (be > -error and be < error)
      case _ => fail
    }
  }

}