/*
 * @author Robby McKilliam
 */

package sounder

import org.junit.Test
import org.junit.Assert._
import sounder.Util._
import scala.math.Pi

class UtilTest {
  
  @Test
  def SincTest() {
    println("Testing sinc function")
    val tol = 1e-8
    //output from Mathematica
    assertEquals(sinc(0),1.0,tol)
    assertEquals(sinc(0.1),0.9836316431,tol)
    assertEquals(sinc(-0.1),0.9836316431,tol)
    assertEquals(sinc(0.5),0.6366197724,tol)
    assertEquals(sinc(-0.5),0.6366197724,tol)
    assertEquals(sinc(-0.5),0.6366197724,tol)
    assertEquals(sinc(-10.5),0.03031522726,tol)
  }
  
  @Test
  def dSincTest() {
    println("Testing derivative of sinc")
    val tol = 1e-8
    //output from Mathematica
    assertEquals(dsinc(0),0.0,tol)
    assertEquals(dsinc(0.1),-0.3257512679,tol)
    assertEquals(dsinc(-0.2),0.6323614471,tol)
    assertEquals(dsinc(0.2),-0.6323614471,tol)
    assertEquals(dsinc(-33.0/10.0),0.1544695340,tol)
    assertEquals(dsinc(33.0/10.0),-0.1544695340,tol)
  }
  
  @Test
  def d2SincTest() {
    println("Testing second derivative of sinc")
    val tol = 1e-8
    //output from Mathematica
    assertEquals(d2sinc(0),-Pi*Pi/3,tol)
    assertEquals(d2sinc(1e-6),-3.289868134,tol)
    assertEquals(d2sinc(-1e-6),-3.289868134,tol)
    assertEquals(d2sinc(0.1),-3.193029836,tol)
    assertEquals(d2sinc(-0.1),-3.193029836,tol)
    assertEquals(d2sinc(11.0/10),2.306741312,tol)
    assertEquals(d2sinc(-11.0/10),2.306741312,tol)
  }
  
}
