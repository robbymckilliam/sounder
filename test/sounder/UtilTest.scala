/*
 * @author Robby McKilliam
 */

package sounder

import org.junit.Test
import org.junit.Assert._
import sounder.Util._
import scala.math.Pi
import scala.math.round
import sounder.Sounder._

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
  
  @Test
  def shepardTest() {
    val f0 = 220 //center frequency of the tone
    val R = 15
    val h = 1/Pi
    val T = R*h //period of the shepard tone
    val numloops = 3 //number of period that will be played
    
    println("You should hear a Shepard tone play for about " + round(numloops*T).toInt + " seconds")
    
    val clip = constructClip(shepard(_, f0,R,h), 0.0, T) //construct a clip containing a single period
    clip.loop(numloops)
    Thread.sleep(5)
    clip.drain
    clip.stop
    clip.close  
  }
  
}
