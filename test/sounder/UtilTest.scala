/*
 * @author Robby McKilliam
 */

package sounder

import org.junit.Test
import org.junit.Assert._
import sounder.Util._

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
  
}
