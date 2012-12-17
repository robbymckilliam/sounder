/*
 * Thomas Stratfold - Function to call different types of waves
 */

package sounder

import scala.math.abs
import scala.math.Pi
import scala.util.control.Exception._
import java.io.IOException
//import sounder.Sounder.play

// a = amplitude, f = frequency, p = phase in degrees
class WaveFunct(val a: Double, val f: Double, val p: Double = 0) {
  def isValid(): Boolean = f match{
    case _ if(f > 20000 || f < 20 ) => {false; throw new IOException("Frequency must be between 20Hz and 20,000Hz")}
    case _ if(a < 0 || a > 100) => {false; throw new IOException("Amplitude must be between 0 and 100")}
    case _ => true
  }
  
  def rect(t: Double): Double = t match {
    case _ if(abs(t) > 0.5)  => 0
    case _ if(abs(t) < 0.5)  => 1
    case _ if(abs(t) == 0.5) => 0.5
  }
  
  def sgn(t: Double): Double = t match {
    case _ if(t > 0)  => 1
    case _ if(t < 0)  => (-1)
    case _ if(t == 0) => 0
  }
  
  def squW(t: Double): Double = a*sgn(sin(t+(p*Pi/180)))

  def sawW(t: Double): Double = a*(t*f - scala.math.floor(t*f))
  
  def triW(t: Double): Double = abs(2*(t-scala.math.floor(t+1/2)))
  
  /**def triW2(t: Double): Double = t match {
    case _ if()
  }*/
  
  def sin(t: Double): Double = a*scala.math.sin(2*Pi*t*f+(p*Pi/180))
  def cos(t: Double): Double = a*scala.math.cos(2*Pi*t*f+(p*Pi/180))
  def tan(t: Double): Double = a*scala.math.tan(2*Pi*t*f+(p*Pi/180))
  

}
