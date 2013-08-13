package sounder

import scala.math.sin
import scala.math.cos
import scala.math.min
import scala.math.max
import scala.math.floor
import scala.math.round
import scala.math.ceil
import scala.math.Pi
import scala.math.pow
import scala.math.log
import scala.math.sqrt
import scala.math.exp

/**
 * Static object containing commonly used functions. 
 * @author Robby McKilliam
 */
object Util {
    
  /** The sinc function sin(pi x)/(pi x) */
  def sinc(t : Double) : Double = { 
    if(t.abs < 5e-3 ) {  //use a 4th order expansion if t is small
      val t2 = t*t
      return  1.0 - t2*( 1.0/6 - 1.0/120*t2 )
    }
    else return sin(Pi*t)/(Pi*t)
  }
  
  /** First derivative of the sinc function */
  def dsinc(t : Double) : Double = { 
    if(t.abs < 5e-3 ) {  //use a 4th order expansion if t is small
      return  -Pi*Pi*t/3.0 + pow(Pi,4)*t*t*t/30.0
    }
    else return (Pi*t*cos(Pi*t) - sin(Pi*t)) / (Pi*t*t) 
  }
  
   /** Second derivative of the sinc function */
  def d2sinc(t : Double) : Double = { 
    if(t.abs < 5e-3 ) {  //use a 4th order expansion if t is small
      return  Pi*Pi*( -1.0/3 + pow(Pi,4)*t*t/10 - pow(Pi,6)*pow(t,4)/168)
    }
    else return ((2 - Pi*Pi*t*t)*sin(Pi*t) -2*Pi*t*cos(Pi*t) )/Pi/t/t/t;
  }
  
  /** Return a Sherpard tone (sonic barbers pole) at time t */
  def sherpard(t : Double, F : Double = 1000, T : Double = 10, sigma : Double = 2, A : Double = 1.0) : Double = {
    def chirp(t : Double) : Double = exp(-t*t/sigma/sigma)*sin(2*Pi*F*pow(2,-t)*t)/sigma/sqrt(Pi)
    val kmin = round(t/T - 15).toInt
    val kmax = round(t/T + 15).toInt
    val out = (kmin to kmax).foldLeft(0.0) ( (sum, k) => sum + chirp(t/T - k)/T*2 )
    return out
  }

}