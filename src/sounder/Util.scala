package sounder

import scala.math.sin
import scala.math.min
import scala.math.max
import scala.math.floor
import scala.math.ceil
import scala.math.Pi

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

}