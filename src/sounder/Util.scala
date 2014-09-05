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
  
  /** Returns the (centered) fractional part of t */
  def fracpart(t : Double) : Double = t - round(t)
  
  /** Takes its argument modulo 2pi in to the interval [0,2pi] */
  def mod2pi(x : Double) = 2*Pi*(x/2/Pi - floor(x/2/Pi))
  
  /** The natual logarithm of 2 */
  val ln2 = log(2)
  /** logarithm with base 2 */
  def log2(x : Double) = log(x)/ln2
    
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
  
  /** Return a Sherpard tone (sonic barbers pole) at time t.
   * f0 : center frequency where amplitude is largest (default f0=220Hz)
   * R : exponential frequency decay rate. Value of 4 means it takes 4 seconds for the frequency to half (default 15)
   * h : spacing between frequencies in octaves, Value of 1 meas an octave spacing (default 1/Pi)
   * The signal constructed will have period T=Rh
   */
  def shepard(t : Double, f0 : Double = 220, R : Double = 15, h : Double = 1.0/Pi) : Double = {
    val B = R //kernel bandwidth, how fast the high and low frequencies are attenuated (fixed to R)
    def s(t : Double) : Double = exp(-t*t/B)*sin(2*Pi*f0*pow(2,-t/R)*t)/sqrt(Pi*B) //chirp pulse
    val T = R*h //period
    val kmin = floor(-10*B/T).toInt //sum out to 10 standard deviations in positive and negative direction
    val kmax = ceil(10*B/T).toInt
    val tp = T*fracpart(t/T) //get equivalent time inside fundamental period
    val out = (kmin to kmax).foldLeft(0.0) ( (sum, k) => sum + s(tp - T*k)*T/1.5 )
    return out
  }

  /*
   * Create a Hamming window of size N
   *  Author: Thomas Stratfold
   */
  def hammingwindow(N: Int) = (0 until N).map( n => 0.54-0.46*cos(2*Pi*n/(N-1)) )
  
}