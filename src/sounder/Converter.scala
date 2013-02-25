/*
 * Thomas Stratfold 17/01/2013
 * This method takes in variables in a list format, and then plays the
 * corresponding signal through the play function. Allows for up to 5 different
 * functions to be played, otherwise plays nothing.
 */

package sounder

import scala.math.sin
import scala.math.cos
import scala.math.tan
import scala.math.Pi
import sounder.Sounder.play
import sounder.Sounder.playRecord

object Converter {
  // str = types of functions  a = amplitudes  f = frequencies  x = time
  def toPlay(str: List[String], a: List[Double], f: List[Double],x: Double) {
    str.length match {  // Compares the length of the functions to determine how many need to be played
        // One function
      case 1 => play(t=> (str(0) match {case "sin" => a(0)*sin(2*Pi*t*f(0))
                                        case "cos" => a(0)*cos(2*Pi*t*f(0))
                                        case "tan" => a(0)*tan(2*Pi*t*f(0))
                                       }),0,x)
        // Two different functions
      case 2 => play(t=> (str(0) match {case "sin" => a(0)*sin(2*Pi*t*f(0))
                                        case "cos" => a(0)*cos(2*Pi*t*f(0))
                                        case "tan" => a(0)*tan(2*Pi*t*f(0))
                                       })
                        +(str(1) match {case "sin" => a(1)*sin(2*Pi*t*f(1))
                                        case "cos" => a(1)*cos(2*Pi*t*f(1))
                                        case "tan" => a(1)*tan(2*Pi*t*f(1))
                                       }),0,x)
        // Three different functions
      case 3 => play(t=> (str(0) match {case "sin" => a(0)*sin(2*Pi*t*f(0))
                                        case "cos" => a(0)*cos(2*Pi*t*f(0))
                                        case "tan" => a(0)*tan(2*Pi*t*f(0))
                                       })
                        +(str(1) match {case "sin" => a(1)*sin(2*Pi*t*f(1))
                                        case "cos" => a(1)*cos(2*Pi*t*f(1))
                                        case "tan" => a(1)*tan(2*Pi*t*f(1))
                                       })
                        +(str(2) match {case "sin" => a(2)*sin(2*Pi*t*f(2))
                                        case "cos" => a(2)*cos(2*Pi*t*f(2))
                                        case "tan" => a(2)*tan(2*Pi*t*f(2))
                                       }),0,x)
        // Four different functions
      case 4 => play(t=> (str(0) match {case "sin" => a(0)*sin(2*Pi*t*f(0))
                                        case "cos" => a(0)*cos(2*Pi*t*f(0))
                                        case "tan" => a(0)*tan(2*Pi*t*f(0))
                                       })
                        +(str(1) match {case "sin" => a(1)*sin(2*Pi*t*f(1))
                                        case "cos" => a(1)*cos(2*Pi*t*f(1))
                                        case "tan" => a(1)*tan(2*Pi*t*f(1))
                                       })
                        +(str(2) match {case "sin" => a(2)*sin(2*Pi*t*f(2))
                                        case "cos" => a(2)*cos(2*Pi*t*f(2))
                                        case "tan" => a(2)*tan(2*Pi*t*f(2))
                                       })
                        +(str(3) match {case "sin" => a(3)*sin(2*Pi*t*f(3))
                                        case "cos" => a(3)*cos(2*Pi*t*f(3))
                                        case "tan" => a(3)*tan(2*Pi*t*f(3))
                                       }),0,x)
        // Five different functions
      case 5 => play(t=> (str(0) match {case "sin" => a(0)*sin(2*Pi*t*f(0))
                                        case "cos" => a(0)*cos(2*Pi*t*f(0))
                                        case "tan" => a(0)*tan(2*Pi*t*f(0))
                                       })
                        +(str(1) match {case "sin" => a(1)*sin(2*Pi*t*f(1))
                                        case "cos" => a(1)*cos(2*Pi*t*f(1))
                                        case "tan" => a(1)*tan(2*Pi*t*f(1))
                                       })
                        +(str(2) match {case "sin" => a(2)*sin(2*Pi*t*f(2))
                                        case "cos" => a(2)*cos(2*Pi*t*f(2))
                                        case "tan" => a(2)*tan(2*Pi*t*f(2))
                                       })
                        +(str(3) match {case "sin" => a(3)*sin(2*Pi*t*f(3))
                                        case "cos" => a(3)*cos(2*Pi*t*f(3))
                                        case "tan" => a(3)*tan(2*Pi*t*f(3))
                                       })
                        +(str(4) match {case "sin" => a(4)*sin(2*Pi*t*f(4))
                                        case "cos" => a(4)*cos(2*Pi*t*f(4))
                                        case "tan" => a(4)*tan(2*Pi*t*f(4))
                                       }),0,x)
   // Will allow for up to 5 different functions else plays nothing
      case _ => play(t=>t,0,0)
    }
    
  }
  
  def toPlayRecord(str: List[String], a: List[Double], f: List[Double],x: Double): Array[Byte] = {
    str.length match {  // Compares the length of the functions to determine how many need to be played
        // One function
      case 1 => playRecord(t=> (str(0) match {case "sin" => a(0)*sin(2*Pi*t*f(0))
                                        case "cos" => a(0)*cos(2*Pi*t*f(0))
                                        case "tan" => a(0)*tan(2*Pi*t*f(0))
                                       }),0,x)
        // Two different functions
      case 2 => playRecord(t=> (str(0) match {case "sin" => a(0)*sin(2*Pi*t*f(0))
                                        case "cos" => a(0)*cos(2*Pi*t*f(0))
                                        case "tan" => a(0)*tan(2*Pi*t*f(0))
                                       })
                        +(str(1) match {case "sin" => a(1)*sin(2*Pi*t*f(1))
                                        case "cos" => a(1)*cos(2*Pi*t*f(1))
                                        case "tan" => a(1)*tan(2*Pi*t*f(1))
                                       }),0,x)
        // Three different functions
      case 3 => playRecord(t=> (str(0) match {case "sin" => a(0)*sin(2*Pi*t*f(0))
                                        case "cos" => a(0)*cos(2*Pi*t*f(0))
                                        case "tan" => a(0)*tan(2*Pi*t*f(0))
                                       })
                        +(str(1) match {case "sin" => a(1)*sin(2*Pi*t*f(1))
                                        case "cos" => a(1)*cos(2*Pi*t*f(1))
                                        case "tan" => a(1)*tan(2*Pi*t*f(1))
                                       })
                        +(str(2) match {case "sin" => a(2)*sin(2*Pi*t*f(2))
                                        case "cos" => a(2)*cos(2*Pi*t*f(2))
                                        case "tan" => a(2)*tan(2*Pi*t*f(2))
                                       }),0,x)
        // Four different functions
      case 4 => playRecord(t=> (str(0) match {case "sin" => a(0)*sin(2*Pi*t*f(0))
                                        case "cos" => a(0)*cos(2*Pi*t*f(0))
                                        case "tan" => a(0)*tan(2*Pi*t*f(0))
                                       })
                        +(str(1) match {case "sin" => a(1)*sin(2*Pi*t*f(1))
                                        case "cos" => a(1)*cos(2*Pi*t*f(1))
                                        case "tan" => a(1)*tan(2*Pi*t*f(1))
                                       })
                        +(str(2) match {case "sin" => a(2)*sin(2*Pi*t*f(2))
                                        case "cos" => a(2)*cos(2*Pi*t*f(2))
                                        case "tan" => a(2)*tan(2*Pi*t*f(2))
                                       })
                        +(str(3) match {case "sin" => a(3)*sin(2*Pi*t*f(3))
                                        case "cos" => a(3)*cos(2*Pi*t*f(3))
                                        case "tan" => a(3)*tan(2*Pi*t*f(3))
                                       }),0,x)
        // Five different functions
      case 5 => playRecord(t=> (str(0) match {case "sin" => a(0)*sin(2*Pi*t*f(0))
                                        case "cos" => a(0)*cos(2*Pi*t*f(0))
                                        case "tan" => a(0)*tan(2*Pi*t*f(0))
                                       })
                        +(str(1) match {case "sin" => a(1)*sin(2*Pi*t*f(1))
                                        case "cos" => a(1)*cos(2*Pi*t*f(1))
                                        case "tan" => a(1)*tan(2*Pi*t*f(1))
                                       })
                        +(str(2) match {case "sin" => a(2)*sin(2*Pi*t*f(2))
                                        case "cos" => a(2)*cos(2*Pi*t*f(2))
                                        case "tan" => a(2)*tan(2*Pi*t*f(2))
                                       })
                        +(str(3) match {case "sin" => a(3)*sin(2*Pi*t*f(3))
                                        case "cos" => a(3)*cos(2*Pi*t*f(3))
                                        case "tan" => a(3)*tan(2*Pi*t*f(3))
                                       })
                        +(str(4) match {case "sin" => a(4)*sin(2*Pi*t*f(4))
                                        case "cos" => a(4)*cos(2*Pi*t*f(4))
                                        case "tan" => a(4)*tan(2*Pi*t*f(4))
                                       }),0,x)
   // Will allow for up to 5 different functions else plays nothing
      case _ => playRecord(t=>t,0,0)
    }
    
  }
}
