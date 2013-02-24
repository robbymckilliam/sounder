/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package sounder

import math.cos
import math.Pi

object HamWind {
  def hamm(N: Int): Array[Double] = {
    val w = new Array[Double](N)
    for(i <- 0 until N) {
      w(i) = 0.54-0.46*cos(2*Pi*i/(N-1))
    }
    w
  }
    
  
}