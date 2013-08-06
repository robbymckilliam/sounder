/*
 * @author Robby McKilliam
 */

package sounder

import org.junit.Test
import org.junit.Assert._
import sounder.Sounder._
import scala.math.sin
import scala.math.Pi

class SounderTest {
  
  @Test
  def PlayRecordTest() {
    println("Testing playback and recording")
    println("You should hear a 150Hz tone play for 2 seconds.") 
    val Fs = 44100
    val (left, right) = playRecord(t => 0.5*sin(2*Pi*100*t), 0, 2.0, Fs)
    println("Playing back the left channel") 
    playSamples(left)
    println("Playing back the right channel") 
    playSamples(right)
    val filet = new java.io.FileWriter("testdata.csv")
    (0 until left.length) foreach { i =>
      filet.write((i.toDouble/Fs).toString.replace('E', 'e') + "\t" + left(i).toString.replace('E', 'e')  + "\t" + right(i).toString.replace('E', 'e')  + "\n")
    }
    filet.close
  }
  
  @Test def PlayTest() {
    println("Testing playback")
    println("You should hear a 100Hz tone play for 2 seconds.") 
    play(t => 0.5*sin(2*Pi*100*t), 0.0, 2.0)
    println("You should hear a 200Hz tone play for 2 seconds.") 
    play(t => 0.5*sin(2*Pi*200*t), 0.0, 2.0)
    println("You should hear a 500Hz tone play for 2 seconds.") 
    play(t => 0.5*sin(2*Pi*500*t), 0.0, 2.0)
    println("You should hear a 1000Hz tone play for 2 seconds.") 
    play(t => 0.5*sin(2*Pi*1000*t), 0.0, 2.0)
    println("You should hear a 2000Hz tone play for 2 seconds.") 
    play(t => 0.5*sin(2*Pi*2000*t), 0.0, 2.0)
  }
  
}
