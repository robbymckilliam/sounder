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
    val rec = playRecord(t => 50*sin(2*Pi*100*t), 0, 2.0, Fs)
    println("You should hear a recording play for about 2 seconds.") 
    playSamples(rec)
    val filet = new java.io.FileWriter("testdata.csv")
    (0 until rec.length) foreach { i =>
      filet.write((i.toDouble/Fs).toString.replace('E', 'e') + "\t" + rec(i).toString.replace('E', 'e')  + "\n")
    }
    filet.close
  }
  
  @Test def PlayTest() {
    println("Testing playback")
    println("You should hear a 100Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*100*t), 0.0, 2.0)
    println("You should hear a 200Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*200*t), 0.0, 2.0)
    println("You should hear a 500Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*500*t), 0.0, 2.0)
    println("You should hear a 1000Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*1000*t), 0.0, 2.0)
    println("You should hear a 2000Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*2000*t), 0.0, 2.0)
  }
  
}
