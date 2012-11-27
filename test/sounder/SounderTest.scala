/*
 * @author Robby McKilliam
 */

package sounder

import org.junit.Test
import org.junit.Assert._
import sounder.Sounder.play
import scala.math.sin
import scala.math.Pi

class SounderTest {
  
  @Test def PlayTest() {
    println("Testing playback")
    println("You should here a 100Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*100*t), 0.0, 2.0)
    println("You should here a 200Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*200*t), 0.0, 2.0)
    println("You should here a 500Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*500*t), 0.0, 2.0)
    println("You should here a 1000Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*1000*t), 0.0, 2.0)
    println("You should here a 2000Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*2000*t), 0.0, 2.0)
  }
  
  @Test def SounderTest() {
    println("Testing the channel sounder. Connect the output of the soundcard to the input")
    println("You should here a 150Hz tone play for 2 seconds.")
    val s = new Sounder(t => 50*sin(2*Pi*150*t), 2.0)
    println("Now playing the recorded copy.")
    play(s.y, -3, 8)
  }
  
}
