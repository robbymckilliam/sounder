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
    println("You should here a 100Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*100*t), 0.0, 2.0)
    println("You should here a 200Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*200*t), 0.0, 2.0)
    println("You should here a 500Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*500*t), 0.0, 2.0)
    println("You should here a 1000Hz tone play for 2 seconds.") 
    play(t => 50*sin(2*Pi*10ls
                     00*t), 0.0, 2.0)
    
  }
  
}
