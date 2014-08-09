/*
 * @author Robby McKilliam
 */

package sounder

import org.junit.Test
import org.junit.Assert._
import sounder.Sounder._
import sounder.FileIO.waveWriter
import sounder.FileIO.WaveReader
import scala.math.sin
import scala.math.cos
import scala.math.Pi

class FileIOTest {

  @Test
  def waveWriterReaderTest() {
    val Fs = 22050F
    val Ts = 1/Fs
    def f(t : Double) = 0.5*sin(2*Pi*200*t)
    val fs = (0.0 to 1.0 by Ts).map(t=>f(t)) //sequence of sample to play on left speaker
    waveWriter(fs, "testfile.wav", Fs)
    
    val reader = new WaveReader("testfile.wav")
    val fsr = reader.map( s => s._1 ).toArray //take the left sample
    assertTrue(fsr.size == fs.size)
    fs.indices.foreach( i => assertEquals(fs(i), fsr(i), 0.01) )
    //fs.indices.foreach( i => println(fs(i), fsr(i)) )
    
  }

}
