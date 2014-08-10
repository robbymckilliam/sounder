package sounder

import org.junit.Test
import org.junit.Assert._
import sounder.Sounder._
import sounder.FileIO.StereoWavReader
import sounder.FileIO.StereoWavWriter
import sounder.FileIO.MonoWavReader
import sounder.FileIO.WavWriter
import javax.sound.sampled.AudioFormat
import scala.math.sin
import scala.math.cos
import scala.math.Pi

class FileIOTest {

  @Test 
  def wavMonoWriterReaderTest {
    val Fs = 22050F
    val Ts = 1/Fs
    def f(t : Double) = 0.5*sin(2*Pi*200*t)
    val fs = (0.0 to 1.0 by Ts).map(t=>f(t)) //sequence of sample to play on left speaker
    WavWriter.writeMonoSamples(fs, "testfile.wav", Fs) //write these mono samples to a wav file
    
    val stereoreader = new StereoWavReader("testfile.wav")
    val fsrstereo = stereoreader.map( s => s._1 ).toArray //take the left sample
    assertTrue(fsrstereo.size == fs.size)
    fs.indices.foreach( i => assertEquals(fs(i), fsrstereo(i), 0.01) )
    //fs.indices.foreach( i => println(fs(i), fsrstereo(i)) )
    
    val monoreader = new MonoWavReader("testfile.wav")
    val fsrmono = monoreader.toArray //puts samples in an array
    assertTrue(fsrmono.size == fs.size)
    fs.indices.foreach( i => assertEquals(fs(i), fsrmono(i), 0.01) )
    //fs.indices.foreach( i => println(fs(i), fsrmono(i)) )
    
  }
  
  @Test 
  def wavStereoWriterReaderTest {
    val Fs = 44100F
    val Ts = 1/Fs
    def fl(t : Double) = 0.5*sin(2*Pi*200*t)
    def fr(t : Double) = 0.5*sin(2*Pi*1000*t)
    val writer = new StereoWavWriter("teststereowriter.wav")
    val ts = (0.0 to 2.0 by Ts)
    ts.foreach( t => writer.put(fl(t), fr(t)))
    writer.close
    
    val fsrstereo = new StereoWavReader("teststereowriter.wav").toArray //array of (Double, Double) tuples
    assertTrue(fsrstereo.size == ts.size)
    ( ts, fsrstereo).zipped.foreach { (t, s) => //assert left and right sample are correct
      assertEquals(fl(t), s._1, 0.01)
      assertEquals(fr(t), s._2, 0.01)
    }
    //fs.indices.foreach( i => println(fs(i), fsrstereo(i)) )
    
    val fsrmono = new MonoWavReader("teststereowriter.wav").toArray //puts samples in an array
    assertTrue(fsrmono.size == ts.size)
    ( ts, fsrmono).zipped.foreach { (t, s) => assertEquals( (fl(t) + fr(t))/2.0, s, 0.01) } //should average stereo samples
    //( ts, fsrmono).zipped.foreach { (t, s) => println(t, s, (fl(t) + fr(t))/2, fl(t), fr(t)) }
    
  }
  
  @Test 
  def testWritingEmptyWav {
    val audioFormat = new AudioFormat(44100F,16,1,true,true)
    WavWriter.writeEmptyWav("empty.wav",audioFormat) //write an empty wav
    val monoreader = new MonoWavReader("empty.wav")
    assertFalse(monoreader.hasNext) //assert it's empty
    val fsrmono = monoreader.toArray //puts samples in an array
    assertTrue(fsrmono.size==0)
  }
  
  @Test 
  def perSampleWaveWriter {
    val Fs = 48000F
    val Ts = 1/Fs
    def f(t : Double) = 0.5*sin(2*Pi*200*t)
    
    val writer = WavWriter.mono("testsequentialwriter.wav", Fs)
    val ts = (0.0 to 1.0 by Ts)
    ts.foreach( t => writer.put(f(t)) )
    writer.close
    
    val monoreader = new MonoWavReader("testsequentialwriter.wav")
    val fsrmono = monoreader.toArray //puts samples in an array
    assertTrue(fsrmono.size == ts.size)
    fsrmono.indices.foreach( i => assertEquals(f(ts(i)), fsrmono(i), 0.01) )
    //fsrmono.indices.foreach( i => println(ts(i), fsrmono(i)) )
    monoreader.close
    
  }  

}
