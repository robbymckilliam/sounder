package sounder

import org.junit.Test
import org.junit.Assert._
import sounder.Sounder._
import sounder.FileIO.StereoWavReader
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
    stereoreader.close
    
    val monoreader = new MonoWavReader("testfile.wav")
    val fsrmono = monoreader.toArray //puts samples in an array
    assertTrue(fsrmono.size == fs.size)
    fs.indices.foreach( i => assertEquals(fs(i), fsrmono(i), 0.01) )
    //fs.indices.foreach( i => println(fs(i), fsrmono(i)) )
    monoreader.close
    
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
