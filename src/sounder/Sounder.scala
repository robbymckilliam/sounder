/*
 * Sounder.
 * 
 * @author Robby McKilliam
 * Extended by Thomas Stratfold
 */

package sounder

import javax.sound.sampled._ 
import scala.math.ceil 
import scala.math.round 
import scala.Short
import java.io._
import java.nio._
import scala.collection.mutable._

object Sounder {
  
  /** 
   * Plays the function f from time start to time stop out of the speakers.  Optional aguments are 
   * sampleRate (default 44100Hz i.e. CD quality) and clipLevel (default 100) which specfies the
   * maximum magnitude f can attain before being clipped (wrapped).
   */
  def play(f : Double => Double, start : Double, stop : Double, sampleRate : Float = 44100F, clipLevel : Double = 100.0): Array[Byte] = {
    val audioFormat = new AudioFormat(
      sampleRate, //sample rate
      16, //bits per sample (corresponds with Short)
      1, //number of channels, 1 for mono, 2 for stereo
      true, //true = signed, false is unsigned
      true //bigEndian
    )
    val info = new DataLine.Info(classOf[Clip], audioFormat) 
    val clip = AudioSystem.getLine(info).asInstanceOf[Clip] //cast required in java's sound API, at bit annoying
        
    val duration = stop - start
    val numSamples = scala.math.round(duration*sampleRate).toInt
    val buff = ByteBuffer.allocate(numSamples*audioFormat.getFrameSize) //buffer for sound
    //buff.order(ByteOrder.LITTLE_ENDIAN)
    for( i <- 1 to numSamples ) {
      //quantise to a short.  This clips (wraps) if the function is larger than 1
      val v = scala.math.round(scala.Short.MaxValue/clipLevel*f(i/sampleRate + start)).toShort
      buff.putShort(v);
    }
      
    clip.open(audioFormat, buff.array, 0, numSamples*audioFormat.getFrameSize)
    clip.start
    Thread.sleep(5)
    clip.drain
    clip.stop
    clip.close
    buff.array
  }
  
  def record(start: Double, stop: Double, sampleRate: Float = 44100F, clipLevel: Double = 100.0, recorderTail: Double = 0.05) {
    val audioFormat = new AudioFormat(sampleRate, 16, 1, true, true)
    
    val recorderInfo = new DataLine.Info(classOf[TargetDataLine], audioFormat)
    val recorder = AudioSystem.getLine(recorderInfo).asInstanceOf[TargetDataLine]
    
    val numSamples = round((stop-start)*sampleRate).toInt
    
    val recorderBufferSize = 3*ceil((1+recorderTail)*numSamples*audioFormat.getFrameSize).toInt
    val recorderBuff = ByteBuffer.allocate(recorderBufferSize)
    
    recorder.open(audioFormat, recorderBufferSize)
    recorder.start
    recorder.flush
    recorder.read(recorderBuff.array, 0, recorderBufferSize)
    recorder.stop
    recorder.close
    
    val recordered = recorderBuff.asShortBuffer
    
    def y(t : Double) : Double = {
      //round to a sample
      val i = round(t * sampleRate).toInt
      if(i < 0 || i >= recorderBufferSize/2) return 0.0
      else return recordered.get(i)
    }
    
  }

  // Takes a function as the input and plays and records the signal into an Array Buffer
  def playRecord(f : Double => Double, start2 : Double, stop : Double, sampleRate : Float = 44100F, clipLevel : Double = 100.0): Array[Byte] = {
    
    val audioFormat = new AudioFormat(
      sampleRate, //sample rate
      16, //bits per sample (corresponds with Short)
      1, //number of channels, 1 for mono, 2 for stereo
      true, //true = signed, false is unsigned
      true //bigEndian
    )
    val info = new DataLine.Info(classOf[Clip], audioFormat) 
    val clip = AudioSystem.getLine(info).asInstanceOf[Clip] //cast required in java's sound API, at bit annoying
        
    val numSamples2 = scala.math.round((stop-start2)*sampleRate).toInt
    val buff = ByteBuffer.allocate(numSamples2*audioFormat.getFrameSize) //buffer for sound
    //buff.order(ByteOrder.LITTLE_ENDIAN)
    for( i <- 1 to numSamples2 ) {

      val v = scala.math.round(scala.Short.MaxValue/clipLevel*f(i/sampleRate + start2)).toShort
      buff.putShort(v);
    }
    
    val duration = (stop - start2).toInt
    val mic = AudioSystem.getTargetDataLine(audioFormat)

    val bufferSize:Int = audioFormat.getSampleRate().asInstanceOf[Int]*duration*2
    val buffer:Array[Byte] = new Array[Byte](bufferSize)

    mic.open()
    
    clip.open(audioFormat, buff.array, 0, numSamples2*audioFormat.getFrameSize)

    val start = System.currentTimeMillis()
    
    mic.start()
    clip.start
    mic.read(buffer,0,buffer.length)
    clip.drain
    clip.stop
    clip.close
    
    mic.stop()
    mic.close()
    buffer
  }
  
  //Takes a Byte array and plays it through the headphone port
  def playBuffer(buff: Array[Byte],sampleRate: Float = 44100F) {
    val format = new AudioFormat(sampleRate,16,1, true, true);
    val info2 = new DataLine.Info(classOf[Clip], format) 
    val clip2 = AudioSystem.getLine(info2).asInstanceOf[Clip]
    clip2.open(format, buff, 0, buff.length)
    clip2.start
    Thread.sleep(5)
    clip2.drain
    clip2.stop
    clip2.close
  }
  // Takes a Byte array, plays it through headphone port, while recording mic input

  def playBuffRec(buff:Array[Byte], sampleRate : Float = 44100F): Array[Byte] = { 
    val audioFormat = new AudioFormat(sampleRate, 16, 1, true, true)
    val info = new DataLine.Info(classOf[Clip], audioFormat) 
    val clip = AudioSystem.getLine(info).asInstanceOf[Clip]        
    val mic = AudioSystem.getTargetDataLine(audioFormat)
    val buffer:Array[Byte] = new Array[Byte](buff.length)  
    mic.open()
    clip.open(audioFormat, buff, 0, buff.length)
    val start = System.currentTimeMillis()  
    mic.start()
    clip.start
    mic.read(buffer,0,buffer.length)
    clip.drain
    clip.stop
    clip.close
    mic.stop()
    mic.close()
    buffer
  }
  
}
