/*
 * Sounder.
 * 
 * @authors Robby McKilliam and Thomas Stratfold
 */

package sounder

import javax.sound.sampled._ 
import scala.Short
import java.io._
import java.nio._

object Sounder {
  
  /// clipLevel specfies the maximum magnitude f can attain before being clipped (wrapped).
  val clipLevel : Double = 1.0 
  /// Number of bit used for quantising sampled audio 
  val quantiserBits : Int = 16
  /// scale from input output values to quantiser values, i.e. betwen minimum and maximum short (16bit) 
  val quantiserscaler = scala.Short.MaxValue/clipLevel 
  
  /** 
   * Plays the function f from time start to time stop out of the speakers.  Optional aguments are 
   * sampleRate (default 44100Hz i.e. CD quality)
   */
  def play(f : Double => Double, start : Double, stop : Double, sampleRate : Float = 44100F) {
    val Ts = 1/sampleRate //sample period
    val fs = (start to stop by Ts).map(t=>f(t)) //sequence of sample to play
    playSamples(fs,sampleRate)
  }
  
  /** 
   * Plays sequence of samples f out of the speakers.  An optional agument is
   * sampleRate (default 44100Hz i.e. CD quality).
   */
  def playSamples(f : Seq[Double], sampleRate : Float = 44100F) {
    val clip = constructClipFromSamples(f, sampleRate)
    playClip(clip)
  }
  
  /** Starts a clip, waits for it to finish playing (drains), stops it, then closes it */
  def playClip(clip : Clip) {
    clip.start
    Thread.sleep(5)
    clip.drain
    clip.stop
    clip.close  
  }
   
  /** Starts a clip looping loops times, waits for it to finish playing all the loops (drains), stops it, then closes it */
  def loopClip(clip : Clip, loops : Int) {
    clip.loop(loops)
    Thread.sleep(5)
    clip.drain
    clip.stop
    clip.close  
  }
  
  /**
   * Construct a java.sound Clip containing specified samples ready to play a the specified
   * rate.
   */
  def constructClipFromSamples(f : Seq[Double], sampleRate : Float = 44100F) : Clip = {
      val audioFormat = new AudioFormat(
      sampleRate, //sample rate
      quantiserBits, //bits per sample (corresponds with Short)
      1, //number of channels, 1 for mono, 2 for stereo
      true, //true = signed, false is unsigned
      true //bigEndian
    )
    val info = new DataLine.Info(classOf[Clip], audioFormat) 
    val clip = AudioSystem.getLine(info).asInstanceOf[Clip] //cast required in java's sound API, at bit annoying
       
    val numSamples = f.length
    val buff = ByteBuffer.allocate(numSamples*audioFormat.getFrameSize) //buffer for sound
    //buff.order(ByteOrder.LITTLE_ENDIAN)
    f.foreach{ y =>
      //quantise to a short.  This clips (wraps) if the function is larger than 1
      val v = scala.math.round(quantiserscaler*y).toShort
      buff.putShort(v);
    }     
    clip.open(audioFormat, buff.array, 0, numSamples*audioFormat.getFrameSize)
    return clip
  }
  
  /**
   * Constructs a java.sound.Clip ready to play the function f from start to stop at the specified
   * sample rate.
   */
  def constructClip(f : Double => Double, start : Double, stop : Double, sampleRate : Float = 44100F) : Clip = {
    val Ts = 1/sampleRate //sample period
    val fs = (start to stop by Ts).map(t=>f(t)) //sequence of sample to play
    return constructClipFromSamples(fs, sampleRate)
  }

  /** 
   * Takes a function as the input and plays and records the signal.  Returns the two 
   * sequences of doubles representing the left and right (stereo) signals.
  */
  def playRecord(f : Double => Double, start : Double, stop : Double, sampleRate : Float = 44100F): (Seq[Double], Seq[Double]) = {
    
    val playerFormat = new AudioFormat(sampleRate,quantiserBits,1, true, true);
    val recorderFormat = new AudioFormat(sampleRate,quantiserBits,2, true, true);
    val info = new DataLine.Info(classOf[Clip], playerFormat) 
    val clip = AudioSystem.getLine(info).asInstanceOf[Clip] //cast required in java's sound API, at bit annoying
    
    
    val numSamples = scala.math.round((stop-start)*sampleRate).toInt
    val playbuffer = ByteBuffer.allocate(numSamples*playerFormat.getFrameSize) //buffer for player samples
    //buff.order(ByteOrder.LITTLE_ENDIAN)
    for( i <- 1 to numSamples ) {
      val v = scala.math.round(quantiserscaler*f(i/sampleRate + start)).toShort
      playbuffer.putShort(v);
    }
    
    val duration = (stop - start).toInt
    val mic = AudioSystem.getTargetDataLine(recorderFormat)

    val recorderBufferSize:Int = recorderFormat.getFrameSize*duration*sampleRate.toInt
    val recordbuffer = ByteBuffer.allocate(recorderBufferSize) //buffer recorded sample

    mic.open()
    clip.open(playerFormat, playbuffer.array, 0, numSamples*playerFormat.getFrameSize)
    mic.start()
    clip.start
    mic.read(recordbuffer.array,0,recorderBufferSize)
    clip.drain
    clip.stop
    clip.close
    mic.stop()
    mic.close()
    
    //map the bytes to Doubles
    val shorts = recordbuffer.asShortBuffer
    val right = (0 until recorderBufferSize/2 by 2) map ( i => shorts.get(i).toDouble/quantiserscaler )
    val left = (1 until recorderBufferSize/2 by 2) map ( i => shorts.get(i).toDouble/quantiserscaler )
    
    return (left, right)
  }
  
  /// Takes a Byte array and plays it through the headphone port
  def playBuffer(buff: Array[Byte], sampleRate: Float = 44100F) {
    val format = new AudioFormat(sampleRate,16,1, true, true);
    val info = new DataLine.Info(classOf[Clip], format) 
    val clip = AudioSystem.getLine(info).asInstanceOf[Clip]
    clip.open(format, buff, 0, buff.length)
    clip.start
    Thread.sleep(5)
    clip.drain
    clip.stop
    clip.close
  }
 
}
