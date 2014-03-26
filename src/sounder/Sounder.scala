/*
 * Sounder.
 * 
 * @authors Robby McKilliam and Thomas Stratfold
 */

package sounder

import javax.sound.sampled._ 
import scala.math.ceil 
import scala.math.round 
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
   * Retuns a java sound AudioFormat with a given sample rate and number of channels, 1 for 
   * mono and 2 for stereo.
   */
  def javaSoundAudioFormat(sampleRate : Float, channels : Int) = new AudioFormat(
      sampleRate, //sample rate
      quantiserBits, //bits per sample (corresponds with Short)
      channels, //number of channels, 1 for mono, 2 for stereo
      true, //true = signed, false is unsigned
      true//bigEndian
      )
  def monoFormat(sampleRate : Float) = javaSoundAudioFormat(sampleRate, 1)
  def stereoFormat(sampleRate : Float) = javaSoundAudioFormat(sampleRate, 2)
    
  /** 
   * Plays the function f from time start to time stop out of the speakers.  Optional aguments are 
   * sampleRate (default 44100Hz i.e. CD quality)
   */
  def play(f : Double => Double, start : Double, stop : Double, sampleRate : Float = 44100F) {
    val clip = constructClip(f, start, stop, sampleRate)
    playClip(clip)
  }
  
  /** 
   * Plays functions fleft and fright out of left and right speakers from time start to time stop.  
   * Optional aguments are sampleRate (default 44100Hz i.e. CD quality)
   */
  def playStereo(fleft : Double => Double, fright : Double => Double, start : Double, stop : Double, sampleRate : Float = 44100F) {
    val clip = constructClipStereo(fleft, fright, start, stop, sampleRate)
    playClip(clip)
  }
  
  /** 
   * Plays sequence of sample f out of the speakers.  Optional aguments are 
   * sampleRate (default 44100Hz i.e. CD quality) and clipLevel (default 100) which specfies the
   * maximum magnitude f can attain before being clipped (wrapped).
   */
  def playSamples(f : Seq[Double], sampleRate : Float = 44100F) {
    val clip = constructClipFromSamples(f, sampleRate)
    playClip(clip)
  }
  
  /** 
   * Plays sequences of stereo samples fleft and fright out of the left and right speakers.  
   * Optional aguments are sampleRate (default 44100Hz i.e. CD quality) and clipLevel (default 100) 
   * which specfies the maximum magnitude f can attain before being clipped (wrapped).
   */
  def playStereoSamples(fleft : Seq[Double], fright : Seq[Double], sampleRate : Float = 44100F) {
    val clip = constructClipFromStereoSamples(fleft, fright, sampleRate)
    playClip(clip)
  }
  
  /** 
   * Takes a function as the input and plays and records the signal.  Returns the two 
   * sequences of doubles representing the left and right (stereo) signals recorded.
   */
  def playRecord(f : Double => Double, start : Double, stop : Double, sampleRate : Float = 44100F): (Seq[Double], Seq[Double]) = {
    val clip = constructClip(f, start, stop, sampleRate)
    return playRecordFromClip(clip)
  }
  
  /** 
   * Plays functions out the left and right and records the simultaneously.  Returns the two 
   * sequences of doubles representing the left and right (stereo) signals recorded.
   */
  def playStereoRecord(fleft : Double => Double, fright : Double => Double, start : Double, stop : Double, sampleRate : Float = 44100F): (Seq[Double], Seq[Double]) = {
    val clip = constructClipStereo(fleft, fright, start, stop, sampleRate)
    return playRecordFromClip(clip)
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
   * Construct a mono java.sound Clip containing specified samples ready to play a the specified
   * rate.
   */
  def constructClipFromSamples(f : Seq[Double], sampleRate : Float) : Clip = {
    val audioFormat = monoFormat(sampleRate)
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
   * Construct a stero java.sound Clip containing specified samples ready to play a the specified
   * rate.
   */
  def constructClipFromStereoSamples(fleft : Seq[Double], fright : Seq[Double], sampleRate : Float) : Clip = {
    val audioFormat = stereoFormat(sampleRate)
    val info = new DataLine.Info(classOf[Clip], audioFormat) 
    val clip = AudioSystem.getLine(info).asInstanceOf[Clip] //cast required in java's sound API, at bit annoying
   
    if(fleft.length != fright.length) throw new ArrayIndexOutOfBoundsException("Number of samples on left and right must be the same.")
    val numSamples = fleft.length
    val buff = ByteBuffer.allocate(2*numSamples*audioFormat.getFrameSize) //buffer for sound
    //buff.order(ByteOrder.LITTLE_ENDIAN)
    (fleft, fright).zipped.foreach{ (left, right)=>
      buff.putShort(scala.math.round(quantiserscaler*left).toShort);
      buff.putShort(scala.math.round(quantiserscaler*right).toShort);
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
   * Constructs a java.sound.Clip ready to play the function f from start to stop at the specified
   * sample rate.
   */
  def constructClipStereo(fleft : Double => Double, fright : Double => Double, start : Double, stop : Double, sampleRate : Float = 44100F) : Clip = {
    val Ts = 1/sampleRate //sample period
    val fl = (start to stop by Ts).map(t=>fleft(t)) //sequence of sample to play on left speaker
    val fr = (start to stop by Ts).map(t=>fright(t)) //sequence of sample to play on right speaker
    return constructClipFromStereoSamples(fl, fr, sampleRate)
  }
  
  /** 
   * Takes a function as the input and plays and records the signal.  Returns the two 
   * sequences of doubles representing the left and right (stereo) signals.
  */
  def playRecordFromClip(clip : Clip) : (Seq[Double], Seq[Double]) = {
    val sampleRate = clip.getFormat.getSampleRate
    val recorderFormat = stereoFormat(sampleRate);
    
    val mic = AudioSystem.getTargetDataLine(recorderFormat)
    val recorderBufferSize : Int = recorderFormat.getFrameSize*clip.getFrameLength
    val recordbuffer = ByteBuffer.allocate(recorderBufferSize) //buffer recorded sample

    mic.open()
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
  
 
}
