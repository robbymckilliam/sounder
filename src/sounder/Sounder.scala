/*
 * Sounder.
 * 
 * @author Robby McKilliam
 */

package sounder

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.ShortBuffer
import java.nio.ByteOrder
import javax.sound.sampled._;
import scala.math.floor
import scala.math.ceil
import scala.math.round
import scala.Short

object Sounder {
  
  /** 
   * Plays the function f from time start to time stop out of the speakers.  Optional aguments are 
   * sampleRate (default 44100Hz i.e. CD quality) and clipLevel (default 100) which specfies the
   * maximum magnitude f can attain before being clipped (wrapped).
   */
  def play(f : Double => Double, start : Double, stop : Double, sampleRate : Float = 44100F, clipLevel : Double = 100.0) {
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
    clip.drain
    clip.stop
    clip.close
  }
  
}

/** 
 * Class which sounds a channel.  Sends the function x out the soundcard and records the
 * response, y.  x is played from time zero to duration seconds.
 * Optional aguments are sampleRate (default 44100Hz i.e. CD quality) and clipLevel (default 100) 
 * which specfies the maximum magnitude f can attain before being clipped (wrapped).
 * recorderTail is the proportion of time by which the recorder runs longer than the player (default is 0.05, i.e. 5% longer)
 */
class Sounder(x : Double => Double, duration : Double, sampleRate : Float = 44100F, clipLevel : Double = 100.0, recorderTail : Double = 0.05) {
  
  val audioFormat = new AudioFormat(
    sampleRate, //sample rate
    16, //bits per sample (corresponds with Short)
    1, //number of channels, 1 for mono, 2 for stereo
    true, //true = signed, false is unsigned
    true //bigEndian
  )
  val playerinfo = new DataLine.Info(classOf[Clip], audioFormat) 
  val player = AudioSystem.getLine(playerinfo).asInstanceOf[Clip] 
  val recorderinfo = new DataLine.Info(classOf[TargetDataLine], audioFormat) 
  val recorder = AudioSystem.getLine(recorderinfo).asInstanceOf[TargetDataLine] 
  
  //construct buffer for player, fill it with samples taken from x
  val numSamples = round(duration*sampleRate).toInt
  val playBuff = ByteBuffer.allocate(numSamples*audioFormat.getFrameSize) //buffer for sound
  //buff.order(ByteOrder.LITTLE_ENDIAN)
  for( i <- 1 to numSamples ) {
    //quantise to a short.  This clips (wraps) if the function is larger than 1
    val v = round(Short.MaxValue/clipLevel*x(i/sampleRate)).toShort
    playBuff.putShort(v);
  }
  val recoderBufferSize = ceil((1+recorderTail)*numSamples*audioFormat.getFrameSize).toInt //recorder buffer goes for recorderTail% longer
  val recorderBuff = ByteBuffer.allocate(recoderBufferSize) //buffer for recording
  recorder.open(audioFormat, recoderBufferSize)
  player.open(audioFormat, playBuff.array, 0, numSamples*audioFormat.getFrameSize) //load the player
  
  recorder.start
  player.start
  player.drain
  player.stop
  
  recorder.read(recorderBuff.array,0,recoderBufferSize)
  recorder.stop
  
  recorder.close
  player.close
  
  val recodered = recorderBuff.asShortBuffer
  
  /** Simple zero order hold output function */
  def y(t : Double) : Double = {
    //round to a sample
    val i = round(t * sampleRate).toInt
    if(i < 0 || i >= recoderBufferSize/2) return 0.0
    else return recodered.get(i)
  }
  
}