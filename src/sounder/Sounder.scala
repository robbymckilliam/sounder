/*
 * Sounder.
 * 
 * @author Robby McKilliam
 */

package sounder

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.sound.sampled._;

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
                false //littleEndian
                );
        val info = new DataLine.Info(classOf[SourceDataLine], audioFormat) 
        val sourceDataLine = AudioSystem.getLine(info).asInstanceOf[SourceDataLine] //cast required in java's sound API, at bit annoying
        
        val duration = stop - start
        val numSamples = scala.math.round(duration*sampleRate).toInt
        val buff = ByteBuffer.allocate(numSamples*audioFormat.getFrameSize) //buffer for sound
        buff.order(ByteOrder.LITTLE_ENDIAN)
        for( i <- 1 to numSamples ) {
          //quantise to a short.  This clips (wraps) if the function is larger than 1
          val v : Short = scala.math.round(scala.Short.MaxValue/clipLevel*f(i/sampleRate + start)).toShort
          buff.putShort(v);
        }
      
        sourceDataLine.open(audioFormat)
        sourceDataLine.start
        sourceDataLine.write(buff.array, 0, numSamples*audioFormat.getFrameSize)
        sourceDataLine.stop
        sourceDataLine.close
  }
  
}