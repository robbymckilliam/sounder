/**
 * Utilities for reading and writing audio to wav files
 * 
 * @authors Thomas Stratfold and Robby McKilliam
 * 
 */

package sounder

import javax.sound.sampled._
import java.io.File
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder

object FileIO {
  
    /** 
     *  An iterator to the samples in a wave file.  Buffers file reads for efficiency
     */
  class WaveReader(filename : String, BUFFSIZE : Int = 256) extends Iterator[(Double, Double)] {
    val file = new File(filename)
    protected val in = AudioSystem.getAudioInputStream(file);
    if(in.getFormat.getSampleSizeInBits != Sounder.quantiserBits) throw new RuntimeException("Only 16bit wav supported.")
    val isStereo = in.getFormat.getChannels == 2
    val sampleRate = in.getFormat.getSampleRate
    protected val buffer = ByteBuffer.allocate(in.getFormat.getFrameSize * BUFFSIZE) //buffer for samples
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    readToBuffer
    
    protected def readToBuffer = {
      buffer.clear
      val r = in.read(buffer.array)
      buffer.limit(r) //set the buffer limit to the number of bytes just read
    }
    
    override def hasNext : Boolean = {
      if(buffer.remaining == 0 && in.available > 0) readToBuffer
      if(buffer.remaining != 0) return true
      in.close //close the audio system if there are no more samples
      return false
    }
    
    /** Returns next stereo sample.  If the wav file is mono left and right will be the same.  */
    override def next : (Double, Double)= {
      if(!hasNext) throw new RuntimeException("There are no more samples to read.")
      val left = buffer.getShort.toDouble/Sounder.quantiserscaler
      if(isStereo) return (left, buffer.getShort.toDouble/Sounder.quantiserscaler)
      else return (left, left)
    }
    
  }
  
//  /** 
//     *  Allows efficiently writing to a wav file one sample at a time.  Buffers for
//     */
//  class WavWriter(filename : String, BUFFSIZE : Int = 256) {
//    
//  }
//  
//  def waveWriter(Buff: Array[Byte],fileName: String = "Temp", sampleRate: Float = 44100F) {
//    val audioFormat = new AudioFormat(sampleRate,16,1,true,true)
//    val byteArrayIS = new ByteArrayInputStream(Buff)
//    val audioIS = new AudioInputStream(byteArrayIS, audioFormat, Buff.length/2)
//    try {
//      AudioSystem.write(audioIS, AudioFileFormat.Type.WAVE, new File(fileName))
//    } catch {
//      case e: Exception => e.printStackTrace()
//    }
//  }
  
  /** Write function f from time start to time stop to mono wavefile */
  def waveWriter(f : Seq[Double], filename : String, sampleRate : Float = 44100F) {
    val audioFormat = new AudioFormat(sampleRate,16,1,true,true)
    val wavFile = new File(filename);
    val numSamples = f.size
    val buffer = ByteBuffer.allocate(audioFormat.getFrameSize * numSamples) //buffer for samples
    f.foreach{ y =>
      //quantise to a short.  This clips (wraps) if the function is larger than 1
      val v = scala.math.round(Sounder.quantiserscaler*y).toShort
      buffer.putShort(v);
    }
    val byteArrayIS = new ByteArrayInputStream(buffer.array)
    val audioIS = new AudioInputStream(byteArrayIS, audioFormat, numSamples*audioFormat.getFrameSize)
    try {
      AudioSystem.write(audioIS, AudioFileFormat.Type.WAVE, wavFile)
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
  
  def waveReader(file: File): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val in = AudioSystem.getAudioInputStream(file)
           
    val buff = new Array[Byte](in.available())
    var read = 0
    while ({ read = in.read(buff); read > 0} ) {
      out.write(buff, 0, read);
    }
    out.flush();
    val Fbuff = out.toByteArray();
    var temp = new Array[Byte](Fbuff.length)
    var j = 1
    var i = 0
    for(k<- 0 until (Fbuff.length)/2-1) {
      temp(i) = Fbuff(j)
      temp(i+1) = Fbuff(j-1)
      i+=2
      j+=2
    }
    temp
  }
  
 }
