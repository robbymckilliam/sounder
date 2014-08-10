/** Writes a empty wave file with given format */
package sounder

import javax.sound.sampled._
import java.io.File
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.RandomAccessFile

object FileIO {
  
  protected abstract class WavReader(filename : String, BUFFSIZE : Int = 4096) {
    val file = new File(filename)
    protected val in = AudioSystem.getAudioInputStream(file)
    protected val buffer = new BufferedInputStream(in,BUFFSIZE)
    if(in.getFormat.getSampleSizeInBits != Sounder.quantiserBits) throw new RuntimeException("Only 16bit wav supported.")
    val isStereo = in.getFormat.getChannels == 2
    val sampleRate = in.getFormat.getSampleRate
    //println("There are " + in.available + "bytes available in file " + filename)
    
    protected var isClosed = false
    /** Close the streams we have open */
    def close = if(!isClosed) { buffer.close; in.close; isClosed = true }
    
  }
  
    /** 
     *  An iterator to the samples in a wave file.  Returns stereo samples as a tuple (Double, Double)
     */
  class StereoWavReader(filename : String, BUFFSIZE : Int = 4096) extends WavReader(filename, BUFFSIZE) with Iterator[(Double, Double)] {

    override def hasNext : Boolean = {
      if(isClosed) false
      else if(buffer.available == 0) { close; false }
      else true
    }
    
    /** Returns next stereo sample.  If the wav file is mono left and right will be the same.  */
    override def next : (Double, Double)= {
      if(!hasNext) throw new RuntimeException("There are no more samples to read.")
      val samplebytes = ByteBuffer.allocate(in.getFormat.getFrameSize) //buffer for samples
      samplebytes.order(ByteOrder.LITTLE_ENDIAN)
      buffer.read(samplebytes.array)
      val left = samplebytes.getShort.toDouble/Sounder.quantiserscaler
      if(isStereo) return (left, samplebytes.getShort.toDouble/Sounder.quantiserscaler)
      else return (left, left)
    }
    
  }
  
  /** 
     *  An iterator to the samples in a wave file.  Returns mono samples as Double.  If this is a stereo
     *  wav file the average between left and right samples is given.
   */
  class MonoWavReader(filename : String, BUFFSIZE : Int = 4096) extends WavReader(filename, BUFFSIZE) with Iterator[Double] {
      
    override def hasNext : Boolean = {
      if(isClosed) false
      else if(buffer.available == 0) { close; false }
      else true
    }
    
    /** Returns nextsample.  If the wav file is stereo, returns the average of left and right samples.  */
    override def next : Double = {
      if(!hasNext) throw new RuntimeException("There are no more samples to read.")
      val samplebytes = ByteBuffer.allocate(in.getFormat.getFrameSize) //buffer for samples
      samplebytes.order(ByteOrder.LITTLE_ENDIAN)
      buffer.read(samplebytes.array)
      val left = samplebytes.getShort.toDouble/Sounder.quantiserscaler
      if(isStereo) return (left + samplebytes.getShort.toDouble/Sounder.quantiserscaler)/2.0
      else return left
    }
    
  }
  
  object WavWriter {
     /** Writes a empty wave file with given format */
    def writeEmptyWav(filename : String, audioFormat : AudioFormat) {
      val wavFile = new File(filename);
      val byteArrayIS = new ByteArrayInputStream(Array[Byte]())
      val audioIS = new AudioInputStream(byteArrayIS, audioFormat, 0)
      AudioSystem.write(audioIS, AudioFileFormat.Type.WAVE, wavFile)
    }
    /** Constructs a MonoWavWriter.  This is the same as "new MonoWaveWriter" */
    def mono(filename : String, sampleRate : Float = 44100F, BUFFSIZE : Int = 4096) = new MonoWavWriter(filename,sampleRate,BUFFSIZE)
    /** Constructs a StereoWavWriter.  This is the same as "new StereoWaveWriter" */
    def stereo(filename : String, sampleRate : Float = 44100F, BUFFSIZE : Int = 4096) = new StereoWavWriter(filename,sampleRate,BUFFSIZE)
    /** Writes mono samples contained in fs to a mono wav file called filename at sample rate sampleRate */
    def writeMonoSamples(fs : Seq[Double], filename : String, sampleRate : Float = 44100F){
      val writer = new MonoWavWriter(filename, sampleRate)
      fs.foreach( s => writer.put(s) )
      writer.close
    }
    /** Writes stereo samples contained in fleft and fright to a stereo wav file called filename at sample rate sampleRate */
    def writeStereoSamples(fleft : Seq[Double], fright : Seq[Double], filename : String, sampleRate : Float = 44100F){
      val writer = new StereoWavWriter(filename, sampleRate)
      if(fleft.size != fright.size) throw new ArrayIndexOutOfBoundsException("Number of left and right samples is not the same!")
      fleft.indices.foreach( i => writer.put(fleft(i),fright(i)) )
      writer.close
    }
  }
   
  protected abstract class WavWriter(filename : String, sampleRate : Float = 44100F, BUFFSIZE : Int = 4096) {
    val audioFormat = new AudioFormat(sampleRate,Sounder.quantiserBits,numChannels,true,true)
    WavWriter.writeEmptyWav(filename,audioFormat)
    val fo = new FileOutputStream(filename,true) //open file for appending data
    val buffer = new BufferedOutputStream(fo,BUFFSIZE)
    
    def close = {
      buffer.flush
      buffer.close
      fo.close
      modifyheaderlength //correct length in wav header
    }
    
    //We used java sound to write the header and this has the wrong length.  This corrects it.
    protected def modifyheaderlength = {
      val ro = new RandomAccessFile(filename,"rw") //open new wave for random access
      ro.skipBytes(40) //skip to were length information is (see wave structure at https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
      val bytebuff = ByteBuffer.allocate(4)
      bytebuff.order(ByteOrder.LITTLE_ENDIAN)
      bytebuff.putInt(numSamples*numChannels*Sounder.quantiserBits/8)
      ro.write(bytebuff.array) //write the actual size
      ro.close
    }
    
    /** The number of audio channels, 2 for stereo */
    def numChannels : Int
    
    /** The number of samples written so far */
    def numSamples : Int 
    
  }
  
  /** Class for writing mono wav files */
  class MonoWavWriter(filename : String, sampleRate : Float = 44100F, BUFFSIZE : Int = 4096) extends WavWriter(filename,sampleRate,BUFFSIZE){
    override def numChannels = 1
    protected var samplecount = 0
    override def numSamples =  samplecount
    /** Put a mono sample.  If this is a stereo wavfile left and right will be the same. */
    def put(s : Double) = {
      val samplebytes = ByteBuffer.allocate(audioFormat.getFrameSize) //buffer for samples
      samplebytes.order(ByteOrder.LITTLE_ENDIAN)
      val v = scala.math.round(Sounder.quantiserscaler*s).toShort
      samplebytes.putShort(v);
      buffer.write(samplebytes.array)
      samplecount = samplecount + 1 //incremement the number of samples written
    }
  }
  
  /** Class for writing stereo wav files */
  class StereoWavWriter(filename : String, sampleRate : Float = 44100F, BUFFSIZE : Int = 4096) extends WavWriter(filename,sampleRate,BUFFSIZE){
    override def numChannels = 2
    protected var samplecount = 0
    override def numSamples =  samplecount
    /** Put a stereo sample (left,right) */
    def put(s : (Double, Double)) = {
      val samplebytes = ByteBuffer.allocate(audioFormat.getFrameSize) //buffer for samples
      samplebytes.order(ByteOrder.LITTLE_ENDIAN)
      val left = scala.math.round(Sounder.quantiserscaler*s._1).toShort
      val right = scala.math.round(Sounder.quantiserscaler*s._2).toShort
      samplebytes.putShort(left);
      samplebytes.putShort(right);
      buffer.write(samplebytes.array)
      samplecount = samplecount + 1 //incremement the number of samples written
    }
  }
  
 }
