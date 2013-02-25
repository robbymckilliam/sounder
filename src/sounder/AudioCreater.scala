package sounder

import javax.sound.sampled._
import java.io._
import java.nio._
import scala.collection.mutable._

object AudioCreater {
  
  def wavCreater(Buff: Array[Byte],fileName: String = "Temp", sampleRate: Float = 44100F) {
    val audioFormat = new AudioFormat(sampleRate,16,1,true,true)
    val byteArrayIS = new ByteArrayInputStream(Buff)
    val audioIS = new AudioInputStream(byteArrayIS, audioFormat, Buff.length/2)
    try {
      AudioSystem.write(audioIS, AudioFileFormat.Type.WAVE, new File(fileName))
    } catch {
      case e: Exception => e.printStackTrace()
    }
    
  }
  
  def wavConverter(file: File): Array[Byte] = {
    val out = new ByteArrayOutputStream() ;
    val in = AudioSystem.getAudioInputStream(file);
           
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
