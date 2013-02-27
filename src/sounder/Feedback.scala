/*
 * author: @ThomasStratfold
 * Adder, multiply and loop all Loops the buffer, adding the input to the output after each loop
 * Feedback continues adds the input while still playing the signal
 */

package sounder

import sounder.Sounder._
import javax.sound.sampled._ 
import scala.Short
import java.io._
import java.nio._
import scala.collection.mutable._

object Feedback {

  // Method continuously adds in feedback
  def Feedback(f: Double => Double, t: Double,Size: Int = 2200, sampleRate: Float = 44100F, clipLevel: Double = 100.0): Array[Byte] = {
    val Ibuff = ByteBuffer.allocate((sampleRate*t*2).toInt)
    for(i <- 0 until (sampleRate*t).toInt) {
      val v = scala.math.round(scala.Short.MaxValue/clipLevel*f(i/sampleRate)).toShort
      Ibuff.putShort(v); 
    }
    val buffer = Ibuff.array
    
    val audioFormat = new AudioFormat(44100F, 16, 1, true, true)
    val info = new DataLine.Info(classOf[SourceDataLine],audioFormat)
    val soundLine = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
    val bufferSize = Size
    
    val mic = AudioSystem.getTargetDataLine(audioFormat)
  //  val bufferSize2:Int = (audioFormat.getSampleRate()*t*2).toInt
    val Mbuffer:Array[Byte] = new Array[Byte](bufferSize)
        
    mic.open()
    soundLine.open(audioFormat, bufferSize);
    soundLine.start()
    mic.start()
    //val Buffer = ByteBuffer.allocate((t*sampleRate*2).toInt)
    val threshold = (audioFormat.getFrameRate()).toInt;
    var start = 0
    var end = bufferSize
    val buff2 = new Array[Byte]((2*t*sampleRate).toInt)
    for(k <- 0 until (2*t*sampleRate/(bufferSize)).toInt) {
    //  println
     // println("1"+k)     
      val buff = new Array[Byte](bufferSize)
      var m = 0
      for(i <- start until end) {
      //  print(i)
        buff(m) = (buffer(i)+Mbuffer(m)).toByte
        buff2(i) = Mbuffer(m)
        m+=1
        
      }
      start = end
      end += bufferSize
      soundLine.write(buff, 0, bufferSize);
      mic.read(Mbuffer,0,Mbuffer.length)
     // Buffer.put(buff.array)
    }
    mic.stop()
    mic.close()
    soundLine.close()
    buff2
  //  Buffer.array
  }
  
  // Adds a multiple of the output back with the orignal input; default is multiply by 1
  def adder(inBuff: Array[Byte], outBuff: Array[Byte], modu: Double = 1): Array[Byte] = {
    val buff = new Array[Byte](scala.math.min(inBuff.length, outBuff.length))
    for(i <- 0 until buff.length) {
      buff(i) = (inBuff(i)+modu*outBuff(i)).toByte
    }
    buff
  }
  // Multiplies the two signals together with the feedback modulated to a default of 1
  def multiply(inBuff: Array[Byte], outBuff: Array[Byte], modu: Double = 1): Array[Byte] = {
    val buff = new Array[Byte](scala.math.min(inBuff.length, outBuff.length))
    for(i <- 0 until buff.length) {
      buff(i) = (inBuff(i)*(modu*outBuff(i))).toByte
    }
    buff
  }
  // Loops through performing the feedback on the buffered signal; default adds and multiplies by 1
  // InBuff is the original input signal, outBuff is the feedback from the output
  def loop(inBuff: Array[Byte],outBuff: Array[Byte], num: Int, modu: Double = 1, str: String = "adder"): Array[Byte] = {
    val temp = inBuff
    var temp2 = outBuff
    for(i <- 1 to num) {
      str match {

        case "adder" => temp2 = playBuffRec(adder(temp,temp2,modu)) //creates new feedback using previous feedback and original signal
        case "multiply" => temp2 = playBuffRec(multiply(temp,temp2,modu))
        case _ => println("Invalid feedback method, Current Methods include: adder, multiply")
      }
    }
    temp2  
  }
}
