/*
 * author: @ThomasStratfold
 * Loops the buffer, adding the input to the output and continues playing the signal
 */

package sounder

import sounder.Sounder._

object Feedback {

  // Adds a multiple of the output back with the orignal input; default is multiply by 1
  def adder(inBuff: Array[Byte], outBuff: Array[Byte], mult: Double = 1): Array[Byte] = {
    val buff = new Array[Byte](scala.math.min(inBuff.length, outBuff.length))
    for(i <- 0 until buff.length) {
      buff(i) = (inBuff(i)+mult*outBuff(i)).toByte
    }
    buff
  }
  def multiply(inBuff: Array[Byte], outBuff: Array[Byte], modu: Double = 1): Array[Byte] = {
    val buff = new Array[Byte](scala.math.min(inBuff.length, outBuff.length))
    for(i <- 0 until buff.length) {
      buff(i) = (inBuff(i)*(modu*outBuff(i))).toByte
    }
    buff
  }
  // Loops through performing the feedback on the buffered signal; default adds and multiplies by 1
  def loop(inBuff: Array[Byte],outBuff: Array[Byte], num: Int, mult: Double = 1, str: String = "adder"): Array[Byte] = {
    val temp = inBuff
    var temp2 = outBuff
    for(i <- 1 to num) {
      str match {
        case "adder" => temp2 = adder(temp,temp2,mult)
        case _ => println("Invalid feedback method, Current Methods include: adder")
      }
    //  playBuff(temp2)
    }
    temp2
    
  }
}
