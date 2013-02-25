/*
 * author: @ThomasStratfold
 * Loops the buffer, adding the input to the output and continues playing the signal
 */

package sounder

import sounder.Sounder._

object Feedback {

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
