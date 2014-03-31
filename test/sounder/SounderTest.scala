/*
 * @author Robby McKilliam
 */

package sounder

import org.junit.Test
import org.junit.Assert._
import sounder.Sounder._
import scala.math.sin
import scala.math.cos
import scala.math.Pi

class SounderTest {
  
  @Test
  def PlayRecordTest() {
    println("Testing playback and recording")
    val duration = 2
    println("You should hear a 150Hz tone play for " + duration + " seconds.") 
    val Fs = 44100
    val (left, right) = playRecord(t => 0.5*sin(2*Pi*150*t), 0, duration, Fs)
    println("Playing back the left channel") 
    playSamples(left)
    println("Playing back the right channel") 
    playSamples(right)
    println("Playing back stereo")
    playStereoSamples(left, right)
    val filet = new java.io.FileWriter("testdata.csv")
    (0 until left.length) foreach { i =>
      filet.write((i.toDouble/Fs).toString.replace('E', 'e') + "\t" + left(i).toString.replace('E', 'e')  + "\t" + right(i).toString.replace('E', 'e')  + "\n")
    }
    filet.close
  }
  
  @Test
  def PlayRecordStereoTest() {
    println("Testing stereo playback and recording")
    val duration = 2
    println("You should hear a 150Hz tone play on left and 300Hz tone on right for " + duration + " seconds.") 
    val Fs = 44100
    def fleft(t : Double) = 0.5*sin(2*Pi*150*t)
    def fright(t : Double) = 0.25*cos(2*Pi*300*t)
    val (left, right) = playStereoRecord(fleft, fright, 0, duration, Fs)
    println("Playing back the left channel") 
    playSamples(left)
    println("Playing back the right channel") 
    playSamples(right)
    println("Playing back stereo")
    playStereoSamples(left, right)
    val filet = new java.io.FileWriter("testdatastereo.csv")
    (0 until left.length) foreach { i =>
      filet.write((i.toDouble/Fs).toString.replace('E', 'e') + "\t" + left(i).toString.replace('E', 'e')  + "\t" + right(i).toString.replace('E', 'e')  + "\n")
    }
    filet.close
  }
  
  @Test def PlayTest() {
    println("Testing playback")
    val duration = 1
    val freqs = List(100,200,500,1000,2000)
    for( f <- freqs ) {
      println("You should hear a " + f + "Hz tone play for " + duration + " seconds.")
      play(t => 0.5*sin(2*Pi*f*t), 0.0, duration)
    }
  }
  
  @Test def PlayStereoTest() {
    println("Testing stereo playback")
    val duration = 1
    val freqs = List(100,200,500,1000,2000)
    for( f <- freqs ) {
      println("You should hear a " + f + "Hz tone play on left and " + (2*f) + "Hz on right for " + duration + " seconds.") 
      def fleft(t : Double) = 0.5*sin(2*Pi*f*t)
      def fright(t : Double) = 0.5*sin(2*Pi*2*f*t)
      playStereo(fleft, fright, 0.0, duration)
    }
  }
  
}
