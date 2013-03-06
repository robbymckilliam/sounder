/*
 * Thomas Stratfold Jan 2013
 * A simple GUI which should act the same as the command line
 * allowing the user to call any function they chose to be played by the play
 * function
 */
package sounder

import scala.math.abs
import scala.swing._
import scala.swing.event._
import scala.collection.mutable._
import java.io._
import sounder.Converter.toPlay
import sounder.Converter.toPlayRecord
import sounder.Sounder.playBuff
import java.util.Scanner

object PlayApp extends SimpleSwingApplication {
  //Creates a simple box layout for the GUI
  var fBuff = new Array[Byte](0)

  import ComboBox._
  val ui = new FlowPanel {
    val label = new Label("Construct the function to be played")
    object Amplitude extends TextField { columns = 5}
    val label1a = new Label("*")
    val Function = new ComboBox(List("sin", "cos", "tan")) { makeEditable() }
    Function.preferredSize = new Dimension(60,25)
    val label2 = new Label("(2*Pi*t*")
    object Frequency extends TextField { columns = 5}
    val label3 = new Label(")")
    val AddB = new Button("Add Function")
    val ClearB = new Button("Clear")
    val label4 = new Label("Type in playing time")
    object Time extends TextField { columns = 5}
    val PFunc = new TextField(40) { editable = false}
    val PlayB = new Button("Play")
    val PRecB = new Button("Play and Record")
    val PlBaB = new Button("Playback Recording")
    
    // Adds each part of the GUI to the layout
    contents += label       // Describes requirements
    contents += Amplitude   
    contents += label1a
    contents += Function
    contents += label2
    contents += Frequency
    contents += label3
    contents += AddB       // Adds functinon to texfield
    contents += label4
    contents += Time
    contents += PFunc
    contents += PlayB
    contents += PRecB
    contents += PlBaB
    contents += ClearB    

    border = Swing.EmptyBorder(20, 50, 20, 50)
    
    // Inital values for the function
    var f = 100.0   // frequency
    var a = 1.0     // amplitude
    var x = 0.0     // time
    var f1 = "sin"  // type of function
    var ft = ""     // total function
    
    // List variables to be given to toPlay function
    var Af = List[String]()  // All Functions
    var Af2 = List[Double]() // All Frequencies
    var Aa = List[Double]()  // All Amplitudes
    var buff = new Array[Byte](0)
    
    //Reacts whenever a button or field is changed
    listenTo(Amplitude, Frequency, Function.selection, Time, PlayB, AddB, ClearB,PRecB,PlBaB) 
    reactions += {
      case ButtonClicked(AddB) => // Add button adds the function to the total
        ft = ft+"+"+a.toString+"*"+f1+"(2*Pi*t*"+f.toString+")"
        PFunc.text = ft
        val p = Af:::List(f1)
        Af = p
        val p2 = Af2:::List(f)          // Updates lists
        Af2 = p2
        val p3 = Aa:::List(a)
        Aa = p3
      case SelectionChanged(Function) =>    // Change in drop down list
        f1 = Function.selection.item.toString
      case EditDone(Amplitude) =>          // Altered amplitude
        a = Amplitude.text.toDouble
        if(abs(a) > 50) a = 50
      case EditDone(Frequency) =>         // Altered frequency
        f = Frequency.text.toDouble
        if(abs(f) > 20000) f = 20000
      case EditDone(Time)     =>          // Altered Time
        x = abs(Time.text.toDouble)
      case ButtonClicked(PlayB)   =>      // Pressed play
        toPlay(Af,Aa,Af2,x)             // Calls toPlay method to play function
        //play(t => (a)*(sin(f*2*Pi*t)),0,x)
        //inter(ft,x)
      case ButtonClicked(ClearB) =>       // Pressed clear
        ft = ""
        PFunc.text = "" 
        Af = List[String]()                // Sets all variables back to intial
        Af2 = List[Double]()
        Aa = List[Double]()
      case ButtonClicked(PRecB) =>
        buff = toPlayRecord(Af,Aa,Af2,x)
        fBuff = buff
      case ButtonClicked(PlBaB) =>
        playBuff(fBuff)
    }
    
  }
  ui.preferredSize = new Dimension(740,155)
  
  def top = new MainFrame {
    title = "Play Function App"
    contents = ui
    menuBar = new MenuBar {
      contents+= new Menu("File") {
        val chooser = new FileChooser
        contents += new MenuItem(Action("Open") {
           
            val temp = chooser.showOpenDialog(ui)
            if(temp == FileChooser.Result.Approve) {
              val file = chooser.selectedFile
              val length = file.getPath.length
              val name = file.getPath
              // Checks to see if file is a txt or a wav and saves accordingly
              if(name(length-3)+name(length-2).toString+name(length-1) == "wav") {
                fBuff = sounder.AudioCreater.wavConverter(file)
                
              } else if(name(length-3)+name(length-2).toString+name(length-1) == "txt") {            
                val reader = new FileReader(file)
                val src = new Scanner(reader)
                val arrBuff = new ArrayBuffer[Byte]()
                while(src.hasNext) {
                  arrBuff += src.next().toByte
                }
                fBuff = arrBuff.toArray
             }
            }          
        })
        contents += new MenuItem(Action("Save") { //val buttons = new Button(Action("Save") {
            val temp = chooser.showSaveDialog(ui)
            if(temp == FileChooser.Result.Approve) {
              val file = chooser.selectedFile
              
              val length = file.getPath.length
              val name = file.getPath
              // Checks to see if file is a txt or a wav and saves accordingly
              if(name(length-3)+name(length-2).toString+name(length-1) == "txt") {
                val filePrint = new PrintWriter(new File(file.getPath))
                var i = 0
                for(j<- 0 until fBuff.length/2) {
                  filePrint.write(fBuff(i).toString+"  "+fBuff(i+1).toString+"\n")
                  i+=2
                }
                filePrint.close()
              } else if(name(length-3)+name(length-2).toString+name(length-1) == "wav") {
                sounder.AudioCreater.wavCreater(fBuff, name, 44100F)
              }
            }
        })
      
      }
      contents+= new Menu("Help") {
        val buttons = new FlowPanel(){}
        buttons.minimumSize =(new Dimension(740,155))
        contents+= new MenuItem(Action("ReadMe") {
            import Dialog._
            showMessage(buttons, "This application is designed to play a function containing a collection of sinusoids created by the user.\n"
                        +"The first box corresponds with the amplitude of the sinusoid, with the value restricted to between -50 and 50 due to the limits of the output from the soundcard.\n"
                        +"The dropdown list allows for the selection of the sinusoid, either a sin, cos or tan function.\n"
                        +"The last box on the first line represents the frequecny of the sinusoid in Hz, restricted to values below 20,000Hz due to limits on human hearing and the soundcard.\n"
                        +"The first box on the second line corresponds to the time the function will be played for, restricted to positive values.\n"
                        +"The final clear box shows the construction of the function, and can contain up to 5 different sinusoid functions.\n"

                       /* +"This player will play any collection of sinusodial functions that you choose, all you need to do is enter the different values in the corresponding boxes."
                  +"\n"+"The first box is the amplitude, any value between -50 and 50, any higher or lower will be cut off, as this helps limit the output from the soundcard."
                  +"\n"+"The drop down list allows for the selection of the type of sinusoid"
                  +"\n"+"The next box is the frequency of the function, restricted to values below 20,000 Hz, due to restrictions to human hearing and the soundcard"
                  +"\n"+"The next box shows the function which has been created so far and what will be played"
                  +"\n"+"The final box will set how long the function will play for, restricted to postive times." */
                  +"\n"+"Press Add to add a function to be played  Press Play to hear the function  Press Play and Record to record the function  Press Playback to hear the recorded version  Press Clear to start fresh.\n"
                  +"\n"+"To Save the file select File/Save and then find the location you would like to save, to save as a txt file add the extention .txt to save as a wav add .wav\n"
                  +"To Load in a file select File/Open and find the location of the file, only txt files under 50000KB will load (3min of audio) or wav files that were created by this GUI", "Help")
        })
      }
    }
  }
}