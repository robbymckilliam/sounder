sounder
=======

To run this GUI, download the .jar file and use the command 'scala -classpath Sounder.jar sounder.PlayApp' to launch the GUI.

Alternatively the playing and recording functions can be accessed through the Sounder.scala file.

Files contained:
 - AudioCreater.scala -> Contains two methods that will convert a Byte Array to a Wav file and vice versa.
 - Converter.scala -> Contains functions used by PlayApp to call methods from Sounder.
 - Feedback.scala -> Contains Feedback method, that will continously add input from mic to the output, and other methods that add via a loop of audio.
 - HamWind.scala -> Contains a simple Hamming Window function creater
 - PlayApp.scala -> Contains the set up for a GUI to Play, Record, Load and Save sinusoidal audio files
 - Sounder.scala -> Contains the methods to Play and Record sinusoidal functions
