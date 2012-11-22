/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package sounder;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import javax.sound.sampled.*;

/**
 *
 * @author Robby McKilliam
 */
public class Sounder {

    public Sounder() {
    }
    
    public void out(double freq, double vol){
        float sampleRate = 44100.0F;
        AudioFormat audioFormat = new AudioFormat(
                sampleRate, //sample rate
                16, //bits per sample
                1, //number of channels, 1 for mono, 2 for stereo
                true, //true = signed, false is unsigned
                false //littleEndian
                );
        DataLine.Info info = new DataLine.Info(SourceDataLine.class, audioFormat);
        SourceDataLine sourceDataLine = null;
        try {
            sourceDataLine = (SourceDataLine) AudioSystem.getLine(info);
            sourceDataLine.open(audioFormat);
        } catch (LineUnavailableException e) {
            System.out.println("unable to get an audio output line");
            System.exit(1);
        }
        
        int N = (int)(5 * sampleRate); //five seconds worth of samples
        ByteBuffer buff = ByteBuffer.allocate(N * audioFormat.getFrameSize());
        buff.order(ByteOrder.LITTLE_ENDIAN);
        for (int i = 0; i < N; i++) {
            short v =  (short) Math.round(vol * Math.sin(freq/sampleRate * i));
            buff.putShort(v);
        }
        sourceDataLine.start();
        sourceDataLine.write(buff.array(), 0, N * audioFormat.getFrameSize());
        sourceDataLine.stop();

        
    }

    public void in() {
        AudioFormat audioFormat = new AudioFormat(
                44100.0F, //sample rate
                8, //bits per sample
                1, //number of channels, 1 for mono, 2 for stereo
                true, //true = signed, false is unsigned
                false //littleEndian
                );
        System.out.println(audioFormat.getFrameSize());

        DataLine.Info info = new DataLine.Info(TargetDataLine.class, audioFormat);
        TargetDataLine targetDataLine = null;
        try {
            targetDataLine = (TargetDataLine) AudioSystem.getLine(info);
            targetDataLine.open(audioFormat);
        } catch (LineUnavailableException e) {
            System.out.println("unable to get a recording line");
            System.exit(1);
        }

        System.out.println("Press ENTER to start the recording.");
        try {
            System.in.read();
        } catch (IOException e) {
        }

        byte[] b = new byte[100 * audioFormat.getFrameSize()];
        targetDataLine.start();
        targetDataLine.read(b, 0, b.length);
        for (int i = 0; i < b.length; i++) {
            System.out.println(b[i]);
        }
        targetDataLine.stop();

        /*
         * And now, we are waiting again for the user to press ENTER, this time
         * to signal that the recording should be stopped.
         */
        System.out.println("Press ENTER to stop the recording.");
        try {
            System.in.read();
        } catch (IOException e) {
        }
    }
    
}
