
//  can you make a program that tracks how many times you've pressed the letter z
//  and after you reach 100 it makes every keyboard input z
//  :]
//      -MoltenPlastique

import java.awt.*;
import java.awt.event.*;

public class zCounter extends Frame implements KeyListener{

    int zcounter = 0;

    public static void main(String[] args) {
        //idfk :c
    }

    @Override
    public void keyPressed(KeyEvent e) {}
    @Override
    public void keyReleased(KeyEvent e) {}
    @Override
    public void keyTyped(KeyEvent e) {
        if (e.getID() == KeyEvent.VK_Z){
            zcounter = zcounter + 1;
            System.out.println("z presses: "+ zcounter);
        }
    }
}
