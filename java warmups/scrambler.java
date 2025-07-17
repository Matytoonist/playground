//Something that shuffles all but the first and last character in every word. Txet wtirten tihs way is pterty esay to raed.
import java.util.*;

public class scrambler {
    public static void main(String[] args) {
        String message = "";
        for (String word:args){
            message = message + scramble(word) + " ";
        }
        System.out.println(message);   
    }

    private static String scramble(String word){
        //get the midddle of the string
        String middle = word.substring(1, (word.length() - 1));

        //convert to list and scramble it with Collections.shuffle()
        List<Character> workList = new ArrayList<>();
        for (char c : middle.toCharArray()) {
            workList.add(c);
        }

        Collections.shuffle(workList);

        StringBuilder shuffledString = new StringBuilder();
        for (char c : workList) {
            shuffledString.append(c);
        }

        //add the first and last characters again & return
        return (word.charAt(0) + shuffledString.toString() + word.charAt(word.length() - 1));
    }
}