/*
For the cousin relation, the word 'cousin' alone is not allowed
in the input file, because the Lisp FTIP (Family Tree Interpreting Program)
expects to see: (cousin <degree> <removal>)

This program handles the transhlation of 'cousin' to '(cousin <degree> <removal>)'
by replacing each occurrence of 'cousin' with '(cousin 1 0)' so that an input
file designed for the Java/Python FTIP can be interpreted by the Lisp FTIP.
*/

import java.util.Scanner;
import java.util.StringTokenizer;
import java.util.*;
import java.io.FileNotFoundException;

public class parenthesize {
	
  public static void main(String[] args) throws FileNotFoundException {

  	 StringTokenizer st;
  	 int toksLeft;
  	 int i;
  	 String line = "";
  	 String nextTok;
  	 String cousinDegree = args[0];
  	 String cousinRemoval = args[1];
	  
	 Scanner scan = new Scanner(System.in); // Scanner that scans standard input
	  
	 try {
	 	while (scan.hasNextLine()) {
	 		line = scan.nextLine();
	 		if (!line.isEmpty()) {
	 			System.out.print("(");
		 		st = new StringTokenizer(line);
		 		toksLeft = st.countTokens();

		 		for (i=0; i<toksLeft; i++) {
		 			if (i!=0)
		 				System.out.print(" ");

		 			nextTok = st.nextToken();

		 			if (nextTok.equals("cousin"))
		 				System.out.print("(cousin " + cousinDegree + " " + cousinRemoval + ")");
		 			else
		 				System.out.print(nextTok);
		 		}
		 		System.out.print(")\n");
		 	}
		}
	 }
	 
	 finally {
	  	scan.close(); // prevents memory leakage from Scanner scan
	 }
  }
}