import java.util.Scanner;
import java.util.StringTokenizer;
import java.util.*;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.lang.String;
import java.util.Enumeration;


public class Ftip {
	
	private String s; // string in each node
	private Ftip nextNode;
	private Ftip currNode;
	
	// give each Ftip node an ArrayList for each relationship
	private ArrayList<Ftip> spouse = new ArrayList<Ftip>();
	private ArrayList<Ftip> parent = new ArrayList<Ftip>();
	private ArrayList<Ftip> child = new ArrayList<Ftip>();
	private ArrayList<Ftip> sibling = new ArrayList<Ftip>();
	private ArrayList<Ftip> halfsibling = new ArrayList<Ftip>();
	private ArrayList<Ftip> ancestor = new ArrayList<Ftip>();
	private ArrayList<Ftip> descendant = new ArrayList<Ftip>();
	private ArrayList<Ftip> cousin = new ArrayList<Ftip>();

	
	public Ftip(String s){ // constructor
		this.s = s;
		this.nextNode = null;
	}
	
	// sorts ArrayLists' contents alphabetically (query W)
	public static String formatW1(ArrayList<Ftip> t){
		Ftip temp;
		for (int i=0; i<t.size(); i++)		// n comparisons per cycle through elements
			for (int j=i+1;j<t.size();j++){		// n-1 cycles through elements
				// means if t.get(i).toString() is later in the alphabet than
				// t.get(j).toString()
				if (t.get(i).toString().compareTo(t.get(j).toString())>0){
					temp = t.get(i);	// stores t.get(i) in temporary variable of type Ftip
					t.set(i, t.get(j)); // sets t[i] equal to the next element, t.get(j)
					t.set(j, temp);		// sets t[j] equal to the previous element, t.get(i)
			}
		}
		return t.toString();
	}
	
	// re-formats display of ArrayLists' contents (query W)
	public static String formatW2(String f){
		return f.replace("[", "").replace(",", "").replace(" ", "\n").replace("]", "");
	}
	
	
    public static void findAncestors(Ftip par1, Ftip par2, Ftip ch) {
    	Ftip a;
    	Ftip b;
    	
    	if (par1.parent.size() == 2) {	// checking par1's side of the family for ancestors
    		a = par1.parent.get(0);			// first element in par1's parent ArrayList
    		b = par1.parent.get(1);			// second element
    		
    		if (!ch.ancestor.contains(a)) { // if a is not already in ancestor ArrayList of child, add a
    			ch.ancestor.add(a);
    			a.descendant.add(ch);
    		}	
    			
    		if (!ch.ancestor.contains(b)) {
    			ch.ancestor.add(b);	
    			b.descendant.add(ch);
    		}
    		findAncestors(a, b, ch);		// recursive call
    	}
    	
    	if (par2.parent.size() == 2) {	// checking par2's side of the family for ancestors
    		a = par2.parent.get(0);
    		b = par2.parent.get(1);
    		
    		if (!ch.ancestor.contains(a)) { // if a is not already in ancestor ArrayList of child, add a
    			ch.ancestor.add(a);
    			a.descendant.add(ch);
    		}	
    			
    		if (!ch.ancestor.contains(b)) {
    			ch.ancestor.add(b);	
    			b.descendant.add(ch);
    		}
    		findAncestors(a, b, ch);		// recursive call
    	}
    }
	
	
	public void setNext(Ftip c){
		this.nextNode = c;
	}
	
	public String getString(){
		return this.s;
	}
	
	public Ftip getNext(){
		return this.nextNode;
	}
	

	@Override
	public String toString() {
		String quotes = "";
		currNode = this; // setting buffer node equal to head node
		while (currNode != null){
			quotes += currNode.getString();
			currNode = currNode.nextNode;
		}
		return quotes;
	}
	
	
  public static void main(String[] args) throws FileNotFoundException {
	  String name1, name2; // used in queries, E, X, and R; only name1 used in query W
	  String childName = ""; // needs to be initialized
	  StringTokenizer st; // tokenizer for lines in input file
	  int i; // iterator variable for SIBLINGS and HALF-SIBLINGS
	  
	  String QueryChar; // E, R, X, or W
	  String relation; // spouse, parent, child, sibling, half-sibling, ancestor, or cousin
	  
	  String nameConstraintsMessage = "Query entry not registered: a name must\ncontain no hyphen and be less"
			  + " than 20\ncharacters long."; // all queries
	  
	  boolean haveChild = false;	// true if name1 and name2 in query E have a child
	  boolean nameConstraintsIssue = false;	// true if name violates constraints
	  										// described by nameConstraintsMessage.
	  										// Used in all queries
	  boolean sameName = false;	// true if any two names of the three in query E are the same.
	  							// Used in query E only.
	  boolean reincarnation = false; // true if childName was born twice, meaning childName was
	  								 // already in Hashtable before birth
	  
	  Hashtable<String, Ftip> hash = new Hashtable<String, Ftip>();
	  
	  Enumeration<String> e; // serves as an iterator through hash using hash keys
	  String cousinName;	 // prospective cousin's name
	  
	  Scanner scan = new Scanner(System.in); // Scanner that scans standard input
	  
	  try {	  
		  
		  while(scan.hasNextLine()){ // while scan detects more lines in input file
	      
	      // tokenizer st is set to equal next line detected in input file by scan
		  st = new StringTokenizer(scan.nextLine());
		  
		  if (st.hasMoreTokens()){
		  QueryChar = st.nextToken(); // QueryChar set to next token in tokenizer st
		  
		  switch (QueryChar) {
		  
		  case "E": 
			  haveChild = false;
			  nameConstraintsIssue = false;
			  sameName = false;
			  reincarnation = false;
			  
			  name1 = st.nextToken(); // first potential parent 
			  name2 = st.nextToken(); // second potential parent
			  
			  // Check if name1 and name2 meet name constraints
			  if (name1.contains("-") || name2.contains("-")
					  || name1.length() >= 20 || name2.length() >= 20)
			  	  nameConstraintsIssue = true;
			  
			  // Check if name1 and name2 are the same
			  if (name1.equals(name2))
				  sameName = true;
			  
			  if (st.countTokens() == 1) { // if st has 1 token remaining 
				  						   // (i.e. if name1 and name2 have a child)
				  haveChild = true;
				  childName = st.nextToken(); // their child

				  System.out.println("\nE " + name1 + " " + name2 + " " + childName);
			  
				  // Check if childName meets name constraints
				  if (childName.contains("-") || childName.length() >= 20)
					  nameConstraintsIssue = true;
				  
				  // Check if childName is the same as any of the parents' names
				  if (childName.equals(name1)
						  || childName.equals(name2))
					  sameName = true;
				  
				  if (hash.containsKey(childName))
					  reincarnation = true;
				  
				  if (nameConstraintsIssue == false 
						  && sameName == false
						  && reincarnation == false)
					  
					  // if childName's key is not in Hashtable, give it a Ftip instance and put
					  // it in Hashtable
					  // ..unless any of the three names don't meet the name constraints
					  // and unless any two names are the same
					  
					  hash.put(childName, new Ftip(childName));
			  }

			  else if (st.countTokens() == 0)
			  	  System.out.println("\nE " + name1 + " " + name2);
			  
			  
			  if (nameConstraintsIssue == false 
					  && sameName == false
					  && reincarnation == false) {
					  			  
				  // if neither name1 nor name2's key is in Hashtable, give them Ftip instances
				  // and put them in Hashtable
				  if (!hash.containsKey(name1)
						  && !hash.containsKey(name2)) {  
				  
					  // add both to Hashtable
					  hash.put(name1, new Ftip(name1)); // key: name; value: Ftip instance
					  hash.put(name2, new Ftip(name2));
				  }
				  
				  
				  // if name1's key is in Hashtable but name2's is not, give
				  // name2 a Ftip instance and put it in Hashtable
				  else if (hash.containsKey(name1)
						 && !hash.containsKey(name2))
				  	  hash.put(name2, new Ftip(name2));
					  
				  
				  // and vice-versa
				  else if (hash.containsKey(name2)
						 && !hash.containsKey(name1))
					  hash.put(name1, new Ftip(name1));
				  
				  
				  //**************
				  // SPOUSES
				  //**************
				  
				  // add name1 and name2 to each other's spouse ArrayLists (marriage or re-marriage)
				  // if they are not already there
				  
				  if (!hash.get(name1).spouse.contains(hash.get(name2))
						  && !hash.get(name2).spouse.contains(hash.get(name1))) {
					  hash.get(name1).spouse.add(hash.get(name2));
					  hash.get(name2).spouse.add(hash.get(name1));
				  }
						  
				  
				  if (haveChild == true){
				  
				  //**************
			      // SIBLINGS
			      //**************
				  
			      // for any equal names in their child Arraylists,
			      // add child to the sibling ArrayLists of the Ftip instances
			      // of those equal names
				  
		    	  for (i = 0; i < hash.get(name1).child.size(); i++)
		    		  if (hash.get(name2).child.contains(hash.get(name1).child.get(i))
		    				  && !hash.get(childName).sibling.contains(hash.get(name1).child.get(i))
							  && !hash.get(name1).child.get(i).sibling.contains(hash.get(childName))) {
		    			  
		    			  // add older sibling's Ftip instance to child's 
		    			  // Ftip instance's sibling ArrayList if not already there
		    			  // and vice-versa
		    			  
	    				  hash.get(childName).sibling.add(hash.get(name1).child.get(i));
	    				  hash.get(name1).child.get(i).sibling.add(hash.get(childName));
		    		  }
			      
			      //**************************
			      // HALF-SIBLINGS
			      //**************
			      
			      // Check for half-siblings on name1's side (by checking name1's Ftip 
			      // instance's child ArrayList for elements not in common with name2's
			      // Ftip instance's child ArrayList)
		    	  
			      for (i = 0; i < hash.get(name1).child.size(); i++)
		    		  if (!hash.get(name2).child.contains(hash.get(name1).child.get(i))
		    				  && !hash.get(childName).halfsibling.contains(hash.get(name1).child.get(i))
							  && !hash.get(name1).child.get(i).halfsibling.contains(hash.get(childName))) {
		    			  
		    			  // add half-sibling's Ftip instance to child's
		    			  // Ftip instance's sibling ArrayList if not already there
		    			  // and vice-versa
		    			  
	    				  hash.get(childName).halfsibling.add(hash.get(name1).child.get(i));
	    				  hash.get(name1).child.get(i).halfsibling.add(hash.get(childName));
		    		  }
				      
			      // Same thing, but looking at name2's side
			      for (i = 0; i < hash.get(name2).child.size(); i++)
		    		  if (!hash.get(name1).child.contains(hash.get(name2).child.get(i))
		    				  && !hash.get(childName).halfsibling.contains(hash.get(name2).child.get(i))
							  && !hash.get(name2).child.get(i).halfsibling.contains(hash.get(childName))) {
		    			  
		    			  // add half-sibling's Ftip instance to child's
		    			  // Ftip instance's sibling ArrayList if not already there
		    			  // and vice-versa
		    			  
	    				  hash.get(childName).halfsibling.add(hash.get(name2).child.get(i));
	    				  hash.get(name2).child.get(i).halfsibling.add(hash.get(childName));
		    		  }
				  }
			    			  
			      //**************
			      // HALF-SIBLINGS
			      //**************************
			      
				      
				  //**************
				  // CHILD
				  //**************
			      
			      // add childName's Ftip instance to child ArrayList of each parent.
				  
			      hash.get(name1).child.add(hash.get(childName));
			      hash.get(name2).child.add(hash.get(childName));
			      
			      //**************
			      // PARENTS
			      //**************
					  
				  // add parents' Ftip instances to parent ArrayList of childName
			      
			      hash.get(childName).parent.add(hash.get(name1));
			      hash.get(childName).parent.add(hash.get(name2));
					 
				  //****************************
				  // ANCESTORS and DESCENDANTS
				  //****************************
			      
			      // add both parents to childName's ancestor ArrayList
			      hash.get(childName).ancestor.add(hash.get(name1));
			      hash.get(childName).ancestor.add(hash.get(name2));

			      // add child to both parents' descendant ArrayLists
			      hash.get(name1).descendant.add(hash.get(childName));
			      hash.get(name2).descendant.add(hash.get(childName));
			      
			      // then recursively add name1's and name2's parents' parents' parents' (etc.)
			      // to childName's ancestor ArrayList
			      // also adds childName to descendant ArrayLists of Ancestors
			      findAncestors(hash.get(name1), hash.get(name2), hash.get(childName));	// recursive method
			      
			      //**************
				  // COUSINS
				  //**************
			      
			      e = hash.keys();	// e can now enumerate (obtain in sequence) the keys in hash
			      
			      while (e.hasMoreElements()) {
			    	  cousinName = e.nextElement();	// cousinName equals next key in Hashtable
			    	  for (i = 0; i < hash.get(cousinName).ancestor.size(); i++) {
		    		  if (hash.get(childName).ancestor.contains(hash.get(cousinName).ancestor.get(i)))
			    	  		if (!hash.get(childName).cousin.contains(hash.get(cousinName)))
			    	  			if (!cousinName.equals(childName)) {
			    	  				hash.get(childName).cousin.add(hash.get(cousinName));
			      					hash.get(cousinName).cousin.add(hash.get(childName));
			    	  			}
			    	  }
			     }
			 }
  
			  if (nameConstraintsIssue == true)
				  System.out.println(nameConstraintsMessage);
			  if (sameName == true)
				  System.out.println("Query entry not registered: no two names provided may be"
				  		  + " the same.");
			  if (reincarnation == true)
				  System.out.println("Query entry not registered:\n" + childName + " was already born.");
			  break;
			  
		  case "R":
			  
			  nameConstraintsIssue = false;
			  
			  name1 = st.nextToken();
			  name2 = st.nextToken();
			  System.out.println("\nR " + name1 + " " + name2);
			  
			  if (name1.contains("-") || name2.contains("-")
					  || name1.length() >= 20 || name2.length() >= 20) {
				  nameConstraintsIssue = true;
				  System.out.println(nameConstraintsMessage);
			  }
			  
			  if (nameConstraintsIssue == false) { // if true, skip rest of case R
				  
				  // e.g. R john john
				  if (name1.equals(name2))
					  System.out.println("A person has no relation to his"
					  		+ " or herself in this program.");
				  
				  // if at least one of the names is not a key in the Hashtable, unrelated
				  else if (!hash.containsKey(name1)
						  || !hash.containsKey(name2))
					  System.out.println(name1 + " and " + name2 + " are unrelated");
				  
				  else{ // if both names are keys in Hashtable
					  
					  // Written in order of most-closely related to least-closely related, 
					  // using else-ifs so that only the statement representing the closest
					  // relation between name1 and name2 will show up in the output
					  
					  if (hash.get(name2).spouse.contains(hash.get(name1)))
						  System.out.println(name1 + " is the spouse of " + name2);		// most-closely related
					  else if (hash.get(name2).parent.contains(hash.get(name1)))
						  System.out.println(name1 + " is the parent of " + name2);
					  else if (hash.get(name2).child.contains(hash.get(name1)))
						  System.out.println(name1 + " is the child of " + name2);
					  else if (hash.get(name2).sibling.contains(hash.get(name1)))
						  System.out.println(name1 + " is the sibling of " + name2);
					  else if (hash.get(name2).halfsibling.contains(hash.get(name1)))
						  System.out.println(name1 + " is the half-sibling of " + name2);
					  else if (hash.get(name2).ancestor.contains(hash.get(name1)))
						  System.out.println(name1 + " is the ancestor of " + name2);
					  else if (hash.get(name2).descendant.contains(hash.get(name1)))
						  System.out.println(name1 + " is the descendant of " + name2);
					  else if (hash.get(name2).cousin.contains(hash.get(name1)))
						  System.out.println(name1 + " is the cousin of " + name2);		// least-closely related (besides unrelated)
				      else
						  System.out.println(name1 + " and " + name2 + " are unrelated");
				  }
			  }
			  break;
			  
		  case "X":
			  
			  nameConstraintsIssue = false;
			  
			  name1 = st.nextToken();
			  relation = st.nextToken();
			  name2 = st.nextToken();
			  System.out.println("\nX " + name1 + " " + relation + " " + name2);
			  
			  if (name1.contains("-") || name2.contains("-")
					  || name1.length() >= 20 || name2.length() >= 20) {
				  nameConstraintsIssue = true;
				  System.out.println(nameConstraintsMessage);
			  }
			  
			  if (nameConstraintsIssue == false) { // if true, skip rest of case X
			  
				  
				  // e.g. X john <relation> john
				  if (name1.equals(name2))
					  System.out.println("A person has no relation to his"
					  		+ " or herself in this program.");
				  
				  // if at least one of the names is not a key in Hashtable, unrelated 
				  else if (!hash.containsKey(name1)
						  || !hash.containsKey(name2))
					  System.out.println("No");
					  		
				  else{ // if both names are keys in Hashtable and are not the same name
					  
					  switch(relation){
					  
				  	  case ("spouse"):
						  if (hash.get(name2).spouse.contains(hash.get(name1)))
						  	 System.out.println("Yes"); else System.out.println("No");
						  break;
				  	  case ("parent"):
						  if (hash.get(name2).parent.contains(hash.get(name1)))
						  	 System.out.println("Yes"); else System.out.println("No");
						  break;
				  	  case ("child"):
						  if (hash.get(name2).child.contains(hash.get(name1)))
							 System.out.println("Yes"); else System.out.println("No");
						  break;
				  	  case ("sibling"):
						  if (hash.get(name2).sibling.contains(hash.get(name1)))
							 System.out.println("Yes"); else System.out.println("No");
						  break;
				  	  case ("half-sibling"):
						  if (hash.get(name2).halfsibling.contains(hash.get(name1)))
							 System.out.println("Yes"); else System.out.println("No");
						  break;
				  	  case ("ancestor"):
						  if (hash.get(name2).ancestor.contains(hash.get(name1)))
							 System.out.println("Yes"); else System.out.println("No");
						  break;
					  case ("descendant"):
						  if (hash.get(name2).descendant.contains(hash.get(name1)))
							 System.out.println("Yes"); else System.out.println("No");
						  break;
				  	  case ("cousin"):
						  if (hash.get(name2).cousin.contains(hash.get(name1)))
							 System.out.println("Yes"); else System.out.println("No");
						  break;
					  }
				  }
			  }
			  break;

		  case "W":
			  
			  nameConstraintsIssue = false;
			  
			  relation = st.nextToken();
			  name1 = st.nextToken();
			  System.out.println("\nW " + relation + " " + name1);
			  
			  if (name1.contains("-")
					  || name1.length() >= 20) {
				  nameConstraintsIssue = true;
				  System.out.println(nameConstraintsMessage);
			  }
			  
			  if (nameConstraintsIssue == false) { // if true, skip rest of case W
			  
				  // if name1 is not a key in Hashtable, unrelated 
				  if (!hash.containsKey(name1))
					  System.out.println("Nobody is related to " + name1);
				  
				  else{ // if name1 is a key in Hashtable
					  
				  switch(relation){
			  	  case ("spouse"):
			  	      if (!hash.get(name1).spouse.isEmpty()) {
			  	      		System.out.println("List of " + name1 + "'s spouses:");
			  	    	  	System.out.println(formatW2(formatW1(hash.get(name1).spouse)));
			  	      }
			  	      else
			  	    	  	System.out.println(name1 + " has no spouse");
			  	  	  break;
			  	  case ("parent"):
			  	  	  if (!hash.get(name1).parent.isEmpty()) {
			  	  	  		System.out.println("List of " + name1 + "'s parents:");
			  	  		  	System.out.println(formatW2(formatW1(hash.get(name1).parent)));
			  	  	  }
			  	  	  else
			  	  		  System.out.println(name1 + " has no parents");
			  	  	  break;
			  	  case ("child"):
				  	  if (!hash.get(name1).child.isEmpty()) {
				  	  		System.out.println("List of " + name1 + "'s children:");
				  		  	System.out.println(formatW2(formatW1(hash.get(name1).child)));
				  	  }
			  	  	  else
			  	  		  System.out.println(name1 + " has no children");
				  	  break;
			  	  case ("sibling"):
			  	  	  if (!hash.get(name1).sibling.isEmpty()) {
			  	  	  		System.out.println("List of " + name1 + "'s siblings:");
			  	  		  	System.out.println(formatW2(formatW1(hash.get(name1).sibling)));
			  	  	  }
			  	  	  else
			  	  		System.out.println(name1 + " has no siblings");
					  break;
			  	  case ("half-sibling"):
			  	  	  if (!hash.get(name1).halfsibling.isEmpty()) {
			  	  	  	 	System.out.println("List of " + name1 + "'s half-siblings:");
			  	  		  	System.out.println(formatW2(formatW1(hash.get(name1).halfsibling)));
			  	  	  }
			  	  	  else
			  	  		  System.out.println(name1 + " has no half-siblings");
			  	  	  break;
			  	  case ("ancestor"):
			  	  	  if (!hash.get(name1).ancestor.isEmpty()) {
			  	  	  		System.out.println("List of " + name1 + "'s ancestors:");
			  	  		  	System.out.println(formatW2(formatW1(hash.get(name1).ancestor)));
			  	  	  }
			  	  	  else
			  	  		  System.out.println(name1 + " has no ancestors");
			  	  	  break;
			  	  case ("descendant"):
			  	  	  if (!hash.get(name1).descendant.isEmpty()) {
			  	  	  		System.out.println("List of " + name1 + "'s descendants:");
			  	  		  	System.out.println(formatW2(formatW1(hash.get(name1).descendant)));
			  	  	  }
			  	  	  else
			  	  		  System.out.println(name1 + " has no descendants");
			  	  	  break;
			  	  case ("cousin"):
			  	  	  if (!hash.get(name1).cousin.isEmpty()) {
			  	  	  		System.out.println("List of " + name1 + "'s cousins:");
			  	  		  	System.out.println(formatW2(formatW1(hash.get(name1).cousin)));
			  	  	  }
			  	  	  else
			  	  		  System.out.println(name1 + " has no cousins");
			  	  	  break;
				  }
				  }
			  }
			  break;
		  }
		  }
		  }
		  } finally {
			  scan.close(); // prevents memory leakage from Scanner scan
		  }
	}
}