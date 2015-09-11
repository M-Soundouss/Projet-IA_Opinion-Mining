package myprojj;


import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Scanner;

import edu.stanford.nlp.tagger.maxent.MaxentTagger;

class TaggerDemo {

  private TaggerDemo() {}

  public static void main(String[] args) throws Exception {
    int compteur = 0 ;
    while(true){
    	Scanner sc = new Scanner(System.in);
    	System.out.println("Voulez vous :\n  -1. Effectuer le pos tagging au dernier fichier pré-traité\n  -2. Quitter");
    	int a = sc.nextInt();
    if(a==1)
    {
    	System.out.println("Tagging du fichier 'resultpretreatment"+String.valueOf(compteur)+".txt'");
	MaxentTagger tagger = new MaxentTagger("C:/Users/MAIOUAK/workspace/taggers/english-left3words-distsim.tagger");

    PrintWriter writer = new PrintWriter("D:/resultpostagged"+String.valueOf(compteur)+".txt", "UTF-8");

    String tagged;
    
    Path file = Paths.get("D:/resultpretreatment"+String.valueOf(compteur)+".txt");;
	try (InputStream in = Files.newInputStream(file);
	    BufferedReader reader = new BufferedReader(new InputStreamReader(in))) {
	    String line = null;
	    while ((line = reader.readLine()) != null) {
	            
	    	tagged = tagger.tagString(line);
	      
	        	
	            writer.println(tagged);
	    }} 
	
	 writer.close();
	 compteur ++;
    }
    if(a==2) break;
    }
    }
}

