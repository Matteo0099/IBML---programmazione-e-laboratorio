/**
  * Determinare n* soluzioni dato n scacchiera.
  * 
  * Board b;
  * int n, i, j;
  * 
  * b = new Board(n)
  * 
  * b.size()             : int
  * b.queensOn()         : int
  * 
  * b.underAttack(i,j)   : Boolean
  * 
  * b.addQueen(i,j)      : Board
  * 
  * b.arrangement()      : String
  * 
**/

import java.util.function.*;

public class Board {
    
    private static final String ROWS = "123456789ABCDEF";
    private static final String COLS = "abcdefghilmno";

    private final int n;
    private int q;
    private String config;

    private int[] rowAttack;
    private int[] colAttack;
    private int[] dg1Attack;
    private int[] dg2Attack;
    
    public Board(int n) {
        
        this.n = n; 
        this.q = 0; 
        // attack = (x,y) -> false;   // (lambda ( x y ) false) x,y è minacciato oppure no? -> bu -> false
        this.config = " ";
        
        rowAttack = new int[n];
        colAttack = new int[n];
        dg1Attack = new int[ 2*n-1 ];
        dg2Attack = new int[ 2*n-1 ];
        
        for(int k=0; k<n; k++) {
            rowAttack[k] = 0;
            colAttack[k] = 0;
        }
        for(int k=0; k<2*n-1; k++) {
            dg1Attack[k] = 0;
            dg2Attack[k] = 0;
        }
    }

    public int size() {
        
        return n;
    }

    public int QueensOn() {
        
        return q;
    }
    
    public boolean underAttack(int i, int j) {
        return rowAttack[i] > 0 ||
               colAttack[j] > 0 ||
               dg1Attack[i - j + n - 1] > 0 ||
               dg2Attack[i + j] > 0;
    }

    
   public Board addQueen(int i, int j) {
        Board b = new Board(n);
        b.q = this.q + 1;
    
        if (j >= COLS.length() || i >= ROWS.length()) {
            b.config = this.config + "? ";  // oppure usa (i+1),(j+1)
        } else {
            b.config = this.config + COLS.charAt(j) + ROWS.charAt(i) + " ";
        }
    
        b.rowAttack = this.rowAttack.clone();
        b.colAttack = this.colAttack.clone();
        b.dg1Attack = this.dg1Attack.clone();
        b.dg2Attack = this.dg2Attack.clone();
    
        b.rowAttack[i]++;
        b.colAttack[j]++;
        b.dg1Attack[i - j + n - 1]++;
        b.dg2Attack[i + j]++;
    
        return b;
    }

    public String arrangement() {
        
        return "[" + config + "]"; 
    }
    
    public String toString() {
        
        return arrangement();
    }
    
} // class Board
