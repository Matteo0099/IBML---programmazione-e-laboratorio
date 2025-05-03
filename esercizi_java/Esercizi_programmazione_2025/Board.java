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
    
        private final int n;   // n=var
        private final int q; 
        private final String config;   // lista esterna vuota
        
        private int[] rowAttack;
        private int[] colAttack;
        private int[] dg1Attack;
        private int[] dg2Attack;
        
        public Board(int n) {
            
            this.n = n; 
            q = 0; 
            // attack = (x,y) -> false;   // (lambda ( x y ) false) x,y è minacciato oppure no? -> bu -> false
            config = " ";
            
            rowAttack = new int[n];
            colAttack = new int[n];
            dg1Attack = new int[ 2*n-1 ];
            dg2Attack = new int[ 2*n-1 ];
            
            for(int k=0; k<n; k++) {
                rowAttack[k] = 0;
                colAttack[k] = 0;
            }
            for(int k=0; k<2*n-1; k++) {
                rowAttack[k] = 0;
                colAttack[k] = 0;
            }
        }
        
        private Board(int i, int j, Board b) {
            
            /*n = b.size(); 
            q = b.QueensOn() + 1; 
            attack = (x, y) -> ( (x == i) || (y == j) || 
                                 (x-y == i-j) || (x+y == i+j) || 
                                 b.underAttack(x,y) ); 
            
             config = b.arrangement().cons( (new SList<Integer>()).cons(j).cons(i) );*/
        }
        
        public int size() {
            
            return n;
        }
        
        public int QueensOn() {
            
            return q;
        }
        
        public boolean underAttack(int i, int j) {
            
            return ( (rowAttack[i-1] > 0)     || (colAttack[j-1] > 0)   || 
                     (dg1Attack[i-j+n-1] > 0) || (dg2Attack[i+j-2] > 0) 
                     );
        }
        
        public void addQueen(int i, int j) {
            
            q = q + 1; 
            config = config + COLS.substring(j,j+1) + ROWS.substring(i,i+1) + " ";
            
            rowAttack[i-1] = rowAttack[i-1] + 1;  // 1 regina in più di prima.
            colAttack[i-1] = colAttack[j-1] + 1;  // 1 regina in meno che la minaccia
        }
        
        public String arrangement() {
            
            return config; 
        }
        
        public String toString() {
            
            return "[" + arrangement() + "]";
        }
        
    } // class Board
