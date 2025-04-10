    
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
        private static final String COLS = "abcdefghilmnopqrstuvz";
    
        private final int n;   // n=var
        private final int q; 
        private final BiPredicate<Integer,Integer> attack;
        private final String config; 
        
        public Board(int n) {
            
            this.n = n; 
            q = 0; 
            attack = (x,y) -> false;   // (lambda ( x y ) false) x,y è minacciato oppure no? -> bu -> false
            config = ""; 
            
        }
        
        private Board(int i, int j, Board b) {
            
            n = b.size(); 
            q = b.QueensOn() + 1; 
            attack = (x, y) -> ( (x == i) || (y == j) || (x-y == i-j) || (x+y == i+j) || b.underAttack(x,y) ); 
            
            config = b.arrangement() + COLS.substring(j,j+1) + ROWS.substring(i,i+1) + " ";
        }
        
        public int size() {
            
            return n;
        }
        
        public int QueensOn() {
            
            return q;
        }
        
        public boolean underAttack(int i, int j) {
            
            
            return attack.test(i,j);   // (attack i,j) ueh,so'mejo
        }
        
        public Board addQueen(int i, int j) {
            
            return new Board( i, j, this );  // deve essere private
        }
        
        public String arrangement() {
            
            return config; 
        }
        
        public String toString() {
            return "[" + arrangement() + "]";
        }
        
    } // class Board
