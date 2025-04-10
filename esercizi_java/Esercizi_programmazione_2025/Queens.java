
/**
 * ( non ancora da compilare )
 * 
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

public class Queens {
    
    private static final SList<Board> NULL_BOARDLIST = new SList<Board>();  // lista vuota unica
    
    public static int numeroSoluzioni( int n ) {
        
        return numeroCompletamenti( new Board(n) );
    }
    
    public static int numeroCompletamenti( Board b ) {
        
        int n = b.size();
        int q = b.QueensOn(); 
        
        if( q == n ) {   // caso in cui è stata trovata
            return 1;
        } else {
            int count = 0;   // counter regine (di b)   - 0
            int i = q + 1;   // indice di riga          - 1
            for( int j=1; j<=n; j++ ) {
                // se è minacciata --> NON metto regina
                // se NON è minacciata --> la metto
                if( !b.underAttack(i,j) ) {
                    count = count + numeroCompletamenti(b.addQueen(i,j));   // somma n* completamenti - valore cumulativo
                }
            }
            return count;
        }
    }
    
    // BoardList : esercizio (lista soluzioni,completamenti)
    
    public static SList<Board> listaSoluzioni( int n ) {  // n > 0
        
        return listaCompletamenti( new Board(n) );
    }
    
    public static SList<Board> listaCompletamenti( Board b ) {
        
        int n = b.size();
        int q = b.QueensOn(); 
        
        if( q == n ) {
            return NULL_BOARDLIST.cons( b );   // soluzione: lista vuota
        } else {
            SList<Board> lista = NULL_BOARDLIST;   // counter regine (di b)  - 0
            int i = q + 1;          // indice di riga         - 1
            for( int j=1; j<=n; j++ ) {
                // se è minacciata --> NON metto regina
                // se NON è minacciata --> la metto
                if( !b.underAttack(i,j) ) {
                    lista = lista.append( listaCompletamenti(b.addQueen(i,j)) );   // somma n* completamenti
                }
            }
            return lista;
        }
    }
    
    
    
} // class Queens
