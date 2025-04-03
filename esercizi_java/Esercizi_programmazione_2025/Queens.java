
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
    public static int numeroSoluzioni( int n ) {
        
        return numeroCompletamenti( new Board(n) );
    }
    
    public static int numeroCompletamenti( Board b ) {
        
        int n = b.size();
        int q = b.queensOn(); 
        
        if( q == n ) {
            return 1;
        } else {
            int count = 0;   // counter regine (di b)  - 0
            int i = q + 1;   // indice di riga         - 1
            for( int j=1; j<=n; j++ ) {
                // se è minacciata --> NON metto regina
                // se NON è minacciata --> la metto
                if( !b.underAttack(i,j) ) {
                    count = count + numeroCompletamenti(b.addQueens(i,j));   // somma n* completamenti
                }
            }
            return count;
        }
    }
    
    // BoardList : esercizio (lista soluzioni,completamenti)
    
    public static BoardList listaSoluzioni( int n ) {  // n > 0
        
        //return listaCompletamenti( new Board(n) );
    }
    
    public static BoardList listaCompletamenti( Board b ) {
        
        int n = b.size();
        int q = b.queensOn(); 
        
        if( q == n ) {
            return BoardSList.NULL_BOARDLIST.cons( b );   // soluzione: lista vuota
        } else {
            BoardSList lista = 0;   // counter regine (di b)  - 0
            int i = q + 1;          // indice di riga         - 1
            for( int j=1; j<=n; j++ ) {
                // se è minacciata --> NON metto regina
                // se NON è minacciata --> la metto
                if( !b.underAttack(i,j) ) {
                    lista = lista.append( listaCompletamenti(b.addQueens(i,j)) );   // somma n* completamenti
                }
            }
            return lista;
        }
    }
    
} // class Queens
