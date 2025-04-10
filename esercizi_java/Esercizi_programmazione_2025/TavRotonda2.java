
/**
 *   Tavrotonda 2?
     *  TavRotonda t;
     *  int n;
 * 
 *    new TavRotonda(n)   : TavRotonda
 *    t.numCavalieri()    : int
 *    t.cavConBrocca()    : int
 *  
 *    t.serve()           : TavRotonda
 *    t.passa()           : TavRotonda  
 */

public class TavRotonda2 {
    
    private static final SList<Integer> NULL_INTLIST = new SList<Integer>(); // lista vuota è unica (1)
    private final int n;
    private final int brocca;
    private final SList<Integer> altri;   // cambia poco da int->integer (valore->oggetto)
    private final SList<Integer> rovesciati;
    
    public TavRotonda2( int n ) {  // n > 0
        
        this.n = n;
        brocca = 1; 
        altri = intervallo( 2, n );
        rovesciati = NULL_INTLIST;
    }
    
    private TavRotonda2( int n, int brocca, SList<Integer> altri, SList<Integer> rovesciati ) {  // n > 0
        
        this.n = n;
        this.brocca = brocca; 
        this.altri = altri;
        this.rovesciati = rovesciati;
    }
    
    // metodi
    public int numCavalieri() {
        
        return n;
    }
    
    public int cavConBrocca() {
        
        return brocca; 
    }
    
    public TavRotonda2 serve() {
        
        if ( n > 1 ) {
            if( altri.isNull() ) {
                SList<Integer> s = rovesciati.reverse();
                return new TavRotonda2( n-1, brocca, s.cdr(), NULL_INTLIST );
            } else {
                return new TavRotonda2( n-1, brocca, altri.cdr(), rovesciati );
            }
        } else {
            return this;
        }
    }
    
    public TavRotonda2 passa() {
        
        if ( n > 1 ) {
            if( altri.isNull() ) {
                SList<Integer> s = ( rovesciati.cons(brocca) ).reverse();
                return new TavRotonda2( n-1, s.car(), s.cdr(), NULL_INTLIST );
            } else {
                return new TavRotonda2( n, altri.car(), altri.cdr(), rovesciati.cons(brocca) );
            }
        } else {
            return this;
        }
    }
    
    public static SList<Integer> intervallo( int inf, int sup ) {
        
        if (inf > sup) {
            return NULL_INTLIST;
        } else {
            return intervallo( inf+1, sup ).cons(inf);
        }
    }

}  // class TavRotonda
