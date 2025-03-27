
/**
 * 
 *  TavRotonda t;
 *  int n;
 * 
 *    new TavRotonda(n)   : TavRotonda
 *    t.numCavalieri()    : int
 *    t.cavConBrocca()    : int
 *  
 *    t.serve()           : TavRotonda
 *    t.passa()           : TavRotonda  
 *
 *    --------------------------
 *    CALCOLI
 *    
 *    n     : k=n-1
 *    serve : k-1
 *    passa : k-1  -cons
 *    serve : k-2
 *    passa : k-2  -cons
 *    ...
 *    
 *    (n-1) + (n-2) + (n-3) + ... + 1  =  ? 
 *    
 *    -------------------------- 
 *    
 */

public class TavRotonda {
    
    private final int n;
    private final int brocca;
    private final IntSList altri;
    
    public TavRotonda( int n ) {  // n > 0
        
        this.n = n;
        brocca = 1; 
        altri = intervallo( 2, n);
    }
    
    private TavRotonda( int n, int brocca, IntSList altri ) {  // n > 0
        
        this.n = n;
        this.brocca = brocca; 
        this.altri = altri;
    }
    
    // metodi
    public int numCavalieri() {
        
        return n;
    }
    
    public int cavConBrocca() {
        
        return brocca; 
    }
    
    public TavRotonda serve() {
        
        if ( n > 1 ) {
            return new TavRotonda( n-1, brocca, altri.cdr() );
        } else {
            return this;
        }
    }
    
    public TavRotonda passa() {
        
        if ( n > 1 ) {
            IntSList ultimo = IntSList.NULL_INTLIST.cons(brocca);
            return new TavRotonda( n, altri.car(), altri.cdr().append(ultimo)  );
        } else {
            return this;
        }
    }
    
    public static IntSList intervallo(int inf, int sup) {
        
        if (inf > sup) {
            return IntSList.NULL_INTLIST;
        } else {
            return intervallo(inf + 1, sup).cons(inf);
        }
    }

}  // class TavRotonda
