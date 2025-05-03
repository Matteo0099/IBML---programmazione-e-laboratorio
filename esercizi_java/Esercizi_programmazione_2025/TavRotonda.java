
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
    private int n; 
    private int b;
    private int[] cav;   // cavalieri
     
    public TavRotonda(int n) {
        
        this.n = n;
        b = 0;
        cav = new int[ 2*n-1 ];
        
        for(int k=1; k<=n; k++) {
            cav[k-1] = k;
        }
    }
    
     // metodi
    public int numCavalieri() {
        
        return n;
    }
    
    public int cavConBrocca() {
        
        return cav[b]; 
    }
    
    public void serve() {
        
        if ( n > 1 ) {
            cav[b+1] = cav[b];
            b = b + 1;
            n = n - 1;
        } 
    }
    
    public void passa() {
        
        if ( n > 1 ) {
            cav[b+n] = cav[b];
            b = b + 1;
        }
    }
}

/*public class TavRotonda {
    
    private static final SList<Integer> NULL_INTLIST = new SList<Integer>(); // lista vuota è unica (1)
    private final int n;
    private final int brocca;
    private final SList<Integer> altri;   // cambia poco da int->integer (valore->oggetto)
    
    public TavRotonda( int n ) {  // n > 0
        
        this.n = n;
        brocca = 1; 
        altri = intervallo( 2, n );
    }
    
    private TavRotonda( int n, int brocca, SList<Integer> altri ) {  // n > 0
        
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
            SList<Integer> ultimo = NULL_INTLIST.cons(brocca);
            return new TavRotonda( n, altri.car(), altri.cdr().append(ultimo)  );
        } else {
            return this;
        }
    }
    
    public static SList<Integer> intervallo( int inf, int sup ) {
        
        if (inf > sup) {
            return NULL_INTLIST;
        } else {
            return intervallo( inf + 1, sup ).cons(inf);
        }
    }

}*/  // class TavRotonda
