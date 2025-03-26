
/**
 * IntSList: liste di interi nello stile di Scheme
 * 
 *   IntSList s, t;
 *   int e;
 *   
 *   new IntSList() : IntSList [null]
 *   
 *   s.isNull() : boolean
 *   s.car()    : int
 *   s.cdr()    : IntSList
 *   s.cons(e)  : IntSList  
 */

public class IntSList extends Test {
    
    // creazione di una lista
    public static final IntSList NULL_INTLIST = new IntSList();
    
    // variabili d'istanza:
    private boolean empty;
    private int first; 
    private IntSList rest;
    
    /**
     * Costruttore degli oggetti di classe  IntSList
     */
    public IntSList() {
       empty = true;
       first = 0; 
       rest = null;
    }
    
    public IntSList( int e, IntSList r ) {
        empty = false;
        first = e; 
        rest = r;
    }

    /**
     * Metodi...
    **/
    
    public boolean isNull() {
        
        return empty;        
    }
    
    public int car() {

        return first;
    }
    
    public IntSList cdr() {
        
        return rest;    
    }
    
    public IntSList cons( int e ) {
        
        return new IntSList( e, this );
    }
    
    public boolean equals( IntSList t ) {
        
        IntSList s = this; 
        IntSList r = t; 
        while ( !s.isNull() && !r.isNull() ) {
            // se sono diversi, le liste sono diverse
            if ( s.car() != r.car() ) {
                return false;
            }
            s = s.cdr();
            r = r.cdr();
        }
        return ( s.isNull() && r.isNull() );
    }
    
    public int length() {
        
        int len = 0;
        IntSList r = this;
        while ( !r.isNull() ) {
            len = len + 1;
            r = r.cdr();
        }
        
        return len;
    }
    
    
    /**
     * 
     *  esercizio: listRef(...)
     * 
    **/
    
    public IntSList append ( IntSList t ) {
        
        if ( isNull() ) {
            return t;
        } else {
            return ( cdr().append(t) ).cons( car() );
        }
    }
    
    
    public IntSList reverse ( IntSList t ) {
       
        IntSList rev = NULL_INTLIST;
        IntSList s = this;
        while ( !s.isNull() ) {
            rev = rev.cons( s.car() );
            s = s.cdr();
        }
        
        return rev;  
    }
    
    
    // ...ogni iterazione è il cdr del cdr precedente...
    public String toString() {
        if ( isNull() ) {   // this.isNull() è uguale   
            return "()";
        } else {
            String s = "(" + car();
            IntSList r = cdr();
            while ( !r.isNull() ) {
                s = s + "" + r.car();
                r = r.cdr();
            }
            return s + ")";
        }
    }
}
