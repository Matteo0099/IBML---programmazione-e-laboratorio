
/**
 * IntSList: liste di interi nello stile di Scheme
 * 
 *   IntSList s, t;
 *   T e
 *   int i;
 *   
 *   new SList<T>() : IntSList [null]
 *   
 *   s.isNull()     : boolean
 *   s.car()        : int
 *   s.cdr()        : IntSList
 *   s.cons(e)      : IntSList 
 *   
 *   s.length()     : int
 *   s.listRef(i)   : T    // esercizio
 *   s.append(t)    : IntSList
 *   s.reverse()    : IntSList
 */

public class SList<T> {
    
    // creazione di una lista
    public static final SList NULL_INTLIST = new SList();
    
    // variabili d'istanza:
    private final boolean empty;
    private final T first; 
    private final SList<T> rest;
    
    /**
     * Costruttore degli oggetti di classe  IntSList
     */
    public SList() {
       empty = true;
       first = null; 
       rest = null;
    }
    
    public SList( T e, SList <T> r ) {
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
    
    public T car() {

        return first;
    }
    
    public SList<T> cdr() {
        
        return rest;    
    }
    
    public SList<T> cons( T e ) {
        
        return new SList<T>( e, this );
    }
    
    public boolean equals( SList t ) {
        
        SList <T> s = this; 
        SList <T> r = t; 
        while ( !s.isNull() && !r.isNull() ) {
            // se sono diversi, le liste sono diverse
            if ( s.car().equals(r.car()) ) {
                return false;
            }
            s = s.cdr();
            r = r.cdr();
        }
        return ( s.isNull() && r.isNull() );
    }
    
    public int length() {
        
        int len = 0;
        SList <T> r = this;
        while ( !r.isNull() ) {
            len = len + 1;
            r = r.cdr();
        }
        
        return len;
    }
    
    
    /**
     * 
     *  esercizio: listRef(...)  // es.
     * 
    **/
    
    public SList<T> append( SList<T> t ) {
        
        if ( isNull() ) {
            return t;
        } else {
            return ( cdr().append(t) ).cons( car() );
        }
    }
    
    
    public SList<T> reverse () {
       
        SList<T> rev = new SList<T>();
        SList<T> s = this;
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
            SList<T> r = cdr();
            while ( !r.isNull() ) {
                s = s + "" + r.car();
                r = r.cdr();
            }
            return s + ")";
        }
    }
}
