
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

public class IntSList {
    
    // variabili d'istanza:
    private boolean empty;
    private int first; 
    private IntSList rest;
    
    
    /**
     * Costruttore degli oggetti di classe  IntSList
     */
    public IntSList() {}

    /**
     * Metodi...
     */
    
    public boolean isNull() {
        
    }
    
    public int car() {
    
        return first;
    }
    
    public IntSList cdr() {
    
    }
    
    public IntSList cons(e) {
    
    }
    
}
