
public class Test {
    public static IntSList intervallo( int inf, int sup ) {
        if( inf > sup ) {
            return new IntSList(); 
        } else {
            return intervallo(inf+1, sup).cons( inf );
        }
    }
    
} // class Test