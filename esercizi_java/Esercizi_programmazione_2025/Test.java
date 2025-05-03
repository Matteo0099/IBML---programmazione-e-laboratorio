
/**
 * classe test, riguardo la tavola rotonda di G. Flavio 
 */

public class Test {

    public static int gFlavio(int n) { // n > 0
        
        TavRotonda tav = new TavRotonda(n); // Costruttore corretto

        while ( tav.numCavalieri() > 1 ) {
            
            tav.serve(); // Metodo chiamato sull'istanza
            tav.passa(); // Verifica se il metodo è "passa" o "passa"
        }
        
        return tav.cavConBrocca();
    }
    
    /*public static int gFlavio2( int n ) {   // n > 0
        TavRotonda2 tav = new TavRotonda2( n );
        
        while( tav.numCavalieri() > 1 ) {
            tav = tav.serve();
            tav = tav.passa();
        }
        
        return tav.cavConBrocca();
    }
    
    private static final long TMIN = 1000;  // t mis. 1 sec.
    
    public static double tempoRilevato( int n ) {
        
        int count = 0; 
        long t;
        
        // misura tempi elaboraz.
        
        long t0 = System.currentTimeMillis();
        
        do {
            count = count + 1;
            int x = gFlavio2( n );
            t = System.currentTimeMillis() - t0;
        
        } while ( t < TMIN );
        
        long t1 = System.currentTimeMillis();
        long u = 0; 
        
        for ( int k = 0; k < count; k++ ) {
            u = System.currentTimeMillis() - t1; 
        }
        
        return ( ((double) t - u) / count );
    }
    
    public static void test() {
        System.out.println(); 
        for ( int n = 1000; n<=10000; n=n+1000 ) {
            double t = tempoRilevato( n );
            System.out.println( t + "msec: " + (t / n));
        }
    }*/
    
}  // class Test