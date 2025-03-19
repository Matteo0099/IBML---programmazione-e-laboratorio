
/**
* Corso programmazione parte 2 - java
* Programmazione imperativa
*/

public class tutto {

    // massimo comune divisore
    public static int mcd (int x, int y) {  // x,y>0
        while ( x != y ) {
            if ( x < y ) {
                y = y - x;
            } else {
                x = x - y;
            }
        }
        return x;
    }
    
    // minimo comune multiplo
    public static int mcm (int x, int y) {
        int m = x;
        while ( m % y > 0 ) {
            m = m + x;
        }
        return m;
    }
    
    // numero primo?
    private static boolean primo ( int n ) {
        if ( n % 2 == 0 ) {
            return ( n == 2 );
        } else {
            int k = 3;
            while ( k*k <= n ) {
                if ( n % k == 0 ) {
                    return false;
                } else {
                    k = k + 2;
                }
            }
            return true;
        }
    }
    
    // lista dei numeri primi
    public static void listaPrimi ( int max ) {
        for ( int n=2; n<=max; n=n+1) {
            if ( primo (n) ) {
                System.out.print( "   " + n );
            }
        }
    }
    
    // bit
    public static int btrVal ( String btr ) {
        int n = btr.length();
        int val = 0; 
        
        for ( int i = 0; i<n; i=i+1 ) {
            val = 3 * val + btdVal(btr.substring(i,i+1));
        }
        
        return val; 
    }
    
    public static int btdVal ( String btd ) {
        if ( btd.equals("-") ) {
            return -1;
        } else if ( btd.equals(".") ) {
            return 0;
        } else {
            return +1;
        }
    }
    
    // ufo
    public static int ufo ( int n ) {
        int[] u = new int[ n+1 ];
        u[1] = 1;
        
        for ( int x=2; x<=n; x=x+1 ) {
            if ( x % 2 == 0 ) {
                u[x] = 2 * u[x/2] - 1;
            } else {
                u[x] = 2 * u[x/2] + 1;
            }
        }
        return u[n];
    }
    
    // ufo --- Soluzione "Top down" ---
    /*
     * n = 13
     * |12| /2 |6| /2 |3| /2 |1|
     *                        1
     *                 3
     *          5
     *  9
     *
     *  k:
     *  y = (1,3) 5
     *  T[] a = T[ dim ];  // creazione di un array
     *  a[i] = ...         // accesso in scrittura
     *  ... = ... a[i] ... // accesso in lettura
     *  if/while (... a[i] ...)
     */
    public static int ufoTopDown ( int n ) {
        int[] x = new int[ (int) (Math.log(n)/Math.log(2)) + 1 ];   // log in base 2 di n
        x[0] = n;   // (limite sup) prima invocazione ricorsiva (argomento n)
        
        // fase discendente - ritorno con i risultati
        int k = 0;
        while( x[k] > 1 ) {   // se caso base > 1
            x[k+1] = x[k] / 2;   // es: 12/2 = 6, ... , 1.
            k = k + 1;          // indice
        }
        
        // fase ascendente - chiamate ricorsive
        int y = 1;  // x = 1
        while( k > 0 ) {
            k = k - 1;
            if ( x[k] % 2 == 0 ) {
                y = 2 * y - 1;
            } else {
                y = 2 * y + 1;
            }
        }
        
        return y;
    }
    
    // Crivello di Eratostene
    public static void eratostene( int n ) {
        //System.out.print( "  2" );
        int count = 1;
        
        boolean[] crivello = new boolean[ n/2+1 ];
        for ( int k=3; k<=n; k=k+2 ) {
            crivello[k/2] = true;
        }
        
        for ( int k=3; k<=n; k=k+2 ) {
            if ( crivello[k/2] ) {
                //System.out.print( "  " + k );
                count = count + 1;
                long m = ((long) k)*k;
                while( m <= n ) {
                    crivello[ (int) m/2] = false;
                    m = m + 2*k; 
                }
            }
        }
        System.out.println( "  P(" + n + ") = " + count );
        System.out.println( "  R(" + n + ") = " + (n/Math.log(n)) );
        System.out.println( "  P(" + n + ")/R(" + n + ") = " + (count/(n/Math.log(n))) );
    }
}
