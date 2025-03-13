
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
}
