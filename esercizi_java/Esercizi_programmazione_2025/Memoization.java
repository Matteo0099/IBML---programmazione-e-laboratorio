
/**
 * Problema di Fibonacci
 * 
 * Fib(4)=2,
 * Fib(10)=89,
 * Fib(20)=10946,
 * ...
 * Dopo fib(40) diventa molto lento.
 * E con long si può andare oltre fib(46).
 * 
 * 
 * Dimostrazione per induzione: R(n) >= (3/2)^(n-1)    (complessità)
 * 
 * R(0) = 1 >= (3/2)^(0-1) = 2/3
 * R(1) = 1 >= (3/2)^(1-1) = 1
 * 
 * per n >= 2:
 * R(n) = 1 + R(n-2) + R(n-1) > R(n-2) + R(n-1) 
 *      >= (3/2)^(n-3) + R(n-1) 
 *      >= (3/2)^(n-3) + (3/2)^(n-2)
 *       = (3/2)^(n-2) (3/2^-1 + 1)
 *       = 5(3/2)^(n-2)
 *       
 * Poiché: 5/3 > 3/2
 * 
 * R(100) >= (3/2)^99 = 2.71 * 10^17
 * tempo = 2.7 * 10^8 secondi = 8,6 anni [inutilizzabile]
 * 
 */

public class Memoization {
    
    // procedura CONSIDEREVOLMENTE INEFFICIENTE.
    public static long fib (int n) {  // n > 1
        if (n < 2) {
            return 1;
        } else {
            return (fib(n-2)+fib(n-1));
        }
    }
    
    public static long fibMem(int n) {
        long[] mem = new long[n+1];
        
        for(int i = 0; i <= n; i++) {
            mem[i] = UNKNOWN;
        }
        
        return fibRec(n,mem);
    }
    
    public static long fibRec (int n, long[] mem) {  // n >= 0
        if (mem[n] == UNKNOWN) {
            if(n<2) {
                mem[n] = 1;
            } else {
                mem[n] = (fib(n-2,mem)+fib(n-1,mem));
            }
        }
        return mem[n];
    }
    
    private static final int UNKNOWN = 0;
}
