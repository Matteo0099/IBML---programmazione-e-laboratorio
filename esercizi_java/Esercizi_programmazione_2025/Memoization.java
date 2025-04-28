
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
                mem[n] = (fibRec(n-2,mem)+fibRec(n-1,mem));
            }
        }
        return mem[n];
    }
    
    private static final int UNKNOWN = -1;
    
    /**
     * Traduzione in java del problema scheme di llcs (longest common subsequence)
     */
    public static int llcs(String u, String v) {
        
        int m = u.length();
        int n = v.length();
        
        int[][] mem = new int[m+1][n+1];
        
        for(int i = 0; i <= m; i++) {
            for(int j = 0; j <= n; j++) {
                mem[i][j] = UNKNOWN;
            }
        }
        return llcsRec(u,v,mem);
    }
    
    private static int llcsRec(String u, String v, int[][] mem) {
        int i = u.length();
        int j = v.length();
        
        if(mem[i][j] == UNKNOWN) {
            if((i == 0) || (j == 0)) {
                mem[i][j] = 0;
            } else if (u.charAt(0) == v.charAt(0)) {
                mem[i][j] = 1 + llcsRec(u.substring(1), v.substring(1), mem);
            } else {
                mem[i][j] = Math.max(llcsRec(u.substring(1),v,mem), 
                                     llcsRec(u,v.substring(1),mem) ); 
            }
        }
        return mem[i][j];
    }
    
    
    /**
     * lcs (differente da llcs), non ricorsivo
     */
    
    public static String lcs(String u, String v) {
        int m = u.length(); 
        int n = v.length(); 
        
        String[][] mem = new String[m+1][n+1]; 
        for (int i = 0; i<=m; i++) {
            for (int j = 0; j<=n; j++) {
                mem[i][j] = null;
            }
        }
        
        return lcsRec(u,v,mem);
    }
    
    public static String lcsRec(String u, String v, String[][] mem) {
        int i = u.length(); 
        int j = v.length(); 
        
        if(mem[i][j] == null) {
            if((i == 0) || (j == 0)) {
               mem[i][j] = "";  
            } else if (u.charAt(0) == v.charAt(0)) {
                mem[i][j] = u.substring(0,1) + lcsRec(u.substring(1),v.substring(1),mem);
            } else {
                mem[i][j] = longer(lcsRec(u.substring(1),v,mem),
                                   lcsRec(u,v.substring(1),mem) ); 
            }
        }
        return mem[i][j];
    }
    
    private static String longer (String u, String v) {
        int m = u.length(); 
        int n = v.length(); 
        
        if(m < n) {
            return v; 
        } else if (m > n) {
            return u; 
        } else if (Math.random() < 0.5) {   // probabilità che è sempre diversa (50%)
            return v;
        } else {
            return u; 
        }
        
    }
    
    /**
     * lcs con programmazione dinamica (computazione efficiente)
     */
    public static String lcsDP(String u, String v) {
        int m = u.length();
        int n = v.length();
        
        int[][] mem = new int[m+1][n+1];
        
        // Inizializzazione prima riga
        for (int j = 0; j <= n; j = j + 1) {
            mem[0][j] = 0;
        }
        
        // Inizializzazione prima colonna
        for (int i = 1; i <= m; i = i + 1) {
            mem[i][0] = 0;
        }
        
        // Riempimento tabella DP
        for (int i = 1; i <= m; i = i + 1) {
            for (int j = 1; j <= n; j = j + 1) {
                if (u.charAt(m - i) == v.charAt(n - j)) {
                    mem[i][j] = 1 + mem[i - 1][j - 1];
                } else {
                    mem[i][j] = Math.max(mem[i - 1][j], mem[i][j - 1]);
                }
            }
        }
        
        String s = "";
        int i = m;
        int j = n;
        
        while (mem[i][j] > 0) {
            if (u.charAt(m - i) == v.charAt(n - j)) {
                s = s + u.charAt(m - i);
                i = i - 1;
                j = j - 1;
            } 
            else if (mem[i - 1][j] < mem[i][j - 1]) {
                j = j - 1;
            } 
            else if (mem[i - 1][j] > mem[i][j - 1]) {
                i = i - 1;
            } 
            else if (Math.random() < 0.5) {
                j = j - 1;
            } 
            else {
                i = i - 1;
            }
        }
        
        return s;
    }
    
    public static long manhDP(int i, int j) {
        
        long[][] mem = new long[i+1][j+1]; 
        
        for(int y = 0; y<=j; y++)
            mem[0][y] = 1;
        
        for(int x = 1; x<=i; x++)
            mem[x][0] = 1;
        
        // parto da 1 perchè lo 0 lo ho già calcolato, 
        // quindi sopra(x-1) e a sx(j-1) ci deve essere qualcosa. 
        // Matrice: (i+1) * (j+1)
        for(int x = 1; x<=i; x++) 
            for(int y = 1; y<=j; y++) 
                mem[x][y] = mem[x-1][y] + mem[x][y-1];
        
        return mem[i][j];
    }
}
