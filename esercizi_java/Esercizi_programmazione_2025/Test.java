public class Test {
    
    public static IntSList intervallo(int inf, int sup) {
        if (inf > sup) {
            return IntSList.NULL_INTLIST;
        } else {
            return intervallo(inf + 1, sup).cons(inf);
        }
    }

    public static int gFlavio(int n) { // n > 0
        
        TavRotonda tav = new TavRotonda(n); // Costruttore corretto

        while ( tav.numCavalieri() > 1 ) {
            
            tav = tav.serve(); // Metodo chiamato sull'istanza
            tav = tav.passa(); // Verifica se il metodo è "passa" o "passa"
        }
        return tav.cavConBrocca();
    }
    
}  // class Test