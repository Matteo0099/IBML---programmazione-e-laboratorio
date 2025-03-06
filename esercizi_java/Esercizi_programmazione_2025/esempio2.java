
/**
 Esempio2: plurali maschili/femminili
 */
public class esempio2 {
    public static String pluraleSm( String sm ) {
        return radiceSost(sm) + "i";   // stringa
    }
    public static String radiceSost( String s ) {
        return s.substring(0, s.length()-1);   // numero intero
    }
}