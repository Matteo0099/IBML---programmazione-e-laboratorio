
/**
 Esempio3: femminile di un sostantivo
 */
public class esempio3
{
    public static boolean femminile( String s) {
        return s.substring(s.length()-1).equals("a");
        // return s.charAt(s.length()-1)) == 'a';
    }
}