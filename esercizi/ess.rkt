;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Semplice prodotto da due numeri x,y.
(define product
  (lambda (x y)
    (if (or (= x 0) (= y 0))
        0
        (+ x (product x (- y 1)))
        )
    ))


(define fattoriale
  (lambda (n)
    (if (= n 0)
        1
        (* n (fattoriale (- n 1)))
        )
    ))


(define fibonacci
  (lambda (a)
    (if (< a 2)
        a
        (+(fibonacci (- a 1))(fibonacci (- a 2)))
        )
    ))


; programma per plurale di un sostantivo maschile
(define plurale
  (lambda (sm)
    (string-append (sost-m sm) "i")
 ))

(define sost-m
  (lambda (s)
    (substring s 0 (-(string-length s) 1))
 ))



; programma per verificare se un sostantivo, al singolare, e' femminile.
(define sost-fem
  (lambda (s)
    (string=?
     (substring s (- (string-length s) 1))
     "a"
     )
 ))


; programma per complemento-a-uno
; es: 1101 --> 0010
(define compl
  (lambda (bit)
    (if (string=? bit "0")
        "1"
        "0"
      )
  ))

(define compl-uno
  (lambda (seq)
    (if (> (string-length seq) 1)
        ;caso ricorsivo
        (string-append
          (compl (substring seq 0 1)) ; prende il primo bit "1011" -> "1"
          (compl-uno (substring seq 1)) ; prende tutto il resto "1011" -> "011"
         )
        ;caso base
        (compl seq)
     )
 ))



;input: "abcd"
;output:  "dcba"
; Ad ogni chiamata ricorsiva:
;  - si toglie il primo carattere della stringa,
;  - si "delega" la funzione al resto della stringa,
;  - il risultato del resto (già invertito) viene poi unito
;    con il primo carattere alla fine, usando string-append.
; In questo modo, l'ultima lettera diventa la prima
; e si costruisce la stringa al contrario.
;
; 1 chiamata: (string-append (string-rev "bcd") "a")
; 2 chiamata: (string-append (string-rev "cd") "b")
; 3 chiamata: (string-append (string-rev "d") "c")
; 4 chiamata: caso base, d.

(define s-rev
  (lambda (s)
    (if (< (string-length s) 2)
        s  ; caso base
        ; caso ricorsivo ==> abcd => a+bcd => ... => d
        (string-append
          (s-rev (substring s 1)) ;(delega) inverte parte restante stringa
          (substring s 0 1)       ; primo carattere alla fine
         )
     )
 ))


; costruisca una funzione ricorsiva che, conoscendo le dimensioni base di A0 e A1,
; calcoli le dimensioni dei fogli successivi, rispettando la relazione geometrica della serie A.
(define s0 (* 100 (sqrt 2)))    ; lato lungo A0
(define s1 (/ 100 (sqrt 2)))    ; lato lungo A1
  
(define formato
  (lambda (k)
    (if (< k 2)
        (if (= k 0)
            s0
            s1
         )
        (/ (formato (- k 2)) 2)
    )
))


; valore da binario a decimale (base 2 -> base 10)
; "1010" --> "10"
; "100" --> "4"
(define bin-val
  (lambda (bin) ; bin: stringa di 0/1
    (let ((k (- (string-length bin) 1)))
      (if (= k 0)
          (bit-val (string-ref bin k))
          (+ (* 2 (bin-val (substring bin 0 k))) ; rosso
             (bit-val (string-ref bin k))  ; verde
           ) 
       )
     )
 ))

(define bit-val
  (lambda (bit) ; bit: carattere #\0, #\1
    (if (char=? bit #\0)
        0
        1
     )
 ))



; esercizio inverso da numero a binario con bin-rep:
; "104" --> "1101000"
; "5" --> "101"
(define bin-rep ; val: stringa di 0/1
  (lambda (n)   ; n: intero non negativo
    (let (
          (q (quotient n 2))    ; 46 e non 46,5 (divisione intera)
          (r (remainder n 2))  ; resto della divisione per 2
         )
      (if (= q 0)  ; r >= 1:vuol dire che non c'è niente davanti l'ultima cifra
          (bit-rep r)
          (string-append  ; dal valore alla stringa
            (bin-rep q)
            (bin-rep r)
           )
        )
     )
 ))

(define bit-rep  ; val: stringa "0" oppure "1" (resto divisione per 2 = 0)
  (lambda (v)    ; v: 0 oppure 1
    (if (= v 0)
        "0"
        "1"
      )
  ))



; stringa palindroma (tanti modi per farlo ricorsivamente)
;Ragionamento iterativo (due indici)
"""
(lambda (s) ...) -->  definisce una funzione anonima che prende un argomento s, la stringa da controllare.
(let ((n (string-length s))) ...) -->  calcola la lunghezza della stringa una sola volta e la associa a n.
cond --> serve per gestire i casi multipli in modo ordinato:
Caso base: se n ≤ 1, la stringa è un palindromo (#t);
Caso ricorsivo: se il primo e l’ultimo carattere sono uguali, richiama palind sulla sottostringa interna ((substring s 1 (- n 1)));
Caso finale: altrimenti restituisce #f.
"""
(define palind
  (lambda (s)
    (let ((n (string-length s)))
      (cond ((<= n 1)
            #true )
           ((char=? (string-ref s 0) (string-ref s (- n 1)))  ; "prima e ultima posizione (estremi) della stringa"
             (palind (substring s 1 (- n 1)))  ; "parte centrale (centrali) della stringa"
            )
           (else #false)
       )
     )
  ))


; esercizio inverso da numero a binario con bin-rep:
(define num-rep  ; val: stringa di 0/1
  (lambda (n b)  ; n: intero positivo, b: base (2–16)
    (let (
           (q (quotient n b))     ;n di volte di un N in N (quoziente)
           (r (remainder n b))    ;resto r dopo la divisione (resto)
          )
      (if (= q 0)
          (dgt-rep r)     ; casi base
          (string-append
            (num-rep q b)
            (dgt-rep r)
           )
       )
     )
  ))

; trasforma in un numero in una stringa di 1 cifra.
(define dgt-rep
  (lambda (v)
    (cond
      ((< v 10) (string (integer->char (+ ascii-0 v)))) ; (0–9)
      (else
       (string (integer->char (+ ascii-A (- v 10))))  ; A–F (10-16)
       )
     )
  )) 

(define ascii-0 (char->integer #\0))   ; 0->48 in ascii
(define ascii-A (char->integer #\A))   ; base 2 < b < 16


; Bilancia Ternaria
; immaginiamo che mettiamo sulla bilancia ternaria dei pesi, però
;  47 g = 20 + 10 + 10 + 5 + 2 = 50 - (2 + 1).
;  1 3 9 27 = 40g (minimo pesi campione >= 4).
; btd: "-", ".", "+"
; 
(define btr-val   ; btr-val: intero
  (lambda (btr)   ; btr: stringa
    (let ((k (- (string-length btr) 1))) ; indice dell’ultima cifra
      (if (= k 0)
          (btd-val btr) ; caso base: 1 cifra
          (+ (* 3 (btr-val (substring btr 0 k)))  ; valore parte sx
             (btd-val (substring btr k (string-length btr))) ; ultima cifra
           )
        )
     )
 ))

; converte una singola cifra bilanciata in intero
(define btd-val ; val: [-1,0,+1]
  (lambda (btd)
    (cond ((string=? btd "-") -1)
          ((string=? btd ".") 0)
          ((string=? btd "+") 1)
     )
  ))


; Bilancia ternaria al "contrario"
; → si implementa il seguente programma, per i resti di divisione per 3 possibili (-2,-1,0,1,2)
; n = 3q + 2            <==>
; n = 3(q + 1) + 2 - 3  <==>
; n = 3(q+1) - 1        ; quel "-1" è il valore di (btd-rep)
(define btr-rep    ; converte un intero in stringa ternaria bilanciata
  (lambda (n)
    (if (< (abs n) 2)   ; caso base: -1, 0, +1    equivalente: (<= (abs n) 1)
        (btd-rep n)
        (let (
              (r (remainder n 3)) ; Resto: -2, -1 ,0, 1, 2
              (q (quotient  n 3)) ; N_volte; n = q3 + r
              )
          (cond
            ((= r -2)  ; n = 3(q-1) + 1
              (string-append (btr-rep (- q 1)) (btd-rep 1))
             )
            ((= r +2)  ; n = 3(q+1) - 1
              (string-append (btr-rep (+ q 1)) (btd-rep -1))
             )
            (else
             (string-append (btr-rep q) (btd-rep r)))
            )
         )
      )
  ))

(define btd-rep  ; val: "-", ".", "+"  :   converte una cifra bilanciata in simbolo
  (lambda (v)    ; v: -1,0,1
    (cond
      ((= v -1) "-")
      ((= v 0) ".")
      ((= v 1) "+")
     )
  ))


; vie manhattan --> paths(i,j-1) --> prima dx, poi giù.
(define vie-manhattan  ; val: due numeri reali interi positivi
  (lambda (i j)        ; i,j: interi pos.
    (cond
      ((= i 0) 1)      ; caso base 1: --> percorsi orizzontali ==> j passi
      ((= j 0) 1)      ; caso base 2: --> percorsi verticali ==> i passi
      (else
        (+ (vie-manhattan (- i 1) j)   ; (...) paths verso il basso 
           (vie-manhattan i (- j 1))   ; (...) paths verso destra
         )
       )
    )
 ))

; Problema Tassellazione 1 (cordolo lunghezza n=12 ==> 233 soluzioni)
(define tass-qr  ; val: intero
  (lambda (n)    ; n: intero+
    (cond ((= n 1)
            1)
          ((= n 2)
            2)
          (else  ; n > 2
            (+ (tass-qr (- n 1)) (tass-qr (- n 2)))
           )
     )
  ))

; Problema della Tassellazione 2 (n=12 ==> 377)
(define tass-br  ; val: intero
  (lambda (n)          ; n: intero positivo (lunghezza del cordolo)
    (cond ((= n 1) 2)
          ((= n 2) 3)
          ; 2,3,...,x  --> n=3==>5, n=6==> 21, se n=12 ==> 377
          (else  ; n > 2
            (+ (tass-qr (- n 1)) (tass-qr (- n 2)))   ; quadrata a sinistra / rettangolare a sinistra
           )
      )
  ))

; Programma per verificare se n è positivo o no.
(define verificaValAssoluto
  (lambda (n)
    (if (= (abs n) n)
        true
        false
     )
  ))


; Problema di Manhattan (i,j-1) o (i-1, j)
; (paths 5 5) -> 252 totali combinazioni di strade
(define paths   ; val: intero
  (lambda (i j) ; i,j: interi non negativi
    (if (or (= i 0) (= j 0))
        1
        ;else
        (+ (paths i (- j 1)) (paths (- i 1) j)) ; i,j>0
     )
 ))


; 1 IMPLEMENTAZIONE
; (es-similManhattan 0 0 3 2) --> 32 totali combinazioni
"""(define es-similManhattan
  (lambda (x y W H)     ; A(x,y) origine, B(W,H) traguardo.
    (cond
      ((or (> x W) (> y H)) 0)         ; fuori griglia
      ((and (= x W) (= y H)) 1)        ; arrivo
      (else
       (+ (es-similManhattan (+ x 1) y W H)
          (es-similManhattan (+ x 2) y W H)
          (es-similManhattan x (+ y 1) W H)
          (es-similManhattan x (+ y 2) W H)))
     )))"""


; 2 IMPLEMENTAZIONE (GIUSTA)
; (es-similManhattan 0 0 3 2) --> 32 totali combinazioni
(define es-simileManhattan
  (lambda (W H)
    (cond
      ((or (< W 0) (< H 0)) 0)       ; Caso base 1: Fuori dalla griglia
      ((and (= W 0) (= H 0)) 1)      ; Caso base 2: Arrivo (un percorso trovato)
      (else
       (+ (es-simileManhattan (- W 1) H)       ; Movimento: Destra di 1
          (es-simileManhattan (- W 2) H)       ; Movimento: Destra di 2
          (es-simileManhattan W (- H 1))       ; Movimento: Giù di 1
          (es-simileManhattan W (- H 2))       ; Movimento: Giù di 2
          )
       )
     )
  ))


; Numeri di Stirling del II tipo
(define stirling       ; val: intero
  (lambda (n k)  ; [1 <= k <= n] interi
    (if (or (= k 1)(= k n))
        1
        ;   15 + (3 * 25) = 90 modi per 6 pasticcini in 3 piatti
        (+ (stirling (- n 1)(- k 1)) (* k (stirling (- n 1) k)))
     )
 ))



; lunghezza della sottosequenza comune più lunga (llcs):
; 1.situazione:     llcs("", y) = llcs(x, "") = 0
; 2.situazione:     llcs(ax, ay) = 1 + llcs(x, y)
; 3.situazione:     llcs(ax, by) = max( llcs(x, by), llcs(ax, y) ) se a!=b

; NB: Potrebbe non essere possibile confrontare i due simboli,
;     per esempio quando manca 1 dei 2 simboli.
; (substring u 1) ---> (tolgo il primo carattere.)


; versione 1
(define llcs1       ; val: intero
  (lambda (u v)    ; u,v: stringhe
    (cond ((or (string=? u "") (string=? v "")) ; 2/2 vuote
           0
           )
          ((char=? (string-ref u 0) (string-ref v 0))  ; 1 carattere per entrambe
           (+ 1 (llcs1 (substring u 1) (substring v 1)))
           )
          (else
            (max
              (llcs1 (substring u 1) v)
              (llcs1 u (substring v 1))
             )
           )
      )
 ))

; versione 2 - finale
(define llcs2       ; val: stringa
  (lambda (u v)    ; u,v: stringhe
    (cond ((or (string=? u "") (string=? v "")) ; 2/2 vuote
           ""
           )
          ((char=? (string-ref u 0) (string-ref v 0))  ; 1 carattere per entrambe
            (string-append (substring u 0 1)
                           (llcs1 (substring u 1) (substring v 1))
             )
           )
          (else
            (longer
              (llcs1 (substring u 1) v)
              (llcs1 u (substring v 1))
             )
           )
      )
 ))

(define longer
  (lambda (u v)  ; stringhe
    (cond ((> (string-length u) (string-length v))
            u
           )
          ((< (string-length u) (string-length v))
            v
           )
          ((= (random 2) 0)  ; testa o croce (non deterministico)
            v
           )
          (else
            u
           )
     )
 ))
; oppure
(define longer2
  (lambda (u v)
    (let ((m (string-length u))
          (n (string-length v))
          )
      (cond ((< m n)
             v)
            ((> m n)
             u)
            ((= (random 2) 0)
             v)
            (else
             u)
       )
    )
 ))
    


;; **Liste in Scheme** (5 ingredienti fondamentali)

; 1. Lista vuota: null '()

; Aggiunta di elementi: cons  -> constratto
; (cons 5 (cons 10 (cons 11 null))) ...

; *****************
; Acquisizione 1* elemento e del resto una lista: car, cdr.
; *****************
;

; car+cdr = cons(inverso) -> (car (cons 5 (cons 10 null)))
; (cdr (cons 5 (cons 10 null))) = 10
; (cdr (cons 5 (cons 54 (cons 10 null)))) = 54 10

; Verifica se lista è vuota: null?   -> (null? (cons 5 null)) = false

; fino a dove si possono fare le liste in scheme:
; (1 2 3 4 5)
; (1 . (2 3 4 5))
; (1 . (2 . (3 4 5)))
; (1 . (2 . (3 . (4 5))))
; (1 . (2 . (3 . (4 .  (5)))))
; (1 . (2 . (3 . (4 .  (5 . ())))))

; (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 null)))))  = (list 1 2 3 4 5)


; DIFFERENZA LISTA VS QUOTE
; LISTA: LISTA DI ELEMENTI
; QUOTE: LISTA (DI DATI) SENZA VALUTAZIONE

; (list 1 (+ 1 1) (+ 2 1) (* 2 2) 5)  =  (list 1 2 3 4 5)
; '(1 (+ 1 1) (+ 2 1) (* 2 2) 5)      =  (list 1 (list '+ 1 1) (list '+ 2 1) (list '* 2 2) 5)

; NB: quello che segue l'apice, NON viene interpretato, ovvero non viene eseguita alcuna operazione.
; In alcuni casi tra LIST E QUOTE hanno lo stesso risultato, in altri no.
; E' PIU' PROBABILE USARE LIST. MOLTO RARAMENTE QUOTE.



;; list-ref  
; (lista-ref '(1 2 3 4 5) 2) ---> 3
; l'indice parte da 0, arriva fino a i-1 con i=length(lista)

(define lista-ref  ; val: tipo dell'elemento
  (lambda (s i)   ; s: lista, i: indice (pos. nella lista)
    (if (= i 0)
        (car s)
        (lista-ref (cdr s) (- i 1))
     )
 ))


;; length --> es(lunghezza)




;; append
(define giustapponi  ; val: 
  (lambda (s t)      ; s,t: liste
    (if (null? s)
        t
        (cons (car s) (giustapponi (cdr s) t))
     )
 ))

; append != cons
; la differenza fondamentale sta in 2 osservazioni:
; -append: ha 2 argomenti che sono entrambi liste. E' simmetrico
; -cons: il 1 argomento può essere una lista, mentre il 2 DEVE essere una lista. NON è simmetrico.
; esempio
; (cons 1 '()) != (cons '() 1) => error 
; (cons x y) = (append (list x) y)



;; reverse
(define rovescia-1  ; val: lista
  (lambda (s)     ; s: lista
    (if (null? s)
        null
        (append (rovescia-1 (cdr s)) (list (car s)))
     )
 ))

;; reverse ricorsivo
(define rovescia-list
  (lambda (s)
    (rovescia-rec s '())
 ))

(define rovescia-rec
  (lambda (s t)
    (if (null? s)
        t
        (rovescia-rec (cdr s) (cons (car s) t))
     )
 ))

; riempio s
; (1 2 3 4 5) ()
; (2 3 4 5) (1)
; (3 4 5) (1 2)
; (4 5) (1 2 3)
; (5) (1 2 3 4)
; () (1 2 3 4 5)
; restituisco t in caso vuota.



; differenza tra (substring u 1) e (substring u 0 1) con u = "ciao"
; ---> 



;; LCS: variazioni sul tema - (lcs+)
; input esempio: (lcs+ "albero" "blatta")
;     --> (list (list "b" 2 0))
;     --> (list (list "a" 0 2))
;     --> (list (list "l" 1 1))
; 
; NB-1: Assieme alla lettera in comune cè la posizione
;       della lettera nella prima e nella seconda stringa.
; NB-2: la lettera in comune può essere diversa.
; NB-3: si avrà lista di liste(**terne**).
 
(define lcs+         ; val: lista di terne <stringa, indice1, indice2>
  (lambda (u v)      ; u,v: stringhe
    (lcs-rec u 0 v 0)
 ))

(define lcs-rec      ; val: lista di terne <stringa, indice1, indice2>
  (lambda (u i v j)
    (cond ((or (string=? u "") (string=? v ""))                ; entrambe vuote
            null
          )
          ((char=? (string-ref u 0) (string-ref v 0))          ; 1* carattere uguale per entrambe
           ; aggiungo 
           (cons (list (substring u 0 1) i j)         ; (list "b" 2 0)
                       (lcs-rec (substring u 1) (+ i 1) (substring v 1) (+ j 1))
            )
          )
          (else
           (longer+                                             ; altrimenti iniziano con caratteri
                    (lcs-rec (substring u 1) (+ i 1) v j)            ; diversi e prendo la più lunga comune.
                    (lcs-rec u i (substring v 1) (+ j 1))
            )
           )
      )
  ))

(define longer+  ; val: lista di terne
  (lambda (u v)  ; u,v: lista di terne
    (let ((m (length u))
          (n (length v))
          )
         (cond ((< m n)
                v)
               ((> m n)
                u)
               ((= (random 2) 0)
                v)
               (else
                u)
          )
     )
  ))




;; All-LCS: Voglio vedere tutte le soluzioni possibili
;; input di esempio:
;; (all-lcs "palla" "pala") --> (list "pala")
;; (all-lcs "arto" "atrio") --> (list "ato" "aro" "aro")

(define all-lcs                                         ; val: lista di stringhe
  (lambda (u v)                                         ; u,v: stringhe
    (cond ((or (string=? u "") (string=? v ""))         ; caso base: entrambe vuote
            (list "")
          )
          ((char=? (string-ref u 0) (string-ref v 0))   ; 1* carattere uguale per entrambe
           ; aggiungo 
           (all-prefix (substring u 0 1)                ; (list "b" 2 0)
                       (all-lcs (substring u 1) (substring v 1))
            )
           )
          (else
           (all-longer                                  ; altrimenti iniziano con caratteri
                    (all-lcs (substring u 1) v)         ; diversi e prendo la più lunga comune.
                    (all-lcs u (substring v 1))
            )
           )
      )
 ))

(define all-prefix  ; val: lista di stringhe
  (lambda (p s)     ; p: stringa, s: lista di stringhe
    (if (null? s)
        null
        (cons (string-append p (car s))
              (all-prefix p (cdr s))
         )
     )
 ))

; definisco all-longer a partire da longer
(define all-longer    ; val: lista di stringhe
  (lambda (s t)       ; s,t: liste di stringhe
    (let ((m (string-length (car s)))
          (n (string-length (car t)))
          )
      (cond ((< m n)
             t)
            ((> m n)
             s)
            (else
             (append s t)
             )
         )
     )  
 ))



;; Test di primalità
;; definire se un numero n è primo

(define primo? ; val: booleano
  (lambda (n)  ; n >= 2 intero
    (not (divisori-in? n 2 (- n 1)))
    ; se non ha divisori, allora è primo.
    ; se il secondo estremo sx < estremo dx allora Int.=vuoto
  ))

(define divisori-in?  ; val: booleano
  (lambda (n a b)     ; n, a, b: interi positivi
    (cond ((> a b)    ; estr.sx < estr.dx
           false)
          ((= (remainder n a) 0) ; resto 0, ha divisori.
           true)
          (else
           (divisori-in? n (+ a 1) b)   ; int. più piccolo
           )
      )
   ))



;; Moltiplicazione "alla russa"
(define molt-russa  ; intero
  (lambda (m n)     ; m, n: interi positivi
    (cond ((= n 0)
          0)
          ((even? n)
           (molt-russa (* 2 m) (quotient n 2))) ; 2m * n/2 = m*n
          (else
           (+ m (molt-russa (* 2 m) (quotient n 2)))) ; 2m * (n-1)/2 = m*(n-1) = m*n-m
     )
  ))


;; Moltiplicazione del contadino russo
;; (molt-russa 7 9) -->  63
(define molt-cont     ; intero
  (lambda (m n)       ; m, n: interi positivi
    (molt-rec m n 0)
    ))


(define molt-rec    ; intero
  (lambda (m n p)   ; m, n, p: interi positivi
    (cond ((= n 0)
            p
           )
          ((even? n)
           (molt-rec (* 2 m) (quotient n 2) p))            ; 2m * n/2 = m*n
          (else
           (+ m (molt-rec (* 2 m) (quotient n 2) (+ m p))) ; 2m * (n-1)/2 = m*(n-1) = m*n-m
           ) 
      )
    ))


; Moltiplicazione russa tradotta in Imperativo
"""
int peasantMul(int m, int n) (
    int p = 0;                // (mul-rec m n 0)

    while (n > 0) (
       if (n % 2 == 0) (     // n: pari
         m = 2 * m;
         n = n / 2;
     ) else (                // n: dispari
         p = m + p;
         m = 2 * m;
         n = n / 2;
         
    )
)
"""



;; Cyclic-pattern
;; (cyclic-pt "abcabcabc" 3) --> "abc"

(define cyclic-pt      ; val: stringhe
  (lambda (s k)        ; s,k: ""
    (let ((n (string-length s)))
      (cond ((< n k)
             "")
            ((= n k)
             s)
            (else
             (let ((p (cyclic-pt (substring s k) k)))  ; Taglia la stringa s eliminando i primi k caratteri
               (if (string=? (substring s 0 k) p)
                   p
                   ""
                )
              )
            )
        )
      )
   ))


; MCD - ricorsione di coda
(define mcd
  (lambda (x y)
    (cond ((= x y)
           x
           )
          ((< x y)
           (mcd x (- y x))
           )
          (else
           (mcd (- x y) y)
           )
      )
 ))
; L'iterazione inizia, con es: 2, 4. --> 2, 2. ---> si ferma nel caso base x=y

; Programma iterativo equivalente
"""
int mcd(int x, int y) (
  while (x != y) (
     if (x < y) (
       y = y - x;
     )else (
       x = x - y;
     )
   )
   return x;
)
"""


; Problema di fibonacci  (coppie conigli fertili)
; F(i+2) = F(i) + F(i+1)   per i=>0
; Casi base:
; Casi ricorsivi:
; 1 coppia t = 0
; 2 coppia t = 1
; 3 coppie t = 2
; x coppie t = n
; INPUT: (+ (coppie-frt 12) (coppie-cucc 12))  -OUTPUT->  377

; INPUT: (+ (coppie-frt 2) (coppie-cucc 2))    -OUTPUT->  3
; Perchè: 
; 1 + 0 = 1   // 1* livello di ricorsione
; 1 + 1 = 2   // 2* livello di ricorsione
;       = 3   // tot

; INPUT: (+ (coppie-frt 3) (coppie-cucc 3))
; 2 + 1 = 3   // 1* livello di ricorsione
; 1 + 0 = 1   // 2* livello di ricorsione
; 1 + 0 = 1   // 3* livello di ricorsione
;       = 5   // tot

(define coppie-frt      ; val: interi+
  (lambda (s)            ; s: interi+
    (if (= s 0)
         1
         (+ (coppie-frt (- s 1)) (coppie-cucc (- s 1)))
      )
 ))

(define coppie-cucc
  (lambda (s)
    (if (= s 0)
        0
        (coppie-frt (- s 1))
     )
 ))



; Criptazione (Procedura con argomenti procedurali)
(define encrypt        ; val: stringa
  (lambda (msg rule)   ; msg: stringa, rule: procedure [caratt->caratt]
    (if (string=? msg "")
         ""
        (string-append
          (string (rule (string-ref msg 0)))
          (encrypt (substring msg 1) rule)
         )
     )
  ))
; (encrypt "PROGRAMMAZIONE" (lambda (c) c)) --> "PROGRAMMAZIONE"
; (encrypt "PROGRAMMAZIONE" char-downcase) --> "programmazione"
; (encrypt "PROGRAMMAZIONE" (lambda (c) (integer->char (+ (char->integer c) 1)))) --> "QSPHSBNNB[JPOF"



; Algoritmo di Cesare vers. 1 (Mir0l0)
(define ascii-Z (char->integer #\Z))
(define caesar-cipher-3     ; val: char (lettera maiuscola)
  (lambda (c)               ; c: char (lettera maiuscola)
    (let ((k (+ (char->integer c) 3))
          )
      (if (> k ascii-Z)
          (integer->char (- k 26))
          (integer->char k)
       )
      )
  ))

; Aumenta di 3 la rotazione del carattere.  (M0nic4)
(define aA (char->integer #\A))  ; costanti
(define aZ (char->integer #\Z))
(define n-car 26)

(define rgl-cesare  ; val: char (lettera maiuscola)
  (lambda (crt)     ; crt: char (lettera maiuscola)
    (let ((new-ascii (+ 3 (char->integer crt)))
          )
      (if (<= new-ascii aZ)
          (integer->char new-ascii)
          (integer->char (- new-ascii n-car))
          )
      )
   ))
; (rgl-cesare #\M) --> #\P

; Algoritmo "Encrypt" unito a quello di Cesare:
; (encrypt "ALEAIACTAEST" caesar-cipher-3) --> "DOHDLDFWDHVW"


(define caesar-cipher
  (lambda (rot)
    (lambda (c)
      (let ((k (+ (char->integer c) rot))
            )
        (if (> k ascii-Z)
            (integer->char (- k 26))
            (integer->char k)
         )
       )
      )
   ))


; Procedure con argomenti e valori procedurali
(define caesar-undoer
  (lambda (rule)
    (let ((rot (- (char->integer (rule #\A)) ascii-A))
          )
        (caesar-cipher (- 26 rot))
      )
  ))

; input: (define enc (caesar-cipher 3))
; input: (define dec (caesar-undoer enc))
; (encrypt "ALEAIACTAEST" enc) --> "DOHDLDFWDHVW"    //criptazione
; (encrypt "DOHDLDFWDHVW" dec) --> "ALEAIACTAEST"    //decriptazione



; Esempio
; voglio una procedura che riesce a "tornare indietro" rispetto a qualsiasi criptazione di messaggi.
(define perm-undoer   ; val: procedura [caratt --> caratt]
  (lambda (rule)      ; rule: procedura [caratt --> caratt]

    (lambda (c)
      (invert c rule 0)
      )
    
 ))

(define invert        ; val: 
  (lambda (c rule k)  ; c: car, rule: [caratt-->caratt], k: posizione.
    (if (char=? (rule (integer->char k)) c)
        (integer->char k)
        (invert c rule (+ k 1))
     )
  ))


; comando con argomenti procedurali:
; ==> map <==
; esempi:
; (map (lambda (x) (+ x 1)) '(1 2 3 4 5)) --> (list 2 3 4 5 6)
; (map char-downcase '(#\A #\B #\C))      --> (list #\a #\b #\c)
; (map integer->char '(48 49 50 51))      --> (list #\0 #\1 #\2 #\3)

;; Procedura predefinita map fatta "a mano"
(define mappa
  (lambda (f s)
    (if (null? s)
        null
        (cons (f (car s)) (mappa f (cdr s)))    ; primo elemento + tutto il resto
     )
  ))

; espansione:
;  (cons (f 1) (mappa f '(2 3)))
;  (cons 2     (cons (f 2) (mappa f '(3))))
;  (cons 2     (cons 4     (cons (f 3) (mappa f '()))))
;  (cons 2     (cons 4     (cons 6     '())))

; (map char-downcase '(#\A #\B #\C))   --> (list #\a #\b #\c)
; (mappa char-downcase '(#\A #\B #\C)) --> (list #\a #\b #\c)



; map applicato ad all-lcs:
; (all-lcs-2 "arto" "atrio") --> (list "ato" "aro" "aro")  //problema ripetizione
; (all-lcs-2 "saltimbanco" "emblematico") --> (list "mbaco" "mbaco" "mbaco" "mbaco" "mbaco" "mbaco" "mbaco" "mbaco" "mbaco" "mbaco" "mbaco" "mbaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "ltico" "mbaco" "mbaco" "mbaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "ltico" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "ltico" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "ltico" "atico" "mbaco" "mbaco" "mbaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "ltico" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "ltico" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "ltico" "atico" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "ltico" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "ltico" "atico" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "lmaco" "ltico" "atico" "atico" "atico" "atico")

(define all-lcs-2                                ; val: lista di stringhe
  (lambda (u v)                                  ; u,v: stringhe
    (cond ((or (string=? u "") (string=? v ""))  ; caso base: entrambe vuote
            (list "")
           )
          ((char=? (string-ref u 0) (string-ref v 0))   ; 1* carattere uguale per entrambe
           ;(all-prefix (substring u 0 1)
           (map (lambda (x) (string-append (substring u 0 1) x))
                (all-lcs-2 (substring u 1) (substring v 1))
                )
            )
          (else
           (all-longer-2                                  ; altrimenti iniziano con caratteri
                    (all-lcs-2 (substring u 1) v)         ; diversi e prendo la più lunga comune.
                    (all-lcs-2 u (substring v 1))
            )
          )
      )
 ))

; definisco all-longer con la procedura "merge" per risolvere il problema delle ripetizioni.
; definisco all-longer a partire da longer
(define all-longer-2    ; val: lista di stringhe
  (lambda (s t)       ; s,t: liste di stringhe
    (let ((m (string-length (car s)))
          (n (string-length (car t)))
          )
      (cond ((< m n)
             t)
            ((> m n)
             s)
            (else
             ;(append s t)
              (merge-2 s t)
             )
         )
     )  
 ))

(define merge-2     ; val: lista di stringhe senza ripetizioni
  (lambda (s t)   ; s,t: lista di stringhe senza ripetizioni
    (if (null? s)
        t
        (cons (car s) (merge-2 (cdr s) (remove (car s) t)))   ; remove: rimuove un specifico elemento da t.
     )
 ))

; (remove 3'(1 2 3 4 5)) --> (list 1 2 4 5)
; riprovo all-lcs-2 (senza ripetizioni)
; (all-lcs-2 "saltimbanco" "emblematico") --> (list "mbaco" "lmaco" "ltico" "atico")    ; funziona!



; Funzioni di ordine superiore --> Composizione di funzioni
; [f:D -> E, g:E -> F] ==> gof(x)= g(f(x)) per ogni x app. a D.

(define comp      ; val: procedura  [D -> F]
  (lambda (g f)   ; f:D -> E, g:E -> F  (procedure)
    (lambda (x) (g (f x)))
 ))

; Esempio: 
; (define incr-1 (lambda (x) (+ x 1)))
; (define double (lambda (x) (* x 2)))
; (define h (comp incr-1 double))
; (h 5) --> 11
; che è lo stesso che fare: ((comp (lambda (x) (+ 1 x)) (lambda (x) (* 2 x))) 5)



; Iterata di una funzione: f:D->D  con i app. a N (n naturali) t.c.
; f^i (x) =
; x            se i = 0
; f(f^i-1 (x)) se i > 0

(define iter
  (lambda (f i)   ; f:D -> D (procedura), i: intero+
    (lambda (x)
      (if (= i 0)
          x
          (f ((iter f (- i 1)) x))
       )
     )
 ))

(define q
  (lambda (x)
    (+ 1 (/ 1 x))
 ))

((iter q 0) 1)
((iter q 1) 1)
((iter q 2) 1)
((iter q 3) 1)
((iter q 4) 1)
((iter q 5) 1)
((iter q 6) 1)
((iter q 10) 1)
((iter q 20) 1)
((iter q 30) 1)
((iter q 40) 1)
; converge alla sezione aurea (numero irrazionale) --> (/ (+ 1 (sqrt 5)) 2)
