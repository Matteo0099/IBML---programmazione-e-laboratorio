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


; Problema di Manhattan (i,j-1) o (i-1, j)  ---- Ricorsione ad Albero != Ricorsione di coda
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


; Numeri di Stirling del II tipo  (pasticcini sui piatti -- 6 pasticcini su 3 piatti)
(define stirling       ; val: intero
  (lambda (n k)        ; [1 <= k <= n] interi
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
  (lambda (s t)     ; s,t: lista di stringhe senza ripetizioni
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

; Esempio 1: 
; (define incr-1 (lambda (x) (+ x 1)))
; (define double (lambda (x) (* x 2)))
; (define h (comp incr-1 double))
; (h 5) --> 11
; che è lo stesso che fare: ((comp (lambda (x) (+ 1 x)) (lambda (x) (* 2 x))) 5)

; Esempio 2:
; (define flor (lambda (x) (* x x)))
; (define sum (lambda (x) (+ x x)))
; (define gtr (comp flor sum))
; (gtr 10) --> 400



; Iterata di una funzione: f:D->D  con i app. a N (n naturali) t.c.
; f^i (x) =
; x            se i = 0
; f(f^i-1 (x)) se i > 0
; dentro iter i due pezzi equivalgono

(define iter
 (lambda (f i)   ; f:D -> D (procedura), i: intero+
;   (lambda (x)
;      (if (= i 0)
;          x
;          (f ((iter f (- i 1)) x))
;       )
;     )
   (if (= i 0)
       id                         ; lambda: procedura
       (comp f (iter f (- i 1)))  ; compongo f con l'iterata f^i-1
       )
 ))

(define id (lambda (x) x))

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
((iter q 100) 1) ; si stabilizza l'approssimazione(aspetto matematico)
; converge alla sezione aurea (numero irrazionale) --> (/ (+ 1 (sqrt 5)) 2) = 1.5(periodico)



; Funzione polinomiale P(x) = c0 + c1*x^1 +...+ ck*x^k = sommatoria da i=0 a k (ci*x^i)
; P(x) definita in:
;  - c0          se k = 0
;  - c0 + x*Q(x) se k > 0
; Con Q(x) = c0 + c1*x^1 +...+ ck*x^k = sommatoria da i=1 a k (ci*x^i-1)

(define P                                   ; val: intero+
  (lambda (c k)                             ; c,k: lista, intero+
    (cond ((null? c)                        ; caso base(1)
           0   
          )
         (else
           (+ (car c) (* k (Q (cdr c) k)))  ; caso ricorsivo(2)
          ; c1 + x * Q(x) 
          )
      )
  ))

(define Q
  (lambda (c k)
    (cond ((null? c)
           0
           )
          (else
            (+ (car c) (* k (Q (cdr c) k)))
           )
      )
  ))

; Esempio input:
; P(x) = 3x^2 + 2x + 1
; (P '(1 2 3) 2)  --> 17    // viene messo il 2 al posto della x in f(x) e viene calcolato. 



; Calcolo x^2 e 2i-1 (Proprietà da dimostrare, per induzione)
(define unknown   ; val: intero       x^2
  (lambda (x)     ; x: intero+
    (if (= x 0)
        0
        (+ (unknown (- x 1)) (odd x))
      )
  ))

(define odd       ; val: intero       2i-1
  (lambda (i)     ; x: intero+
    (if (= i 1)
        1
        (+ (odd (- i 1)) 2)
      )
  ))


; Esempio dimostrazione:
;
; Vn app.a N   (odd n) -*-> 2n-1
; caso base: (odd 1) -> 2*1-1 = 1
; ip. ind. : (odd k) -> 2*k-1
; pass.ind.: (odd k+1) -> 2(k+1)-1 = 2k+1
;
; valutazione programma:
; (odd k+1) -> (if (= k+1 1) 1
;                  (+ (odd (- k+1 1)) 2))
; (+ (odd (- k+1 1)) 2)
; (+ (odd k) 2) -*-> (+ 2k-1 2)
; --> 2k+1
;
; R) La dimostrazione si è conclusa con la verifica della proprietà 2n-1


; Sintassi da usare:
; --> numero finito di passi
; -*-> numero indeterminato di passi


; Proprietà generale da dimostrare: Vn app.aN (unknown n) -> n^2
; caso base: (unknown 0) -*-> 0^2 = 0
;            (unknown 0) -*-> (if (= 0 0) 0 ...) --> 0
; ipo. ind.: (unknown n) -*-> n^2   (assumo che...)
; pass. ind: (unknown n+1) -*-> (n+1)^2
; dim. ind.: (unknown n+1) --> (if (= n+1 0) 0 (+ ...))
;       -->  (+ (unknown (- n+1 1))
;               (odd n+1))
;       -->  (+ (unknown n) (odd n+1))
; per l'ipotesi induttiva: (+ n^2 (odd n+1))
; per la proprietà dimostrata:
;       --> (+ n^2 2n+1)
;       --> (n^2+2n+1) = (n+1)^2     // quadrato di binomio --> vale n e per n+1 ovvero Vn.


; Ricorsione -> delega  (top-down)  ==> simile a induzione n+1


; Funzione potenza x^y
(define power    ; val: intero
  (lambda (m n)  ; x > 0, y: interi+
    (if (= n 0)
        1
        (* m (power m (- n 1)))
     )
 ))

; Dimostrazione per ind. formale:
; 2 argomenti, m,n > 0 con n app.aN -> m^n
; casi base: (power m 0) -> m^0 = 1
;  m app. [1,2,3,...] e n=0: (power m n) -> m^n (*)
; dim. p.in: (power m n+1) -> m^n+1
;  m app. [1,2,3,...] e n=k+1: (power m n) -> m^n (*)
;            (power m n+1)
;        --> (if (= n+1 0) 1 (* m ...))
;        --> (... ...)


; ------------------------------------------------------------------
; Dimostrazione su mul-tr (tail-recursive)
; Per ogni m,n,p app.a N  (mul-tr m n p) -> mn+p
; induzione su...? --> n+1
; ipotesi: (mul-tr m 0 p) -*-> p
; ipotesi induttiva: considero n app.a N  (mul-tr m n p) -> mn+p
; proprietà da dimostrare come passo induttivo:
; Vm,p app.a N ==> (mul-tr m n+1 p) -*-> m(n+1)+p
; dim. casi base: 
; ------------------------------------------------------------------
; a) n+1 pari
; (cond ((even? n+1) ...) ...)
; (mul-tr (* 2 m)
;         (quotient n+1 2) p)
; (mul-tr 2m (n+1) / 2 p)
; siccome 2m e p sono naturali e (n+1)/2 app.a [0,n], posso applicare l'ipotesi induttiva:
; 2m (n+1) / 2 + p = [m(n+1) + p]
; ------------------------------------------------------------------
; b) n+1 dispari
; (cond (else ...)
; (mul-tr (* 2 m)
;         (quotient n+1 2)
;         (+ m p))
; (mul-tr 2m n/2 m+p)
; [(quotient n+1 2) -> n/2 poiché n+1 dispari]
; siccome 2m e m+p sono naturali e n/2 app.a [0,n], posso applicare l'ipotesi induttiva:
; --> 2m n/2 + m + p = mn + m + p = [m(n+1) + p]
; CVD
; ------------------------------------------------------------------



; Dimostrazione: percorsi di Manhattan
; (paths i j) --> ?
; Dobbiamo fare l'ipotesi induttiva su AMBO i parametri (non si può escludere 1 dei 2).
; NB: i casi base della ricorsione possono NON essere i casi base dell'induzione.
;
; (paths i j) --> (i+j)! / (i! * j!)  --> [(i-1)+(j-1)] * [(i-2)+(j-2) * ... * (i-n)+(j-n)]   --> [calcolo combinatorio]
; k = i+j --> "Misura di difficoltà"  k app.a N, m,napp.aN t.c (m + n = k) 
; Proprietà generale da dimostrare:
;   Vk app.a N Vm,n app.a N t.c (m + n = k)
;   (paths m n) -*-> (m+n)! / (m! * n!)
; Caso base: k = 0  ==> (paths 0 0) -*-> (0+0)! / (0! * 0!) = 1  [ok]   ---   [0! = 1]
; Ipotesi induttiva: considero k app.a N e assumo che:
;   Vk app.a N Vm,n app.a N t.c (m + n = k) vale: (paths m n) -*-> (m+n)! / (m! * n!)
; Dimostraz. passo induttivo:
;   Vk app.a N Vm,n app.a N t.c (m + n = k + 1)
;   (paths m n) -*-> (m+n)! / (m! * n!)
;   
;   (paths m n)
;     --> (if (or (= n 0) (= m 0)) ...)
;    a) suppongo n = 0
;     --> 1 = (m+0)! / (m! * 0!)  [ok]
;    b) suppongo m = 0
;     --> 1 = (0+n)! / (0! * n!)  [ok]
;    c) suppongo m,n > 0                  ; tanti passaggi quante sono le ricorsioni.
;     --> (+ (paths m (- n 1))
;            (paths (- m 1) n))
;     --> (+ (paths m n-1)         [m+n-1 = k]-> rientro nell'ipotesi induttiva
;            (paths (- m 1) n))
;     --> (+ (m+n-1)! / (m!*(n-1)!)
;            (paths (- m 1) n))    [m-1+n = k]-> rientro nell'ipotesi induttiva
;     --> (+ (m+n-1)! / (m!(n-1)!)
;            (m-1+n)! / ((m-1)!*n!))
;       = (m+n-1)! / (m!(n-1)!) + (m-1+n)! / ((m-1)!*n!))
;       = (m+n-1)!*(n+m) / (m!*n!)
;       = (m+n)! / (m!*n!)   [ok]
;     
; Conclusione: Seguire lo schema generale dell'induzione, seguendo le ricorsioni del programma (albero/di coda).




; Dimostrazione problema di Stirling (pasticcini)
; Proprietà per n=>2 Vn app.a N ==> (st n 2) -*-> [2^n-1 - 1]
; Proprietà per n=>2 Vn app.a N ==> (st n n-1) -*-> [n(n-1) / 2]
;  a)-caso base:
;     (st 2 2) --> 2^2-1 - 1 = 1
;    -ipotesi induttiva: considero n app.a N tolto [0,1] e assumo che.
;     (st n 2) --> 2^n-1 - 1
;    -passo induttivo: per n considerato
;     (st n+1 2) ---> 2^(n+1)-1 - 1 = 2^n - 1
;     (st n+1 2)
;      ---> (+ (st (- n+1 1) (- 2 1))
;              (* 2 (st (- n+1 1) 2)))
;      ---> (+ (st n 1)
;              (* 2 (st (- n+1 1) 2)))
;      ---> (+ 1 (* 2 (st n 2)))
;      ---> (+ 1 (* 2 2^n-1 - 1)) --> 2^n - 1
;  b) -caso base: 
;      (st 2 2-1) --> 2(2-1) / 2 = 1
;     -ipotesi induttiva: considero n app.a N tolto [0,1] e assumo che:
;      (st n n-1) --> n(n-1) / 2
;     -passo induttivo: per n considrato
;      (st n+1 n+1-1) --> (n+1)*n / 2
;      (st n+1 n) 
;       → (+ (st (- n+1 1) (- n 1))
;            (* n (st (- n+1 1) n)))
;       → (+ (st n n-1)
;            (* n (st (- n+1 1) n)))
;       → (+ n(n-1)/2 (* n (st n n)))
;       → (+ n(n-1)/2 n)
;       → (n+1)n/2


; In caso vogliamo confutare una tesi, BISOGNA trovare un controesempio.


; Dimostrazione per induzione programma "UFO" --> Unified Flying Procedure.
; ufo(n) restituisce il massimo numero dispari ≤ n
; Quoziente = risultato tra Dividendo : divisore
"""
(define ufo            ; valore: ?
  (lambda (x)          ; x > 0 naturale
    (cond ((= x 1) 1)
          ((even? x)   ; x pari
           (- (* 2 (ufo (quotient x 2))) 1))   ; se pari +1
          (else        ; o no... (x dispari)
           (+ (* 2 (ufo (quotient x 2))) 1))   ; se dispari -1
      )
  ))

"""
; (ufo n) --> ??
; -Proprietà della funzione f calcolata dalla procedura ufo:
; -(ufo n) --> f(n) con n app.a N+
; a) Se n dispari:  Vn app.a N t.c. (ufo n) --> 2f(n)+1
; b) f(n) <= n
; c) f(0) = 1  e  f(n) = f(n-1)+2 se n non è una potenza di 2.  es n=6 (pari) ==> f(5)+2 = 11 (dispari)
; [V k >= 0]
; d) f(2^k) = 1    
; e) f(2^k+1 - 1) = 2^k+1 - 1
;
; -d)Proprietà generale da dimostrare: [con n=2^k] Vk app.a N  (ufo 2^k) -*-> 1
; -caso base: (ufo 2^0) -*-> 1
; -ipotesi induttiva: considero k app.a N
;  assumo che: (ufo 2^k) -*-> 1    (!!!senza quantificazione, ovvero [k app.a N])
; -dimostro il passo induttivo: per k considerato dimostro:
;  (ufo 2^k+1) -*-> 1
; -dimostro il passo induttivo:
;  (cond ((even? k+1) ...) ...)    !!!sicuramente pari
;   --> (- (* 2 (ufo (quotient 2^k+1 2))) 1))       
;   --> (- (* 2 (ufo (quotient 2^k))) 1)    --> vorrei applicare l'ipotesi induttiva: 2^k -*->1
;   --> (- (* 2 1) 1)  //per l'ipotesi induttiva
;   --> (- 2 1) = 1
;
; -e)Proprietà generale da dimostrare: [n = 2^k+1 - 1]: Vk app.a N  (ufo 2^k+1 - 1) -*-> 2^k+1 - 1
; -
; -
; -
; -
; -
; -
; -
;
;  - proprieta' generale (comprende tutte le precedenti):
;            ogni k >= 0 : ogni j in [0,2^k-1] : (ufo 2^k+j) -*-> 2j+1
;          dove i valori k, j sono numeri naturali
;        - impostazione di riferimento:
;            k     j                           n = 2^k+j
;            0     [0]                         [1]
;            1     [0, 1]                      [2, 3]
;            2     [0, 1, 2, 3]                [4, 5, 6, 7]
;            3     [0, 1, 2,  ...,  6,  7]     [8, 9, 10, ..., 14, 15]
;            4     [0, 1, 2,  ..., 14, 15]     [16, 17,   ..., 30, 31]
;            ...   ...                         ...
;
;            k     n = 2^k+j                   2j+1  (nell'ordine)
;            0     [1]                         [1]
;            1     [2, 3]                      [1, 3]
;            2     [4, 5, 6, 7]                [1, 3, 5, 7]
;            3     [8, 9, 10, ..., 14, 15]     [1, 3, 5,  ..., 13, 15]
;            4     [16, 17,   ..., 30, 31]     [1, 3, 5,  ..., 29, 31]
;            ...   ...                         ...
;
;  2.2. Dimostrazione per induzione su k, nei naturali:
;
;       - casi base: ogni j in [0,2^0-1] : (ufo 2^0+j) -*-> 2j+1
;           (ufo 2^0+0)
;             -*-> (cond ((= 1 1) 1) ... )
;             -*-> 1 = 2j+1  perche' j = 0  (unico valore in [0,2^0-1] = [0,0])
;       - ipotesi induttiva, preso un valore naturale k:
;           ogni j in [0,2^k-1] : (ufo 2^k+j) -*-> 2j+1

;       - passo induttivo, per k scelto sopra e per ogni j in [0,2^(k+1)-1]:
;           (ufo 2^(k+1)+j)
;             -*-> (cond ((= 2^(k+1)+j 1) 1) ((even? 2^(k+1)+j) ... ) (else ... ))
;             -*-> (cond (false 1) ((even? 2^(k+1)+j) ... ) (else ... ))
;             -*-> ?
;   ---- sottocaso (a): 2^(k+1)+j pari,
;         da cui consegue j pari (anche 2^(k+1) e' pari) e inoltre j <= 2^(k+1)-2
;             -*-> (- (* 2 (ufo (quotient 2^(k+1)+j 2))) 1)
;             ---> (- (* 2 (ufo 2^k+(j/2))) 1)      ; poiche' k+1 > 0 e j pari
;             -*-> (- (* 2 (2(j/2)+1)) 1)           ; per l'ipotesi induttiva,
;              infatti j/2 in [0,2^k-1] poiche' j <= 2^(k+1)-2
;             ---> (- 2(j+1) 1)
;             ---> 2(j+1) - 1 = 2j+1   [ok]
;
;   ---- sottocaso (b): 2^(k+1)+j dispari
;         da cui consegue j dispari (2^(k+1) e' pari)
;             -*-> (+ (* 2 (ufo (quotient 2^(k+1)+j 2))) 1)
;             ---> (+ (* 2 (ufo 2^k+((j-1)/2))) 1)  ; poiche' k+1 > 0 e j dispari
;             -*-> (+ (* 2 (2((j-1)/2)+1)) 1)       ; per l'ipotesi induttiva,
;               infatti (j-1)/2 in [0,2^k-1] poiche' 1 <= j <= 2^(k+1)-1
;              ---> (+ 2((j-1)+1) 1)
;              ---> (+ 2j 1) = 2j+1   [ok]
;
; Note a margine.
;  3.1. Formula chiusa per il valore f(n) restituito dalla
;      procedura "ufo" applicata all'argomento intero n > 0:
;
;       - n = 2^k + j,  dove  k = floor( log n )  e  j = n - 2^floor( log n )
;       - f( n ) = 2 ( n - 2^floor(log n) ) + 1      [log in base 2]
;
; Vederla come in chiave del programma complemento bit a 1:
;  n - compl_a_1(n) =
;  n-((2^k+1 - 1) - n) =
;  2n - 2^k+1 + 1 =
;  2(n-2^k) + 1 =  2j+1  [ok]





; ------------ fine 1 parte corso ---------------- ;
; Esercizi basati su temi d'esame ;
; ------------------------------------------------ ;


; :) Prova del 21 gen 2025 - 1 p. acc.
;
; 1 Esercizio
; f:DxD -> D  x,y app.a D e intero k>=0
; p = lista di k elementi di D
; Casi base:
;  x se k>0
;  y se k>1
;  else ==> (k>2) ==> risultante di f applicata ai due precedenti
; es: (p f x y k) --> (e1,e2,e3,...,ek) dove e1 = x, e2 = y, ei=f(ei-2, ei-1) per i>2
;
; esempi
; (p + 1 2 12)                  —> (1 2 3 5 8 13 21 34 55 89 144 233)
; (p string-append "q" "b" 6)   —> ("q" "b" "qb" "bqb" "qbbqb" "bqbqbbqb")
; (p list null null 5)          —> ( () ()  (() ())  (() (()))  ((() ()) (() (() ()))) )
;

(define p             ; val: lista
  (lambda (f x y k)   ; f=funzione, x=el, y=el, k=intero+
    (cond ((= k 0)
            null
           )
          ((= k 1)
            (list x)
           )
          ((= k 2)
            (list x y)
           )
          (else
            (cons x (p f y (f x y) (- k 1)))   ; applico f ad (x,y) e poi abbasso k di 1 ogni volta.
           )
       )
  ))



; es.2) [DA COMPLETARE] Variante problema "sottosequenza comune più lunga (LCS)"
; risultato è una coppia di liste di interi (lcps u v) --> (p q) 
; esempio input:
; (lcps "atrio" "arto")               --> ((1 2 5) (1 3 4))
; (lcps "AGACTGAACATAC" GATCCGACTAC") --> ((2 3 4 6 7 9 11 12 13) (1 2 4 6 7 8 9 10 11))

(define lcps        ; val: coppie liste interi+
  (lambda (u v)     ; u,v: stringhe
    (rec-proc 1 u 1 v)
    ))

(define rec-proc
  (lambda (i u j v)  ; i,j => 0 indici interi; u,v: stringhe. 
    (cond ((or (string=? u "") (string=? v ""))
            (list '()'())
           )
          ((char=? (string-ref u 0) (string-ref v 0))
           (let ((x (rec-proc (+ i 1) (substring u 1) (+ j 1) (substring v 1)))
                 )
                (list (cons i (car x)) (cons j (cadr x)))  ;(car (cdr x))
            ))
          (else
           (let ((x (rec-proc (+ i 1) (substring u 1) j v))
                 (y (rec-proc i u (+ 1 j) (substring v 1)))
                 )
             (if (> (length (car x)) (length (car y)))
                  x
                  y
                 )
           ))
      )
  ))

; SPIEGAZIONE CAR / CDR
; (rec-proc 2 "trio" 2 "rto") --> ((2 5) (3 4))
; (car x)      : (2 5)
; (cdr x)      : ((3 4))
; (car (cdr x)): (3 4)





; Programma tail-rec:
(define f
  (lambda (n)
    (tail-rec 1 0 n)
 ))

(define tail-rec
  (lambda (i j k)
    (if (= k 0)
        j
        (tail-rec (+ i 2) (+ i j) (- k 1))
      )
  ))

; 3) Verifica formale della correttezza
; funzione f, e tail-rec
; V x,y,z app.a N (tail-rec x y z) ---> y + z(x+z-1) (*)
; N = insieme numeri Naturali

; Casi base:
; V x,y app.a N (tail-rec x y 0) --> y + 0 (x+0-1) = y

; Ipotesi induttiva: consideriamo un certo z app.a N e assumiamo
; V x,y app.a N (tail-rec x y z) --> y + z (x+z-1)

; Passo induttivo: per z considerato e voglio dimostrare
; V x,y app.a N (tail-rec x y z+1) --> y + (z+1) (x+z+1-1) = y + (z+1)(x+z)

; [z considerato, V x,y app.a N]

; (tail-rec x y z+1)
;  --> (if (= z+1 0) ... ...)
;  --> (if (= z+1 0) ... ...)
;  --> (tail-rec x+2 x+y z)     //z è quello per cui ho fatto l'ipotesi induttiva****
;  -->  x+y + z (x+2+z-1)
;         = y + (z+1)(x+z) ?
; y + xz + x + zz + z // x + y + xy + zz + z   -----------> sono gli stessi addendi
; CVD (dimostrato)

; Dimostra che V n app.a N (f n) -> n^2   [NO INDUZIONE]
; (f n)
; Caso base:  --> (tail-rec 1 0 n) --> 0 + n(1+n-1) --> n + nn - n --> n^2  [ok]



; 4) Ultimo esercizio (argomenti e valori procedurali)  ---> no corpo funzione, ma semantica...
; (p + 1 2 12)                  —> (1 2 3 5 8 13 21 34 55 89 144 233)
; (p string-append "q" "b" 6)   —> ("q" "b" "qb" "bqb" "qbbqb" "bqbqbbqb")
; (p list null null 5)          —> ( () ()  (() ())  (() (()))  ((() ()) (() (() ()))) )

; a)  (p X X X 7) --> ("|" "|" "||" "|||" "|||||" "|||||||" "|||||||||" "|||||||||||" "|||||||||||||")  // numeri dispari 1,3,5...
;     (p string-append "|" "|" 7)

; b)  (p (lambda (x y) ) "abc" "bcd" 9) --> ("abc" "bcd" "cda" "dab" ...x9) --> GIUSTAPPOSIZIONE
;     (p (lambda (u v) (string-append (substring v 1) (substring u 0 1))) "abc" "bcd" 9)
;                                    (esclude il 1*c) (prendo il 1*c)

; c)  (p X X X X) --> ((1 2 3 4) (5 6 7) (4 3 2 1) (7 6 5) (1 2 3 4) (5 6 7))
;     (p (lambda (s t) (reverse s)) '(1 2 3 4) '(5 6 7) 6)  --> Reverse può avere 1 sola lista / el.

; d)  (p () 103 24 10) -->  (24 103 24 7 3 1 0 0 0 0)    --> Attenzione alla divisione per 0.
;     (p
;       (lambda (x y)
;         (if (or (= x 0) (= y 0))
;           0 
;           (remainder x y)
;          )
;        )
;      24 103 10)
;





; Esercitazione 2/B (26/01/24)

; esercizio 1 (2). Ricorsione ad albero
; (123-tess 2) --> 1
; (123-tess 3) --> 3
; (123-tess 5) --> 4


(define 123-tess    ; val: intero
  (lambda (n)       ;   n: n>0
    (+ (trec (- n 1) 1)
       (if (< n 2)
           0
           (trec (- n 2) 2)
           )
       (if (< n 3)
           0
           (trec (- n 3) 3)
           )
       )
    ))


(define trec
  (lambda (n k)    ; n >= 0, k app.a [1,2,3] , k: lunghezza della base della piastrella.
    (if (= n 0)
        1
        (+
         (if (= k 1)
             0
             (trec (- n 1) 1)
             )
         (if (or (= k 2) (< n 2))
             0
             (trec (- n 2) 2)
             )
         (if (or (= k 3) (< n 3))
             0
             (trec (- n 3) 3)
             )
         )
       )
    ))




; 3) problema Manhattan (variante) (2)

; Dimostrare che Vn app.a N+ (mrec 3 n 1) -> 2n  (*)

; Caso base: n=1
;   (mrec 3 1 1) -> 2

; Ipotesi induttiva:
;   (mrec 3 k 1) -> 2k

; Passo Induttivo:
;   (mrec 3 k+1 1) -> 2(k+1)

; Dimostrazione del passo induttivo:  [i = 3, k+1=j, 1 = k]
;   (mrec 3 k+1 1)
;   --> (lambda 3 k+1 1) ... (if (= j 0) ... (+ (mrec (- 3 2) k+1 -1) (mrec 3 (k+1 -1) 1)) ))
;   --> (+ (mrec 1 k+1 -1) (mrec 3 k 1))
;   --> (+  2   (mrec 3 k 1))    // 2 per la dimostrazione che è già scritta (mrec 1 n -1) per n>=2 --> 2 (**)
;   --> (+  2    2k)            // per l'ipotesi induttiva
;   --> 2k+2 = 2(k+1)      [ok]
;  CVD





; es: dato n>0 con f:D->D e k app.a D, iter-seq restituisce una lista di n elementi di D, ciascuno dei quali
;     risulta da una successiva iterazione dell'applicazione di f a partire da k - dove l'ultimo termine si
;     intende che f è applicata n-1 volte:
;
; (iter-seq n f k) ---> (k (f(k) f(f(k)) f(f(f(k))) ... (f(f ...f(k)...)) )
;
; Partendo da k ogni ulteriore el. si ottiene applicando f a el-1
;
; (iter-seq 4 sqr 2) --> (2 4 16 256)
; (iter-seq 4 cdr '(5 4 3 2 1)) --> ((5 4 3 2 1) (4 3 2 1) (3 2 1) (2 1))


(define iter-seq    ; val: lista di n el.
  (lambda (n f k)   ; n: intero, f: procedura, k: argomento procedura
    (cond ((= n 1)
           (list k)
           )
          (else
           (cons k
                 (iter-seq (- n 1) f (f k))
                 )
           )
          )
    ))



; Argomenti a valori procedurali: 

; (iter-seq 7 XXX XXX)                                 --> (1 3 9 27 81 243 729)
; (iter-seq 7 (lambda (x) (* 3 x)) 1)                  --> (list 1 3 9 27 81 243 729)  

; (iter-seq XXX XXX XXX)                               --> ((1 2 3 4) (2 3 4) (3 4) (4) ())
; (iter-seq 5 cdr '(1 2 3 4))                               --> (list (list 1 2 3 4) (list 2 3 4) (list 3 4) (list 4) '())

; (iter-seq 5 XXX XXX)                                      --> ("" "AB" "ABAB" "ABABAB" "ABABABABAB")
; (iter-seq 5 (lambda (x) (string-append "AB" x)) "")       --> (list "" "AB" "ABAB" "ABABAB" "ABABABAB")

; (iter-seq 4 XXX "E")                                      --> ("E" "(EE)" "((EE)(EE)" "(((EE)(EE))((EE)(EE)))" )
; (iter-seq 4 (lambda (x) (string-append "(" x x ")")) "E") --> (list "E" "(EE)" "((EE)(EE))" "(((EE)(EE))((EE)(EE)))")


; NB: < string-append > può avere più argomenti.
; NB: nella concatenazione di liste i comandi più ricorrenti sono: < CONS > , < APPEND >. 




; Esercizio 20 sett. 2024: f,g: D->D con x app.a D e int k>0
; iter-alt restituisce lista di k el. di D, con successive applicaz. di f,g in
; maniera alternata, a partire da x, e le funzioni vengono applicate k-1 volte.

; (iter-alt f g x k)                                        --> (x f(x) g(f(x)) f(g(f(x))) g(f(g(f(x)))) ...)
; (iter-alt cdr reverse '(1 2 3 4) 5)                     --> ((1 2 3 4) (2 3 4) (4 3 2) (3 2) (2 3))
; (iter-alt (lambda (x) (+ x 1)) (lambda (x) (/ 1 x)) 1 10) --> 

(define iter-alt
  (lambda (f g x k)
    (if (= k 1)
        (list x)
        (cons x (iter-alt g f (f x) (- k 1)))    ; inverto f e g, calcolo f(x) e scalo 1 da k.
      )
  ))





; Argomenti procedurali in Scheme: lista ordinata s (int+) diversi tra loro, combinazioni di s senza ripetizioni,
; per cui la somma degli elementi di s inclusi nella combinazione è esattamente uguale alla somma degli elementi
; di s esclusi.

; (comb '(1 3 5 7))         --> ((1 7) (3 5))    // perchè 3+5=7+1 = 8
; (comb '(1 2 3 4 5))       --> ()
; (comb '(1 2 3 5 7 11 13)) --> ((1 2 5 13) (1 2 7 11) (1 7 13) (2 3 5 11) (3 5 13) (3 7 11))

(define comb
  (lambda (s)
    (comb-rec s 0 0)
  ))

(define comb-rec
  (lambda (s x y)
    (if (null? s)
        (if (= x y) (list null) null)   ; coda: dove termina il programma.
        (let ((e (car s)))
          (append
           ; mappa: add new el., (ultimo el, x+(primo el s), y)
           (map
            (lambda (x) (cons e x))
            (comb-rec (cdr s) (+ x e) y)
            )
           ; append di somma con la seconda parte di lista.
           (comb-rec (cdr s) x (+ y e)) 
           ))
        )
    ))




; Esercizio prova 21/01/22
; Procedura: data stringa(s), e int(k)>0, cyclic-pattern restituisce:
; a) la stringa p(pattern) di lunghezza k, se s è costituita dalla ripetizione ciclica di p 1+ volte
; b) la stringa vuota altrimenti.
; Nel caso a) le ripetizioni di p si intendono consecutive (senza caratteri estrenei) e complete ovvero non troncate.
; 
; (cyclic-pattern "" 3)           --> ""
; (cyclic-pattern "abc" 3)        --> "abc"
; (cyclic-pattern "abcabcabc" 3)  --> "abc"
; (cyclic-pattern "abcabcab" 3)   --> ""
; (cyclic-pattern "abcabcacb" 3)  --> ""
; ((cyclic-pattern "abcabcabc" 2) --> ""

    
(define cyclic-pattern   ; val: stringa
  (lambda (s k)          ; s: stringa, k: intero+
    (cond ((= k 0) "")
          ((= (string-length s) k)  s) 
          ((> (remainder (string-length s) k) 0) "")
          ((string=? (substring s 0 k) (substring s k (* 2 k)))   ; < s 0 3, s 3 6 >
           (cyclic-pattern (substring s k) k)
           )
          (else "")
          )
    ))


; NB: (substring "abc" 2) --> "c"





; Es  3. Argomenti Procedurali
; percorsi di Manhattan diversi, costituiti da:
; - i: spostamenti verticali
; - j: spostamenti orizzontali
; NB: non si possono fare più di k spostamenti orizzontali consecutivi.

(define manh
  (lambda (i j k)
    (manh-rec i j k k)
    ))

(define manh-rec
  (lambda (i j k v)
    (cond ((= i 0)
           (if (> j v) 0 1))
          ((= j 0)
           1)
          ((= v 0)
           (manh-rec (- i 1) j k k))
          (else
           (+
            (manh-rec (- i 1) j k k)
            (manh-rec i (- j 1) k (- v 1))
            ))
       )
  ))


; (paths-o 5 1 2) --> ("000001" "000010" "000100" "001000" "010000" "100000)
; (paths-o 1 5 2) --> ()
; (paths-o 2 2 1) --> ("0101" "1001" "1010")

; Il programma dovrà restituire una lista di percorsi "0/1..."
; 0 (i) = spostamento verticale in basso   -  GIU'
; 1 (j) = spostamento orizzontale a destra -  DX

; make-string --> restituisce string con n ripetizioni di c.
; (make-string n c) : n = intero, c = carattere.

(define paths-o
  (lambda (i j k)
    (paths-rec i j k k)
 ))

(define paths-rec
  (lambda (i j k v)
    ;      5 1 2 2
    (cond ((= i 0)  ; solo spostamenti orizzontali
           (if (> j v)
               null
               (list (make-string j #\1))
               )
           )
          ((= j 0)  ; solo spostamenti verticali
           (list (make-string i #\0))
           )
          ((= v 0)  ; spostamenti verso il basso
           (map (lambda (x) (string-append x "0"))
                (paths-rec (- i 1) j k k))  ; (i-1+j) --> cerco i+j
           )
          (else     ; prima ricorsione con (5 1 2 2)                                  
           (append
            (map (lambda (x) (string-append x "0")) ; GIU'
                 (paths-rec (- i 1) j k k))
            (map (lambda (x) (string-append x "1")) ; DX
                 (paths-rec i (- j 1) k (- v 1))))
           )
       )
  ))
          


; Prova del 19-01-23
; Ricorsione ad albero: procedura scs: calcola soprasequenza comune più corta di 2 seq. di caratteri u,v (stringhe)
; è una variante dell' LCS, siamo però interessati a una sequenza di s più corta possibile, t.c u,v siano sottoseq.
; ovvero che si possono ottenere da s indipendentemente cancellando alcuni caratteri di s (il risultato in generale
; *non* è univoco, per cui scs restituisce una delle possibile soprasequenze più corte.

; (scs "arto" "atrio")   --> "atrito"
; (scs "arco" "ocra")    --> "ocrarco"
; (scs "archetipo" "")   --> "archetipo"
; (scs "copia" "copia")  --> "copia"


(define scs         ; val: stringa
  (lambda (u v)     ; u,v: stringhe
    (cond ((string=? u "")
            v
           )
          ((string=? v "")
            u
           )
          ((char=? (string-ref u 0) (string-ref v 0))
           (string-append (substring u 0 1)
                          (scs (substring u 1) (substring v 1))
                          )
           )
          ; "arco" "ocra"
          (else
           (let ((x (scs (substring u 1) v))  ; --> x = (scs "a" "ocra") = ...(*)
                 (y (scs u (substring v 1)))  ; --> y = (scs "arco" "o") = ...(*)
                 )
             (if (< (string-length x) (string-length y))
                 (string-append (substring u 0 1) x)
                 (string-append (substring v 0 1) y)
                 )
             )
           )
          )
    ))

; (*)  : x = (scs "rco" "ocra")
;        -*-> (scs "co" "ocra")
;        -*-> (scs "o" "ocra")
;        = ["o" = "o"]
;        ==> "o" + (scs "" "cra")
;        ==> "o" + "cra"
;        ==> "ocra"

; (**) : y = (scs "arco" "cra")
;        -*-> (scs "arco" "ra")          ; 'a' ≠ 'c' → ramo y
;        -*-> (scs "rco" "ra")           ; 'a' ≠ 'r' → ramo x
;        -*-> (scs "co" "a")             ; 'r' = 'r'
;        -*-> (scs "o" "a")              ; 'c' ≠ 'a'
;        -*-> (scs "" "a") = "a"          ; caso base
;        -*-> (scs "o" "") = "o"          ; caso base
;        ==> min("a","o") = "o"
;        ==> "c" + "o" = "co"
;        ==> "r" + "co" = "rco"
;        ==> confronto: "rco" (3) < "arco" (4)
;        ==> "a" + "rco" = "arco"
;        ==> "c" + "arco" = "carco"

; scs("arco","ocra")
; │
; ├─ x = scs("rco","ocra")  → "ocrarco"
; ├─ y = scs("arco","cra")  → "carco"
; │
; └─ confronto
;    → |x| = 7, |y| = 5
;    → scelgo y
;    → aggiungo "o" (primo carattere di v)
;    → risultato finale: "ocrarco"





; Verifica formale della correttezza
; (fh t) --> 3*2^(k-2) + 1  (*)
; (fh "111") --> 7
; procedura:
(define fh      ; val: intero
  (lambda (b)   ; b: stringa di 0/1 conclusa da 1
    (cond ((string=? b "1")
           1)
          ((char=? (string-ref b 0) #\0)
           (- (* 2 (fh (substring b 1))) 1))
          (else
           (+ (* 2 (fh (substring b 1))) 1))
          )
    )
  )

; Dimostrazione per induzione della correttezza:
; funzione da dim: (fh t) --> 3*2^(k-2) + 1  (*)  al variare di K.

; Caso base (proprietà): 
; (fh t) con k >= 3:
; (fh "111") --> 3*2^(3-2) + 1 = 3*2 + 1 = 7  [ok]

; Ipotesi induttiva:
; Considero un k >= 3 intero, e assumo:
; (fh "00...111") --> 3*2^(k-2) + 1

; Proprietà da dimostrare come passo induttivo:
; Considerato k = k+1
; (fh "000...111") --> 3*2^(k-1) + 1

; Dimostro il caso base e passo induttivo:
;  Caso base: con lunghezza stringa k>=3 e k-3 cifre 0 --> se k=3 => cifre(0)=0 quindi tutti 1.
;  (fh "111")
;  --> (else (+ (* 2 (f (substring "111" 1))) 1)))
;  --> (+ (* 2 (f "11")) 1)
;  --> (+ (* 2 (+ (* 2 (f "1")) 1)) 1)
;  --> (+ (* 2 (+ (* 2 1) 1)) 1)
;  --> (+ (* 2 3) 1)
;      = 7   [ok]

;  Passo induttivo: considerato k = k+1
;  (fh "000...111") --> 3*2^(k-1) + 1               // k=solo1, k+1=almeno1zero
;  (cond ... ((char=? ...)))
;   --> (- (* 2 (f (substring "000...111" 1))) 1)   // k+1
;   --> (- (* 2 (f "00...111")) 1)                  // k ==> posso applicare l'ipotesi induttiva
;   --> (- (* 2 (3*2^(k-2)+1)) 1)                   // applicato l'ipotesi ind. (verificato le condizioni)
;   --> (- (3*2^k-1)+2) 1)
;   --> (3*2^k-1 + 1)
;   -->  = 3*2^k+1-2 + 1  [CVD per k = k+1]






; Procedura diffs: data una lista non vuota di interi, restituisce la lista delle differenze fra
; coppie di elementi conscutivi, nell’ordine.
; (Se l’argomento è una lista di 1 solo elemento il risultato è la lista vuota.)

; Input esempio procedura: (diffs '(-5 3 7 4 4 5)) → (8 4 -3 0 1)

(define diffs     ; val: lista non vuota
  (lambda (seq)   ; seq: lista non vuota di interi(R)
    (if (null? (cdr seq))
        null
        (cons (- (cadr seq) (car seq)) (diffs (cdr seq)))
        )))

; Passaggi ricorsione
;  --> (cons (- 3 - 5) (diffs '(3 7 4 4 5)))
;  --> (cons  4        (diffs '(7 4 4 5)))
;  --> (cons -3        (diffs '(4 4 5)))
;  --> (cons 0         (diffs '(4 5)))
;  --> (cons 1         (diffs '(5)))
;  --> '(5) => caso base => null
;
;  risultato: (cons 8 (4 -3 0 1)) --> (8 4 -3 0 1)




; data una funzione f definita per tutti gli argomenti interi e dato un intero s,
; restituisce la funzione g tale che g(x) = f(x–s). Per esempio, sulla base della
; definizione  (define h (shift (lambda(x) (* x x)) 3))  , devono risultare le seguenti valutazioni:
;  (h 0) → 9 (h 3) → 0 (h 4) → 1 (h 7) → 16

(define shf
  (lambda (f s)
    (lambda (x)
      (f (- x s))
     )
  ))

(define h (shf (lambda(x) (* x x)) 3))





; Es. pr. scr. 22/09/2023
; es.1
; Date liste u,v la proc. restituisce la lista con u,v alternati, a partire
; dal primo el. di u, seguito dal primo el. di v.
;
; (alternate-merge '(1 2 3 4 5 6 7) '("a" "b" "c" "d")) --> (1 "a" 2 "b" 3 "c" 4 "d" 5 6 7)
; (alternate-merge '(1 2 3 4) '(#\a #\b #\c #\d #\e))   --> (1 #\a 2 #\b 3 #\c 4 #\d #\e)

(define alternate-merge
  (lambda (u v)
    (cond ((null? u)
           v
           )
          ((null? v)
           u
           )
          (else
           (cons (car u)
                 (cons (car v)
                       (alternate-merge (cdr u) (cdr v)))
                 )
           )
          )
    ))





; es 3 - verifica formale della correttezza
;
(define fu
  (lambda (x y)
    (if (= x y)
        (+ x y -1)
        (let ((z (quotient (+ x y) 2)))
          (+ (fu x z) (fu (+ z 1) y))
          ))
    ))

; Dimostrare per ind:
;  Ogni k in N. Ogni m,n in N+ t.c. n-m <= k.
;  (f m n) --> n^2 - (m-1)^2

; Caso base: per [0 < m <= n], e m=n:
;  V m,n in N+ t.c. n-m <= 0    (f m n) --> n^2 - (m-1)^2  
;  V n in N+ . (f n n) --> n^2 - (n-1)^2 = 2n-1             //perchè n=m

; Ipotesi induttiva*: considero un k in N e assumo:
;  V m,n in N+ t.c. n-m <= k    (f m n) --> n^2 - (m-1)^2 

; Passo induttivo:  per k considerato
;  V m,n in N+ t.c. n-m <= k+1  (f m n) --> n^2 - (m-1)^2 

; Dimostrazione passo induttivo:   ***(non richiesto)***
;  (f m n)
;   --> ?     // spezzare la dim. in 2 parti
; 
;  a) m=n
;   --> (+ n n -1) --> 2n-1 = n^2 - (n-1)^2
;  b) m<n
;   --> (let ((z (quotient (+ m n) 2))) (+ (fu m n) (fu (+ z 1) n)) )
;   --> (let ((z (m+n)/2) (+ (fu m z) (fu (+ z 1) n)) )
;   --> (+ (fu m [(m+n)/2]) (fu (+ [(m+n)/2] 1) n) )   
;
;   Se: [(m+n)/2] - m <= k   ...applico l'ipotesi induttiva*
;   --> (+ [(m+n)/2]^2 - (m-1)^2 (f (+ [(m+n)/2] 1) n))
;   --> (+ [(m+n)/2]^2 - (m-1)^2 (f [(m+n)/2]+1 n))
;
;   Se: n - [(m+n)/2] - 1 <= k
;   --> (+ [(m+n)/2]^2-(m-1)^2  n^2-([(m+n)/2]+1-1)^2)
;   -->    [(m+n)/2]^2-(m-1)^2 + n^2-([(m+n)/2]+1-1)^2
;     = -(m-1)^2 + n^2 
;                               vale per k >= 1                              vale per k >= 1
;   m+n pari: [(m+n)/2] - m = [(n-m)/2 <= (k+1)/2 <= k]  /  n-[(m+n)/2]-1 = [(n-m)/2 - 1 <= (k+1)/2 - 1 <=k]
;     
;   m+n dispari: [(m+n)/2] - m = [(n-m-1)/2 <= k/2 <= k]  /  n-[(m+n-1)/2]-1 = [(n-m+1)/2 - 1 <= (k+2)/2 - 1 <=k] = [k/2 <= k]


; Es: Completo la procedura sq, per calcolare il quadrato di un intero positivo
"""
(define sq
  (lambda (n)
    (f XXX XXX)
    ))
"""

; (sq n) --> (f n y) --> x^2 - (y-1)^2 --> n^2
; (sq n) --> (f n 1) --> n^2 - 0 --> n^2          // va bene per n app.aN+ e y!=0
; [quindi per n app.a N+ e y!= 0]





