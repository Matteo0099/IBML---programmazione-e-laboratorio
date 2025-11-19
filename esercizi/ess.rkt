;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define product
  (lambda (x n)
    (if (or (= x 0) (= n 0))
        0
        (+ x (product x (- n 1)))
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