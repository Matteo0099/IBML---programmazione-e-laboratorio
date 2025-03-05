;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname RISOLUZIONE_ESAME_21_GENNAIO_2025) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ESERCIZIO 1: simile a quello del 2024, consiste in una funzione con 4 parametri, e ogni parametro corrisponde 
; (p + 1 2 12) --> (1 2 3 5 8 13 21 34 55 89 144 233)
; In questa funzione esempio, si somma il numero precedente a quello successivo, a partire da 1 per 12 volte.

(define p
  (lambda (f x y k)
     (if (= k 0)
         null
         (cons x (p f y (f x y) (- k 1)))
         )
    ))


; richiesta da parte di Mirolo: risposte più accurate!
; appunto 1: SONO DIVERSE!!!  --> è un imprecisione
; (list )    '()     null
; (list x)   '(x)    1 valore
; (list x y) '(x y)  2 valori

; appunto 2: scritture più facili  --> non è errore
; (append (list x) u) ==> DIFFICILE
; (cons x u)  ==> FACILE
; NB: (cons x u) va bene, (cons u x) NO. --> (append u (list x)).

; appunto 3: cons != append != list
; cons: aggiunge un elemento all'inizio, ha 2 argomenti.
; append: se ha 2 argomenti, giustapposizione di due liste con elementi dello stesso tipo.
; list: se ha 2 argomenti, costruisce una coppia con gli argomenti non necessariamente dello stesso tipo.

; appunto 4: x != (list x) 
; x è diverso da (list x)!!!

; appunto 5: usare la sintassi di student with lambda 2:
; (define function (lambda (x) ...))

; loop in scheme da un punto di vista funzionale:
; let --> non ammette la ricorsione
; letrec --> ammette la ricorsione
(define q
  (lambda (f x y k)
    (letrec
        ((helper
          (lambda (i s)
            (if (= i k)
                s
                (append s (list (f (list-ref s (- i 2)) (list-ref s (- i 1)))))
                )
            )
          ))
      (cond ((= k 0)
             null)
            ((= k 1)
             (list x))
            (else
             (helper 2 (list x y))
             )
            )
      )
    ))
; "i" è l'indice dell' i-esimo elemento che devo aggiungere, s è la lista già costruita.


; ESERCIZIO 2: stringa più lunga, però prendo gli indici delle posizioni delle stringhe.
; la struttura è simile a quella vista in classe sull'algoritmo lcs.
; -esempio: (lcps "atrio" "arto") --> (list (list 1 2 5) (list 1 3 4))
(define lcps
  (lambda (u v)
    (rec-proc 1 u 1 v)
    ))

(define rec-proc
  (lambda (i u j v)
    (cond ((or (string=? u "") (string=? v ""))
           (list null null)   ;nessuna posizione, se non è presente la stringa
           )
          ((char=? (string-ref u 0) (string-ref v 0))
           (let ((x (rec-proc 
                     (+ i 1)                 ;aumento di posizione la 1*
                     (substring u 1)      
                     (+ j 1)                 ;aumento di posizione la 2*
                     (substring v 1))) 
                 )
             (list (cons i (car x)) (cons j (cadr x)))   ;posizioni caratteri (0 e 1) che sono uguali al carattere della stringa
             ))
          (else
           (let ((x (rec-proc (+ i 1) (substring u 1) j v))
                 (y (rec-proc i u (+ j 1) (substring v 1)))
                 )
             (if (< (length (car y)) (length (car x)))   ;prendo le SOLUZIONI PIU' LUNGHE!
                 x
                 y)
             ))
          )))



; ESERCIZIO 3: verifica formale della correttezza   (-> passo di valutazione, = è uguale).
; dimostrazione per induzione di: (tail-rec x y z) --> x(x+y-1)+z (*)  Vx,y,z€N.
; 1) caso base: Vx,y,z€N (tail-rec 0 y z) --> 0(0+y-1)+z = z
; 2) passo induttivo: Vx,y,z€N (tail-rec 0 y z) --> (x+1)(x+1+y-1)+z = (x+1)(x+y)+z
; 3) DIM. passo ind.: se if x=1 allora... segue che (tail-rec x y+2 y+z) -> x(x+y+2-1)+y+z (per l'ipotesi induttiva) -> (x+1)(x+y)+z

; (pt.2) dimostra che Vn€N (f n) --> n^2
; Vn€N (f n) --> (tail-rec n 1 0) --> n(n+1-1)+0 = n^2



; ESERCIZIO 4
; a)
; b)
; c)
; d)


