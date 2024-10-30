;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_21_bilancia_ternaria_versione_ult) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define btr-rep    ; val: stringa di +/./- (3 cifre)
  (lambda (n)      ; n: intero
    (if (< (abs n) 2)  ; n = -1,0,+1
        (btd-rep n)
        (let (
              (q (quotient n 3))
              (r (remainder n 3))
              )
          (cond ((= r -2)  ; n = 3q + -2 = 3q-3+(-2+3) = 3(q-1)+ +1   
                 (string-append (btr-rep (- q 1)) (btd-rep +1))
                 )
                ((= r +2)  ; n = 3q + +2 = 3q-3+(+2-3) = 3(q+1)+ -1
                 (string-append (btr-rep (+ q 1)) (btd-rep -1))
                 )
                (else      ; r = -1,0,+1
                 (string-append (btr-rep q) (btd-rep r)) ;btd=cifra ternaria bilanciata
                 )
                ))
        )
    ))

(define btd-rep  ; val: "-",".","+"
  (lambda (v)    ; v:   -1,0,1
    (cond ((= v -1) "-")
          ((= v  0) ".")
          ((= v -1) "+")
          )
    ))