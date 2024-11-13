;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_28_test_primalità) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Test di primalità (numeri)

(define primo? ; val: booleano
  (lambda (n)  ; n >= 2 intero
    (if (even? n)
        (= n 2)
        (not (divisori-dispari-in? n 3 (- n 1)))
       )
    ; se non ha divisori, allora è primo.
    ; se il secondo estremo sx < estremo dx allora Int.=vuoto
  ))

; n ha divisori nell'intervallo [a, b]?

(define divisori-dispari-in?  ; val: booleano
  (lambda (n a b)     ; n, a, b: interi positivi
    (cond ((> a b)    ; estr.sx < estr.dx
           false)
          ((= (remainder n a) 0) ; resto 0, ha divisori.
           true)
          (else
           (divisori-dispari-in? n (+ a 2) b)   ; int. più piccolo
           )
      )
   ))

; n = pq con p,q > 1
; p > sqrt(n) ?--> q < sqrt(n)

; mi dà l'elenco dei n. primi in un intervallo [n1<>n2]
(define lista-primi
  (lambda (a b)     ; a,b: interi >= 2
    (cond ((> a b)
           null)
          ((primo? a)
           (cons a (lista-primi (+ a 1) b)))
          (else
           (lista-primi (+ a 1) b)
           )
      )
   ))