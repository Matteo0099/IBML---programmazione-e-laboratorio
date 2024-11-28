;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_34_probl_conigli_coppie) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Problema di Fibonacci

(define coppie-fertili  ; val: intero
  (lambda (t)           ; t: intero positivo
    (if (= t 0)
        1
        (+ (coppie-fertili (- t 1)) (coppie-cuccioli (- t 1)))
     )
  ))

(define coppie-cuccioli  ; val: intero
  (lambda (t)           ; t: intero positivo
    (if (= t 0)
        0
        (coppie-fertili (- t 1))
     )
  ))

;; Fib(i) = fertili(i) + cuccioli(i)
;; Fib(i) = Fib(i-2) + Fib(i-1)  [per i>1] due mesi prima + un mese dopo.
;; Fib(0) = 1, Fib(1) = 2.