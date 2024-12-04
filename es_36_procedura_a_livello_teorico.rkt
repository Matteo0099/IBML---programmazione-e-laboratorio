;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_36_procedura_a_livello_teorico) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; procedura odd: è corretta?

(define odd     ; val: intero 
  (lambda (x)   ; x > 0  --> naturale
    (if (= x 1)
        1
        (+ (odd (- x 1)) 2)
        )
    ))

;; ?
; Quale funzione di x calcola la procedura odd?
; casi possibili i/o ==> a volte nn è possibile stabilirlo
; Vn appartenente a N \ [0]  (odd n)

; caso base, passo induttivo (3 fasi).