;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_7_laboratorio_liste) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define belong?
  (lambda (x s)
     (cond
       ((null? s) false)                    ; caso 1: lista vuota
       ((= x (car s)) true)                 ; caso 2: x è il primo elemento
       ((> x (car s)) (belong? x (cdr s)))  ; caso 3: x è un altro elemento di S
       (else false)                         ; caso 4: x è più piccolo del primo elemento (< x (car S))
      )
   ))

(define position
  (lambda (x s)
    (cond
      ((null? s) false)    ; caso 1: lista vuota
      ((= x (car s)) 0)    ; caso 2: x è il primo elemento
      
     )
  ))