;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_27_liste_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; list-ref
(define lista-rif   ; val: tipo dell'elemento
  (lambda (s i)     ; s: lista, i: indice
    (if (= i 0)
        (car s)  ; casi base
        (lista-rif (cdr s) (- i 1))  ; casi ricorsivi
        )
    ))


; length
(define lunghezza
  (lambda (s)       ; s è la lista
    (if (null? s)   ; caso base: lista vuota
        0           ; la lunghezza di una lista vuota è 0
        (+ 1 (lunghezza (cdr s))) ; caso ricorsivo
     )
  ))


;append
(define giustapponi ; val: lista
  (lambda (u v)     ; u, v: liste
    (if (null? u)
        v
        (cons (car u) (giustapponi (cdr u) v))
     )
   ))


; reverse
(define rovescia  ; risultato che spero (val): lista
  (lambda (s)     ; s: è una lista s=(1 2 3 4)
    (if (null? s)
        null
        (giustapponi (rovescia (cdr s)) (list (car s)))  ;cdr (4 3 2) car (1)
     )
    ))

; reverse-2
(define rovescia-2  ; val: lista
  (lambda (s)       ; s:   lista
    (rovescia-ric s null)
    ))

(define rovescia-ric
  (lambda (s r)
    (if (null? s)
        r                ; lista rovesciata finale
        (rovescia-ric (cdr s) (cons (car s) r))  ; altrimenti
     )
   ))

; come si svolge il reverse ricorsivo
; (1 2 3 4) ()
; (2 3 4)   (1)
; (3 4)     (2 1)
; (4)       (3 2 1)
; ()        (4 3 2 1)