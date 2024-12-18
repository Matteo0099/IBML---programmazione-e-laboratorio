;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_38_esempi_prova_esame_24) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss.txt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss.txt" "installed-teachpacks")) #f)))
;; Prova di accertamento del 26/01/2024

;; 1 esercizio ;;
;; input esempio: (iter-seq sqrt 256 4) --> (256 16 4 2)
;; output: x, f(x), f(f(x)), .... (f(f(x)-1)).
;;> (iter-seq sqrt 256 4) --> (list 256 16 4 2)

(define iter-seq     ; val: lista di D
  (lambda (f x k)    ; f:D-->D, x:D, k:intero positivo.
    (if (= k 1)
        (list x)
        (cons x (map f (iter-seq f x (- k 1))))   ; lista: 1,2,...,k-1 volte.
     )
    ))

;; (da prendere spunto)
(define iter      ; val: D-->D procedura
  (lambda (f i)   ; f: D-->D procedura, i: intero positivo
    (lambda (x)
      (if (= i 0)
          x
          (f ((iter f (- i 1)) x))
       ))
   ))


;; 2 esercizio ;;
;; ricorsione ad albero - procedura 123-tess (simile a tassellaz.)
;; 3 tipi: lato 1, 2x1, 3x1.
;; quanti modi diversi di disporre su cordolo length>0 heigth=1 ?
;; > (123-tess 2) -> 1   (123-tess 3) -> 3   (123-tess 5) --> 4   (123-tess 8) --> 12

(define 123-tess   ; val: intero
  (lambda (n)      ; n > 0
    (+ (trec (- n 1) 1)
       (if (< n 2) 0 (trec (- n 2) 2))
       (if (< n 3) 0 (trec (- n 3) 3))
       )
    ))

(define trec
  (lambda (n k)   ; n>=0, k € [1,2,3], [k: lungh. della base].
    (if (= n 0)
      1
      (+ (if (= k 1) 0 (trec (- n 1) 1))
         (if (or (= k 2) (< n 2)) 0 (trec (- n 2) 2))
         (if (or (= k 3) (< n 3)) 0 (trec (- n 3) 3))
       ))
    ))



;; 3 esercizio
; caso base (per n = 1):  (mrec 3 1 +1) -> 2
; ipotesi induttiva:      (mrec 3 k +1) -> 2k
; passo induttivo:        (mrec 3 k+1 +1) -> 2(k+1)
; (mrec 3 k+1 +1)

;  --> (+ (if (< 3 2) 0 (mrec (- 3 2) k+1 -1)) (mrec 3 (- k+1 1) +1))

;  --> (mrec (- 3 2) k+1 -1)) (mrec 3 (- k+1 1) +1))

;  --> (+ 2 (mrec 3 (- k+1 1) +1))  [per (**)]

;  --> (+ 2 (mrec 3 k +1))

;  --> (+ 2 2k)  [per l'ipotesi induttiva]

;  -->  = 2+2k = 2(k+1)  ==> OK.

; ogni k positivo [(mrec 3 k +1) -> 2k   ==>  (mrec 3 k+1 +1) -> 2(k+1)]

;;;;;;;;;;;;;;;;




;; 4 esercizio
; a)
(iter-seq (lambda (x) (* 2 x)) 1 10)

; b)
(iter-seq cdr '(4 3 2 1) 5)

; c)
(iter-seq (lambda (x) (string-append x "10"))  "" 5)

; d)
(iter-seq (lambda (x) (string-append x "|" x)) "-" 4)
