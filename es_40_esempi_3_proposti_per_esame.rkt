;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_40_esempi_3_proposti_per_esame) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss.txt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss.txt" "installed-teachpacks")) #f)))
; 09/01/2024
; es1
; rivalutazione dell'esercizio con matrix-scalar-product
(define scalar-product   ; val: numero
  (lambda (u v)          ; u,v: liste numeriche (numeri reali)
    (if (null? u)
        0
        (+ (* (car u) (car v)) (scalar-product (cdr u) (cdr v)))
        )
    ))

(define matrix-vector-product
  (lambda (A b)
    (if (null? A)
        null
        (cons (scalar-product (car A) b) (matrix-vector-product (cdr A) b))
     )
  ))

; input: (scalar-product '(1 2.3 4) '(1 4 2.3))
; output: 19.4  (= ovvero: 1*1 + 2.3*4 + 4*2.3 )



; es2
; Ogni t stringa (non vuota) t.c. !t| = 2k & T composto da "10" ripetuto . (f t) -*-> (4^k - 1) / 3 (*)
; Dimostrazione per induzione su k

; Caso base (k = 1)
; (f "10") -*-> (4^k - 1)/3 = 1

; Dimostrazione caso base:
; (f "10") --> (- (* 2 (f (substring "10" 0 1))) 1) --> (- (* 2 (f "1")) 1) --> (- (* 2 1) 1) --> 1

; Ipotesi induttiva: scelta k > 0 intero, per t' = "1010 ... 10" composto da k ripetizioni di "10" si assume che:
; (f t') -*-> (4^k - 1)/3

; Passo induttivo: per k scelto sopra, t" = t' + "10" di lunghezza 2(k+1)
; (f t") -*-> (4^(k+1) - 1)/3

; Dimostrazione passo induttivo:  (calcoli) 
; (f t") -*-> (- (* 2 (f (substring b 0 2(k+1)-1))) 1) -*-> (- (* 2 (f t'+"1")) 1)
;        -*-> (- (* 2 (+ (* 2 (f (substring t'+"1" 0 2(k+1)-2))) 1)) 1)
;        -*-> (- (* 2 (+ (* 2 (f t')) 1)) 1)   ; si applica l'ipotesi induttiva(sostituisco f t')
;        -*-> (- (* 2 (+ (* 2 (4^k - 1)/3) 1)) 1)
;        -*-> (- (* 2 (+ 2(4^k - 1)/3 + 1) 1)
;        -*-> (- 4(4^k - 1)/3 +2 1)
;        -*-> 4(4^k - 1)/3 +1 = (4^(k+1) - 1)/3  => Dimostrato.




; es 3 prova del 05/09/2017
; 1, 1
; null
; (string-ref u 0), (string-ref v 0).
; (list i j (substring u 0 1))
; (lcs-rec (+ i 1) (substring u 1) j v)))
; (lcs-rec i u (+ j 1) (substring v 1))))
; 

