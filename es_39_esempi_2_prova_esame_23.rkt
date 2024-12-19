;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_39_esempi_2_prova_esame_23) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss.txt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss.txt" "installed-teachpacks")) #f)))
;; I Prova di accertamento
;; es. 1  -  prodotto scalare fra due vettori
;; input (matrix-vector-product '((0.8 0.5 0.2) (0.1 1.0 0.4) (0.5 0.5 0.4)) '(4.0 6.2 5.5))

(define matrix-vector-product
  (lambda (a b)
    (if (null? a)
        0
        (cons (scalar-product (car a) b) (scalar-product (cdr a) b))
        )
    ))


(define scalar-product
  (lambda (u v)
    (if (null? u)
        0
        (+ (* (car u) (car v)) (scalar-product (cdr u) (cdr v)))
        )
    ))


;; es. 2 - sottosequenza più lunga
;; input: (scs "arto" "atrio"), (scs "arco" "ocra")...
(define scs
  (lambda (u v)
    (cond ((string=? u "") 
           v)
          ((string=? v "")
           u)
          ((char=? (string-ref u 0) (string-ref v 0))
           (string-append (substring u 0 1) (scs (substring u 1) (substring v 1)))
           )
          (else
           (let ((x (scs (substring u 1) v))
                 (y (scs u (substring v 1)))
                 )
             (if (< (string-length x) (string-length y))
                 (string-append (substring u 0 1) x)
                 (string-append (substring v 0 1) y)
              )
             )
           )
          )
    ))

;; es. 3
(define R-B-tassellations ; val: lista di liste
  (lambda (n)             ; n: intero positivo
    (cond ((= n 0)        ; caso base: lista vuota
           (list '())
           )
          ((= n 1)        ; caso 2: lista con min. n* el.
           (list '(#\R) '(#\B))
           )      
          (else
           (append
            (map (add-leftmost-tiles '(#\R #\B))  ; #\R #\B
                 (R-B-tassellations (- n 2))
                 )
            (map (add-leftmost-tiles '(#\B))  ; #\B
                 (R-B-tassellations (- n 1))
                 )
            ))
        )
    ))

(define add-leftmost-tiles
  (lambda (tiles)    ; titles: lista di caratteri
    (lambda (x)
      (append tiles x)
      )
    ))


;; es. 4
; Propietà generale:
; V n, k, x, y, z in N t.c. k<=n . (cube-rec n k x y z) -*-> z + (n-k)*(y + (n-k-1)*(x/2 + n-k-2) )

; Casi base, per v = n - k = 0:
; V n, x, y, z in N . (cube-rec n n x y z) -*-> z + (n-n)*(y + (n-n-1)*(x/2 + n-n-2) ) = z

; Ipotesi induttiva:
; consideriamo un valore intero v >= 0 e assumiamo che:
; V n, x, y, z >=0 . (cube-rec n n-v x y z) -*-> z + v*(y + (v-1)*(x/2 + v-2) )

; Passo induttivo:
; Per v considerato sopra
; V n, x, y, z >=0 . (cube-rec n n-v-1 x y z) -*-> z + (v+1)*(y + v*(x/2 + v-1) )


; Dimostrazione
; Intero v>=0, V n, x, y, z >=0.

; (cube-rec n n-v-1 x y z)
; -*-> (cube-rec n (+1 n-v-1) (+ 6 x) (+ x y) (+ y z))
; -*-> (cube-rec n n-v         x + 6  (+ x y) (+ y z))
; -*-> (cube-rec n n-v         x + 6   x + y   y + z )
;       applicazione ipotesi induttiva, per 1*,3*,4*,5* elemento.
; -*-> y+z + v*(x+y + (v-1)*(x/2+3 + v-2) )

; z + (v+1)*(y + v*(x/2 + v-1) )
; z + (v+1)*(y + xv/2 + v^2-v) )
; z + yv + y + xv^2/2 + xv/2 + v^3 + v^2 - v^2 - v
; 1)  z + yv + y + xv^2/2 + xv/2 + v^3 - v

; y+z + v*(x+y + (v-1)*(x/2+3 + v-2) )
; y+z + v*(y + xv/2 + x/2 + v^2 - 1) )
; 2)  y + z + yv + xv^2/2 + xv/2 + v^3 - v

; le due espressioni sono uguali, con il passo induttivo, e quindi la propietà vale in generale.