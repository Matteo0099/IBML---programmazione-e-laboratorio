;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_30_moltiplicazione_del_contino_russo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Moltiplicazione dle contino russo
(define molt-russa  ; intero
  (lambda (m n)     ; m, n: interi positivi
    (cond ((= n 0)
          0)
          ((even? n)
           (molt-russa (* 2 m) (quotient n 2))) ; 2m * n/2 = m*n
          (else
           (+ m (molt-russa (* 2 m) (quotient n 2)))) ; 2m * (n-1)/2 = m*(n-1) = m*n-m
     )
  ))


; versione più precisa:
; funzione delega
(define molt-cont     ; intero
  (lambda (m n)       ; m, n: interi positivi
    (molt-rec m n 0)
    ))

(define molt-rec    ; intero
  (lambda (m n p)   ; m, n, p: interi positivi
    (cond ((= n 0)
           p)
          ((even? n)
           (molt-rec (* 2 m) (quotient n 2) p))  ; 2m * n/2 = m*n
          (else
           (+ m (molt-rec (* 2 m) (quotient n 2) (+ m p)))) ; 2m * (n-1)/2 = m*(n-1) = m*n-m
      )
    ))
