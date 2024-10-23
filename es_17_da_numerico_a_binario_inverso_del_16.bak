;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_17_da_numerico_a_binario_inverso_del_16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; esercizio inverso da numero a binario con bin-rep:
(define bin-rep ; val: stringa di 0/1
  (lambda (n)   ; n: intero non negativo
    (let (
          (q (quotient n 2))    ; 46 e non 46,5 (divisione intera)
          (r (remainder n 2))  ; resto della divisione per 2
          )
      (if (= q 0)  ; r>=1:vuol dire che non c'è niente davanti l'ultima cifra
          (bit-rep r)
          (string-append  ; dal valore alla stringa
           (bin-rep q)
           (bin-rep r)
           ))
      )
    ))

(define bit-rep  ; val: stringa "0" oppure "1" (resto divisione per 2 = 0)
  (lambda (v)    ; v: 0 oppure 1
    (if (= v 0)
        "0"
        "1"
        )
     ))