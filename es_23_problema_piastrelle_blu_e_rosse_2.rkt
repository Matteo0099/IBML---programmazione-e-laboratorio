;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_23_problema_piastrelle_blu_e_rosse_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define tassellaz-b-r  ; val: intero
  (lambda (n)          ; n: intero positivo (lunghezza del cordolo)
    (cond ((= n 1)
           2)
          ((= n 2)
           3)
          (else
           (+
            (tassellaz-b-r (- n 1))  ; quadrata a sinistra
            (tassellaz-b-r (- n 2))  ; rettangolare a sinistra
            )
           )
          )
    ))