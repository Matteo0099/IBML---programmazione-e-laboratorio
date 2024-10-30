;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_22_problema_piastrelle_gialle_blu_rosse_sequenza) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define tassellaz-1-2  ; val: intero
  (lambda (n)          ; n: intero positivo (lunghezza del cordolo)
    (cond ((= n 1)
           1)
          ((= n 2)
           2)
          (else
           (+
            (tassellaz-1-2 (- n 1))  ; quadrata a sinistra
            (tassellaz-1-2 (- n 2))  ; rettangolare a sinistra
            )
           )
          )
    ))