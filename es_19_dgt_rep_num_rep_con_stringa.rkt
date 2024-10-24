;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_19_dgt_rep_num_rep_con_stringa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; esercizio inverso da numero a binario con bin-rep:
(define num-rep  ; val: stringa di 0/1
  (lambda (n b)  ; n: intero non negativo, b: ... [2, 16]
    (let (
          (q (quotient n b))    
          (r (remainder n b))  
          )
      (if (= q 0)
          (dgt-rep r)     ; casi base
          (string-append
           (num-rep q b)
           (dgt-rep r)
           ))
      )
    ))

(define dgt-rep  ; val: stringa di una sola cifra
  (lambda (v)    ; v: 0...16?
    (string (integer->char (+ ascii-0 v))) ; v < 10
    ; ... esercizio
     ))

(define ascii-0 (char->integer #\0))