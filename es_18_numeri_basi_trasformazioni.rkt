;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_18_numeri_basi_trasformazioni) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define num-val
  (lambda (num b) ; num: stringa di cifre, b: [2, 10] --> [2, 16]
    (let (
          (k (- (string-length num) 1))
          )
      (if (= k 0)
          (dgt-val (string-ref num k))
          (+ (* b (num-val (substring num 0 k) b)) 
             (dgt-val (string-ref num k))
             ))
      )
    ))

(define dgt-val   ; val: intero
  (lambda (dgt)   ; bit: cifra
    (- (char->integer dgt) ascii-0) ; dgt: [#\0, #\9]
    ; cifre seguenti? (esercizio)
    ))

(define ascii-0 (char->integer #\0))