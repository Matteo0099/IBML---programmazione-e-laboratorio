;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_8_2_coniugazione) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;esercizio con verbo 2 coniugazione
(define seconda-coniug?
  (lambda (s)
    (char=? (string-ref s (-(string-length s) 3)) #\e)
    ))
