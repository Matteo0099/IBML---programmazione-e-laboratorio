;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_36_ufo_) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss.txt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss.txt" "installed-teachpacks")) #f)))
;; UFO = Unidentified Flying procedure

;; (ufo n) --> ??

(define ufo
  (lambda (x)
    (cond ((= x 1)
           1)
          ((even? x)
           (- (* 2 (ufo (quotient x 2))) 1))
          (else
           (+ (* 2 (ufo (quotient x 2))) 1))
          )
    ))

;; cosa fa il programma: se x = 2^k --> il valore della f(n) = 1.