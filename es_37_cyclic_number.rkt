;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_37_cyclic_number) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss.txt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss.txt" "installed-teachpacks")) #f)))
(define cyclic-number
  (lambda (s k)
    (if (< (string-length s) k)
        0
        (+ 1 (cn-rec (substring s k) k (substring s 0 k)))
        )
    ))

(define cn-rec
  (lambda (s k p)
    (cond ((< (string-length s) k)
           0)
          ((string=? (substring s 0 k) p)
           (+ 1 (cn-rec (substring s k) k p)))
          (else
           0)
          )
    ))