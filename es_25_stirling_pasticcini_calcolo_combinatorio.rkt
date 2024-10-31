;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_25_stirling_pasticcini_calcolo_combinatorio) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define stirling
  (lambda (n k)   ; n,k: interi positivi t.c. [1<=k<=n]
    ; ricorsione
    (if (or (= k 1) (= k n))
        1
        (+ (stirling (- n 1) (- k 1))   ; (k > 1) dispongo i pasticcini in tutti - quello del canestrello
           (* k (stirling (- n 1) k))  ; (k < n) dispongo l'altro, meno sé stesso in tutti gli altri.
           ))
    ))