;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define plurale-sf      ; val: stringa
  (lambda (sf)          ; sm: stringa
    (string-append (radice-sost sf) "e")
    ))
(define radice-sost     ; val: stringa
  (lambda (s)           ; s: stringa
    (substring s 0 (- (string-length s) 1))
  ))
