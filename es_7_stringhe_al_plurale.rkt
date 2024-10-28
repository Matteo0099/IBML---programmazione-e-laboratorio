;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_7_stringhe_al_plurale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define plurale-sm      ; val: stringa
  (lambda (sm)          ; sm: stringa
    (string-append (radice-sost sm) "i")
    ))
(define radice-sost     ; val: stringa
  (lambda (s)           ; s: stringa
    (substring s 0 (- (string-length s) 1))
  ))
