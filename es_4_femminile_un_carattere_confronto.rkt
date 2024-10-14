;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname femminile_un_carattere_confronto) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define femminile?      ; sara' Booleano
  (lambda (s)           ; s: stringa
    (char=? (string-ref s (- (string-length s) 1)) #\a) ;confronto con =? con due stringhe
    ))
