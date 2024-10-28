;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_3_plurale_maschile_radice_sost) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define plurale-sm      ; val: stringa
  (lambda (sm)          ; sm: stringa
    (string-append (radice-sost sm) "i")
    ))
(define radice-sost     ; val: stringa
  (lambda (s)           ; s: stringa
    (substring s 0 (- (string-length s) 1))
  ))

(define femminile?      ; sara' Booleano
  (lambda (s)           ; s: stringa
    ;(string=? (substring s (- (string-length s) 1)) "a") ;confronto con =? con due stringhe
    (char=? (string-ref s (- (string-length s) 1)) #\a) ;confronto con =? con due stringhe
    ))