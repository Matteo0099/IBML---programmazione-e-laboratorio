;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_13_string_reverse) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define string-reverse  ; val: stringa
  (lambda (str)         ; str: stringa
    (if (string=? str "") ; confronto se la stringa è vuota (= (string-length str) 0)
        ""
        (string-append   ; altrimenti non è vuota
         (string-reverse (substring str 1))
         (substring str 0 1)
         )
        )
    ))