;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_21) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; esercizio 1 laboratorio 28.10.2024

(define comport-verb   ; val: stringa
  (lambda (str)      ; inf: stringa
    (substring str (- (string-length str) 3))  ; togliere gli ultimi 3 caratteri   
    ))

; gestione casi ire, ere.
(define case-ire
  (lambda (str)
    (string-append (str comport-verb) "ono")
    ))

(define case-are
  (lambda (str)
    (string-append (str comport-verb) "a")
    ))

(define case-ere
  (lambda (str)
    (string-append (str comport-verb) "e")
    ))

(define terz-coniug?
  (lambda (inf)
    (char=? (string-ref inf (- (string-length inf) 3)) #\e)
    ))

(define pred-verb
  (lambda (str)
    (cond (case-are str)
        (case-ire str)
        (case-ere str)
        )
    ))
       