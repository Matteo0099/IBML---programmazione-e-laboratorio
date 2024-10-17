;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_10_complemento_a_uno_bit) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define compl-a-uno            ; val: stringa di 0/1
  (lambda (seq)          ; seq: stringa di 0/1 lunga...
    (if (> (string-length seq) 1)
        ; almeno 2 bit
        (string-append
         (bit-compl(substring seq 0 1))  ; colore verde (primo bit pos 0)
         (compl-a-uno (substring seq 1))    ; tutto il resto rosso (2bit...ultimo bit)
         )
        ; esattamente 1 bit
        (bit-compl seq)
        )
    ))

(define bit-compl   ; val: stringa
  (lambda (bit)     ; bit: stringa a "0" oppure "1"
    (if (string=? bit "0")
        "1"
        "0"
        )
    ))