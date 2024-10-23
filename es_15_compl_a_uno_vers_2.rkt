;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_15_compl_a_uno_vers_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define bit-compl   ; val: stringa
  (lambda (bit)     ; bit: stringa a "0" oppure "1"
    (if (string=? bit "0")
        "1"
        "0"
        )
    ))

(define compl-a-1  ; val: stringa
  (lambda (seq)      ; val: stringa di 0/1 non vuota
    (let ((k (quotient (string-length seq) 2))) ; prima parte
      (if (= (string-length seq) 1)
          (bit-compl seq)   ; 1 bit solo
          (string-append
           ; corrispondono ai due pezzettini sx e dx nel diagramma.
           (compl-a-1 (substring seq 0 k))
           (compl-a-1 (substring seq k))
           ))
      )
    ))


; costrutto let
; (let ((x1 E1) (x2 E2) ... (xk Ek) E [x1,x2,...,xk] )