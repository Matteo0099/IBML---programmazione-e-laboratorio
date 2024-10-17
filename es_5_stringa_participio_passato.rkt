;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_stringa_participio_passato) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define part-passato-1-3 ; val: stringa  
  (lambda (inf13)       ; inf: stringa
    (string-append (radice-verb inf13)
                   (substring inf13 (-(string-length inf13) 3) (-(string-length inf13) 2))
                   "to")
    ))

(define radice-verb   ; val: stringa
  (lambda (inf)      ; inf: stringa
    (substring inf 0 (- (string-length inf) 3))
    ))