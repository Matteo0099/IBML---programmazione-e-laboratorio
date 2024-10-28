;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_11_part_passato_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define radice-verb   ; val: stringa
  (lambda (inf)      ; inf: stringa
    (substring inf 0 (- (string-length inf) 3))
    ))

(define part-passato-1-3 ; val: stringa  
  (lambda (inf)       ; inf: stringa
    (string-append (radice-verb inf)
                   (substring inf (-(string-length inf) 3) (-(string-length inf) 2))
                   "to")
    ))

(define part-passato-2 ; val: stringa  
  (lambda (inf)       ; inf: stringa
    (string-append (radice-verb inf)
                   (substring inf (-(string-length inf) 3) (-(string-length inf) 2))
                   "ere")
    ))

(define seconda-coniug?
  (lambda (inf)
    (char=? (string-ref inf (- (string-length inf) 3)) #\e)
    ))

(define part-passato
  (lambda (inf)
    (if (seconda-coniug? inf)
        (part-passato-2 inf)
        (part-passato-1-3 inf)
        )
    ))