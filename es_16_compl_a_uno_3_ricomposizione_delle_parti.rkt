;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_16_compl_a_uno_3_ricomposizione_delle_parti) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define bin-val
  (lambda (bin) ; bin: stringa di 0/1
    (let ((k (- (string-length bin) 1)))
      (if (= k 0)
          (bit-val (string-ref bin k))
          (+ (* 2 (bin-val (substring bin 0 k))) ; rosso
             (bit-val (string-ref bin k))) ; verde
          ))
    ))

(define bit-val
  (lambda (bit) ; bit: carattere #\0, #\1
    ;(if (char=? bit #\0) 0 1)
    (- (char->integer bit) ascii-0) ; metodo 2 con ascii
    ))

; metodo 2 con ascii:
(define ascii-0 (char->integer #\0))