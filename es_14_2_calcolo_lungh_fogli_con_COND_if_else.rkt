;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_14_2_calcolo_lungh_fogli_con_COND_if_else) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define s
  (lambda (k)
    ; (if (<k 2)
    ;     (if (=k 0) s0 s1)
    ;     (/ (s (-k 2)) 2)
    (cond ((= k 0)
           s0)
          ((= k 1)
           s1)
          ((> k 1)
           (/ (s (- k 2)) 2))
      )
    ))

(define s0 (* 100 (expt 2 1/4)))
(define s1 (* 100 (expt 2 -1/4)))