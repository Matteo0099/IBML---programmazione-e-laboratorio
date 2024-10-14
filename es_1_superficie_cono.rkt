;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_1_superficie_cono) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(define sup-tot-cilindro ; val; reale
 ; (lambda (r h)          ; r, h: reali (misure)
  ;  (* (* (* 2 3.14) r) (+ r h)) ; restituisce un valore
   ; ))

;(*(* 5 3.14) (+ 5.0 (sqrt(+(expt 5.0 2)(expt 10.2 2)))))

(define sup-tot-cono
  (lambda (r h)
    (*(* r 3.14) (+ r (sqrt(+(expt r 2)(expt h 2)))))
    ))