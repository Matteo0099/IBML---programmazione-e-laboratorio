;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_32_lez_DDM_cifrario_decr_rot_rgl_cesare) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 21/11
(define rot-cesare 3)
(define rot-decr-cesare (- n-car rot-cesare))
(define rgl-decr-cesare (cifrario rot-decr-cesare))

