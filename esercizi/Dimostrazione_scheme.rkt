;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Dimostrazione scheme|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
(define f ; val: intero
  (lambda (x y) ; x ≥ 0, y > 0 interi
    (if (< x y)
        1
        (+ (f (- x 1) y) (f (- x y) y))
        )))

; 1) Per ogni n e y tali che n ≥ 0, y = 1; (f n 1) -> 2^n
; 2) Ponendo 1 > n ≥ 0; n = 0              (f 0 1) -> 2^0 = 1
; 3) Scelgo n E N ≥ 0                      (f n 1) -> 2^n
; 4) Si fa ipotesi induttiva che           (f n+1 1) -> 2^(n+1)

; 1) Ponendo 1 > n ≥ 0; n = 0 (if (< 0 1) 1  ...  ) [1 = 2^0]
; 2) Per n scelto dimostro                 (f n+1 1) -> 2^(n+1)
;    (if (< n+1 1)) ...
;    caso falso [1]
;    (+ (f n 1) (f n 1)) [trovo ipotesi induttiva]
;    2^n + 2^n = 2^(n+1)