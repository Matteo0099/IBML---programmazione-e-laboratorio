;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_4_laboratorio) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (factorial n)
  (if (= n 0) 1
      (* n (factorial (- n 1)))))

(define (manhattan-3d i j k)
  (let* ((total (+ i j k))
         (numerator (factorial total))
         (denominator (* (factorial i) (factorial j) (factorial k))))
    (/ numerator denominator)))


(manhattan-3d 0 0 7)  ; => 1
(manhattan-3d 2 0 2)  ; => 6
(manhattan-3d 1 1 1)  ; => 6
(manhattan-3d 1 1 5)  ; => 42
(manhattan-3d 2 3 1)  ; => 60
(manhattan-3d 2 3 3)  ; => 560

;(i+j+k)  // ; interi: (i,j,k)
; = 
;[(i!⋅j!⋅k)!]:
;  (i+j+k)!