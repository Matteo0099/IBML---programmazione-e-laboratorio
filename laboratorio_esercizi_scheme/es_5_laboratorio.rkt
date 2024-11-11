;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_5_laboratorio) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (L-tessellation n)
  (letrec (
           ; Draw an L-tile at a specific location
           [draw-L-tile-at (lambda (x y size color)
             (draw-L-tile-at x y size color))]  ; Assuming `draw-L-tile` in `tiles.rkt` takes x, y, size, and color

           ; Recursive tessellation function
           [tessellate (lambda (x y size color)
             (if (= size 1)
                 (draw-L-tile-at x y size color)
                 (let ((half (/ size 2))
                       (new-color (color)))
                   (begin
                     (tessellate x y half new-color)
                     (tessellate (+ x half) y half new-color)
                     (tessellate x (+ y half) half new-color)
                     (tessellate (+ x half) (+ y half) half new-color)
                     (draw-L-tile-at (+ x half) (+ y half) half new-color)))))])
    ; Start the tessellation from the origin with the initial color
    (tessellate 0 0 n 'yellow)))  ; Start with yellow as the initial color
