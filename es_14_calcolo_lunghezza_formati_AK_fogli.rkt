;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_14_calcolo_lunghezza_formati_AK_fogli) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define s     ;val: reale (misura in cm)
  (lambda (k)   ;k: intero non negativo
    (if (< k 2)
        (if (= k 0) s0 s1)
        (/(s (- k 2)) 2)   ;tolgo da K=5 (A5) 2, e ottengo A3. poi dalla lunghezza /2 e ottengo il lato più lungo del formato A5. 
        )
    ))

(define s0 (* 100 (expt 2 1/4))) ; (lato lungo): 100cm * 2^1/4 (radice quarta di 2)
(define s1 (* 100 (expt 2 -1/4)))  ; (lato corto): inverso di 2^1/4