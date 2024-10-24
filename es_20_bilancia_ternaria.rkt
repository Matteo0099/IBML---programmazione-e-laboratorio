;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_20_bilancia_ternaria) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; immaginiamo che mettiamo sulla bilancia ternaria dei pesi:
; 47 g = 20 + 10 + 10 + 5 + 2 = 50 - (2 + 1).
; 1 3 9 27 = 40g (minimo pesi campione = 4).

(define btr-val   ; val (restituito): intero  (r=estesa)
  (lambda (btr)   ; btr: stringa di -/./+
    (let (
          (k (- ((string-length btr) 1)) ; cifra meno significativa
          )
      (if (= k 0)
          (btd-val btr)        ; (d=singola cifra)
          (+ (* 3 (btr-val (substring btr 0 k)))  ; calcolo il valore in qs sist. bilanciato
             (btd-val (substring btr k)) 
             ))
      )
    ))

(define btd-val  ; val: intero
  (lambda (btd)  ; btd: "-", ".", "+"
    (cond ((string=? btd "-") -1) 
          ((string=? btd ".") 0)
          ((string=? btd "+") 1)
          )
    ))