;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_32_lez_DDM_cifrario_decr_rot_rgl_cesare) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DECIFRATURA IN FONDO ;;

(define aA (char->integer #\A))
(define aZ (char->integer #\Z))
(define n-car 26)

(define rgl-cesare  ; val: char (lettera maiuscola)
  (lambda (crt)     ; crt: char (lettera maiuscola)
    (let (
          (new-ascii (+ 3 (char->integer crt)))
          )
      (if (<= new-ascii aZ)
          (integer->char new-ascii)
          (integer->char (- new-ascii n-car))
          )
      )
   ))


; aggiunta:
(define rgl-augusto
  (lambda (crt)
    (let (
          (new-ascii (+ 1 (char->integer crt)))
          )
      (if (<= new-ascii aZ)
          (integer->char new-ascii)
          (integer->char (- new-ascii n-car))
          )
      )
    ))

(define crittazione-cesare
  (lambda (msg)
    (if (string=? msg "")
        ""
        (string-append
         (string (rgl-cesare (string-ref msg 0)))
         (crittazione-cesare (substring msg 1))
         )
        )
    ))

(define crittazione-augusto
  (lambda (msg)
    (if (string=? msg "")
        ""
        (string-append
         (string (rgl-augusto (string-ref msg 0)))
         (crittazione-augusto (substring msg 1))
         )
        )
    ))

(define crittazione  ; val: stringa
  (lambda (msg rgl)      ; msg: stringa, rgl: funzione/procedura [char -> char]
    (if (string=? msg "")
        ""
        (string-append
         (string (rgl (string-ref msg 0)))
         (crittazione (substring msg 1))
         )
        )
    ))

(define cifrario   ; funzione/procedura [char->char]
  (lambda (rot)    ; rot: intero positivo
    (lambda (crt)     ; oggetto: tipo funzione, restituito; crt: char.
      (let (
            (new-ascii (+ rot (char->integer crt)))
            )
        (if (<= new-ascii aZ)
            (integer->char new-ascii)
            (integer->char (- new-ascii n-car))
            )
        )
      )
    ))


;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; DECIFRATURA ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define rot-cesare 3)
(define rot-decr-cesare (- n-car rot-cesare))
(define rgl-decr-cesare (cifrario rot-decr-cesare))

; decifratura (algoritmo inverso)
(define genera-rgl-inversa  ; val: char->char
  (lambda (rgl)             ; rgl: char->char (rgl => regola)
    (lambda (c)             
      (let ((rot (-(char->integer(rgl #\A)) aA)))   ; diff: 26-cost.rot.
            (cifrario (- n-car rot))                ; restituisce la regola di rotazione
        )
      )
    ))

; input
; (genera-rgl-inversa(rgl-cesare(#\a)))
