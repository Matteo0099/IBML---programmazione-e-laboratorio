;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |es_35_esercizi_tema_d'esame_scheme|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 1 ;;
;; Problema simile all'append di liste (DIVERSE)

(define merge    ; val: lista di stringhe
  (lambda (u v)  ; u,v: lista di stringhe
    (cond ((null? u)                 ; caso base: se u è vuota
           v)
          ((belong? (car u)  v)      ; caso in cui il primo elemento della prima lista appartiene alla seconda.
           (merge (cdr u) v))       
          (else                      ; altrimenti: 
           (cons (car u) (merge (cdr u) v)))
      )
   ))

(define belong?   ; val: booleano
  (lambda (e s)   ; e: stringa, s: lista di stringhe
    (cond ((null? s)
           false)
          ((string=? e (car s))
           true)
          (else
           (belong? e (cdr s)))
      )
   ))


;; 2 ;;
;; Problema argomenti procedurali in scheme (simile a longer, lcs)
;; input:   (lps "irradiare") --> "radar"

(define lps-all     ; val: lista di stringhe
  (lambda (s)       ; s: stringa
    (let ((n (string-length s)))
      ; casi possibili
      (cond ((< n 2)    ; meno di due caratteri allora "a","b"...
             (list s))
            ((char=? (string-ref s 0) (string-ref s (- n 1)))  ; 1* e ultimo carattere uguali
             (map (lambda (x) (string-append (substring s 0 1) x (substring s 0 1))) ; concatena i caratteri
                  (lps-all (substring s 1 (- n 1))))) ; esclude il 1* e ultimo carattere
            (else
             (all-longer (lps-all (substring s 0 (- n 1))) ; esclude ultimo carattere
                         (lps-all (substring s 1))))      ; esclude il primo carattere
      ))))

(define all-longer  ; val: lista di stringhe
  (lambda (s t)     ; s,t: liste di stringhe
    (let ((m (string-length (car s)))
          (n (string-length (car t)))
          )
      (cond ((< m n)
             t)
            ((> m n)
             s)
            (else
             (merge s t))
       )
      )
    ))
