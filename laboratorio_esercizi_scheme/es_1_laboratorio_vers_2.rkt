;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_1_laboratorio_vers_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Funzione che verifica se una stringa termina con una certa desinenza
(define (string-suffix? str suffix)
  (let ((start (- (string-length str) (string-length suffix))))
    (and (>= start 0)
         (string=? (substring str start) suffix))))

; Funzione per ottenere l'articolo del soggetto
(define (sog-art sog)
  (cond
    ((sog-finale? sog #\o) (string-append "il " sog))
    ((sog-finale? sog #\i) (string-append "i " sog))
    ((sog-finale? sog #\a) (string-append "la " sog))
    ((sog-finale? sog #\e) (string-append "le " sog))
    (else sog)))

; Funzione per verificare l'ultima lettera del soggetto
(define (sog-finale? str ch)
  (char=? (string-ref str (- (string-length str) 1)) ch))

; Funzione per determinare la coniugazione del verbo (are, ere, ire)
(define (verbo-coniug? verb desinenza)
  (string-suffix? verb desinenza))

; Funzione per ottenere il radicale del verbo rimuovendo le ultime 3 lettere
(define (rad-verb verb)
  (substring verb 0 (- (string-length verb) 3)))

; Funzione per coniugare il verbo in base al soggetto e alla sua desinenza
(define (conj-verb sog verb)
  (let ((rad (rad-verb verb)))
    (cond
      ((and (verbo-coniug? verb "are") (or (sog-finale? sog #\i) (sog-finale? sog #\e))) (string-append rad "ano"))
      ((and (verbo-coniug? verb "are") (or (sog-finale? sog #\o) (sog-finale? sog #\a))) (string-append rad "a"))
      ((and (verbo-coniug? verb "ere") (or (sog-finale? sog #\i) (sog-finale? sog #\e))) (string-append rad "ono"))
      ((and (verbo-coniug? verb "ere") (or (sog-finale? sog #\o) (sog-finale? sog #\a))) (string-append rad "e"))
      ((and (verbo-coniug? verb "ire") (or (sog-finale? sog #\i) (sog-finale? sog #\e))) (string-append rad "ono"))
      ((and (verbo-coniug? verb "ire") (or (sog-finale? sog #\o) (sog-finale? sog #\a))) (string-append rad "e"))
      (else verb))))

; Funzione per costruire una frase con soggetto, verbo e complemento
(define (frase sog verb comp)
  (string-append (sog-art sog) " " (conj-verb sog verb) " " (sog-art comp)))