;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_1_laboratorio_vers_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (sog-art sog)    ; bu, senza lambda
    (cond
     ((sog-o sog)(string-append "il " sog))
      ((sog-i sog)(string-append "i " sog))
       ((sog-a sog)(string-append "la " sog))
      ((sog-e sog)(string-append "le " sog))   
      )
  )

(define sog-o
  (lambda (str)
    (char=? (string-ref str (-(string-length str)1))#\o)
    )
 )
(define sog-i
  (lambda (str)
    (char=? (string-ref str (-(string-length str)1))#\i)
    )
 )
(define sog-a
  (lambda (str)
    (char=? (string-ref str (-(string-length str)1))#\a)
    )
 )
(define sog-e
  (lambda (str)
    (char=? (string-ref str (-(string-length str)1))#\e)
    )
 )


(define verb
  (lambda (sog verb)
    (cond
      ((and(verb-are verb)(or(sog-i sog)(sog-e sog)))(string-append(rad-verb verb)"ano"))
      ((and(verb-are verb)(or(sog-o sog)(sog-a sog)))(string-append(rad-verb verb)"a"))
      ((and(verb-ere verb)(or(sog-i sog)(sog-e sog)))(string-append(rad-verb verb)"ono"))
      ((and(verb-ere verb)(or(sog-o sog)(sog-a sog)))(string-append(rad-verb verb)"e"))
      ((and(verb-ire verb)(or(sog-i sog)(sog-e sog)))(string-append(rad-verb verb)"ono"))
      ((and(verb-ire verb)(or(sog-o sog)(sog-a sog)))(string-append(rad-verb verb)"e"))
    )
  ))

(define verb-are
  (lambda (are)
    (string=?(substring are(-(string-length are)3)(string-length are))"are")))
(define verb-ere
  (lambda (ere)
    (string=?(substring ere(-(string-length ere)3)(string-length ere))"ere")))
(define verb-ire
  (lambda (ire)
    (string=?(substring ire(-(string-length ire)3)(string-length ire))"ire")))

(define rad-verb
  (lambda (inf)
    (substring inf 0(-(string-length inf)3))
    ))



(define frase
  (lambda (a b c)
    (string-append (sog-art a)" " (verb a b)" "(sog-art c))
  )
)