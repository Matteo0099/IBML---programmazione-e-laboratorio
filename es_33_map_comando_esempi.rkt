;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_33_map_comando_esempi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define mappa     ; val: lista di I
  (lambda (f s)   ; f: D->I, s: lista di D
    (if (null? s)
        null
        (cons (f (car s)) (mappa f (cdr s)))
     )
   ))


; I codici crittaz. sono solo per provare "map" DA TERMINALE:
; (mappa (lambda (s) (encryption s (caesar-cipher 3))) '("ALEA" "IACTA" "EST"))
; (mappa (lambda (s) (encryption s (caesar-cipher 23))) '("DOHD" "LDFWD" "HVW"))


(define all-lcs
  (lambda (u v)
    (cond ((or (string=? u "") (string=? v ""))
           (list "")
           )
           ((char=? (string-ref u 0) (string-ref v 0))
            ; procedura con map
            (map                                               ; procedura map: applica string-append ad una stringa variabile.
             (lambda (x) (string-append (substring u 0 1) x))  ; ogni stringa viene pescata dalla lista che segue.
             (all-lcs (substring u 1) (substring v 1))
            ))
           (else
            (all-longer
             (all-lcs (substring u 1) v)
             (all-lcs u (substring v 1))
             ))
           )
    ))

(define all-longer  ; val: lista di stringhe
  (lambda (s t)     ; s, t: liste di stringhe
    (let ((m (string-length (car s)))
          (n (string-length (car t)))
          )
      (cond ((< m n)
             t)
            ((> m n)
             s)
            (else
             (append s t)
             )
        ))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;

;; funzione composta, matematica in scheme.

; f: D-->E, g: E-->I

; g.f: D-->I

; x in D --> g(f(x)) in I

; (g.f)(x) = g(f(x))

;;;;;;;;;;;;;;;;;;;;;;;;;


(define comp      ; val: D-->I (procedura)
  (lambda (g f)   ; f: D-->E, g: E-->I  (procedure)
    (lambda (x)   ; x: D
      (g (f x))
      )
    ))

; f: D-->D, i: intero positivo
; f^i: D-->D
; (f^i)(x) = f(f(...f(x)...)) dove f compare i volte

(define iter      ; val: D-->D procedura
  (lambda (f i)   ; f: D-->D procedura, i: intero positivo
    (lambda (x)
      (if (= i 0)
          x
          (f ((iter f (- i 1)) x))
       ))
   ))

(define iterc    ; val: D-->D procedura
  (lambda (f i)  ; f: D-->D procedura, i: intero positivo
    (if (= i 0)
        (lambda (x) x)
        (comp f (iterc f (- i 1)))
        )
    ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Esempio: Codice di Giulio Cesare
;;
;; Temi affrontati:
;; 1. Procedure con argomenti procedurali
;; 2. Procedure con valori procedurali
;; 3. Procedure con argomenti e valori procedurali
;; Ultimo aggiornamento: 21/12/15


;; 1. Procedure con argomenti procedurali
;;
;; Semplice tecnica di crittazione di un messaggio:
;; ad ogni lettera viene applicata la stessa regola
;; di codifica indipendentemente dal contesto

(define encryption        ; valore: stringa
  (lambda (message rule)  ; message: stringa, rule: lettera -> lettera
    (if (= (string-length message) 0)
        ""
        (string-append
         (string (rule (string-ref message 0)))
         (encryption (substring message 1) rule)
         ))
    ))

;; Qual e' il risultato dell'applicazione:
;;
;;   (encryption "PROGRAMMAZIONE" (lambda (x) x)) ?


;; 2. Procedure con valori procedurali
;;
;; Regole che applicano lo schema del codice di Giulio Cesare:
;; Rotazioni di lettere

(define caesar-cipher     ; valore procedurale: lettera -> lettera
  (lambda (rot)           ; la funzione dipende da rot: integer
    (lambda (letter)      ; espressione procedurale: lettera -> lettera
      (let ((c (+ (char->integer letter) rot)))
        (if (> c codZ)
          (integer->char (- c 26))  ; 26 lettere dell'alfabeto Inglese
          (integer->char c)
          )))
    ))

(define codA (char->integer #\A))  ; codice ASCII della lettera A (costante)
(define codZ (char->integer #\Z))  ; codice ASCII della lettera Z (costante)


;; Esercizio:
;; Che valore assume la seguente espressione?
;;
;;   (encryption (encryption "PROGRAMMAZIONE" (caesar-cipher 3)) (caesar-cipher 23))
;;
;; Perche'?
;;
;; Esercizio:
;; Revisione della procedura "caesar-cipher" applicabile
;; esattamente all'alfabeto Latino (non a quello Inglese).
;; Le lettere dell'alfabeto al tempo di Cesare sono 20:
;;
;;   A B C D E F G H I L M N O P Q R S T V X


;; 3. Procedure con argomenti e valori procedurali
;;
;; Funzione di decrittazione data la funzione di crittazione
;; (biiezione che realizza una permutazione delle lettere maiuscole dell'alfabeto inglese):
;; "Rottura del codice"

(define compl-rotation  ; valore procedurale: lettera -> lettera
  (lambda (rule)        ; argomento procedurale rule: lettera -> lettera
    (caesar-cipher
      (- 26 (- (char->integer (rule #\A)) codA))  ; rotazione complementare
      )
    ))


;; Esempio:
;;
;;   (define enc-rule (caesar-cipher 3))
;;
;;   enc-rule
;;
;;   (encryption "IULIUSCAESAR" enc-rule)
;;
;;   (encryption "LXOLXVFDHVDU" (compl-rotation enc-rule))


;; Piu' in generale:
(define compl-permutation  ; valore procedurale: lettera -> lettera
  (lambda (rule)           ; argomento procedurale rule: lettera -> lettera
    (lambda (letter) (find-from codA letter rule))
    ))

(define find-from               ; valore: lettera
  (lambda (sample letter rule)  ; rule: lettera -> lettera, sample: intero, letter: lettera
    (let ((c (integer->char sample)))
      (if (char=? (rule c) letter)
        c
        (find-from (+ sample 1) letter rule)
        ))
    ))


;; Esempio di applicazione:
;;
;; (define enc-rule (caesar-cipher 3))
;; (define dec-rule (compl-permutation enc-rule))
;; (define encrypted-message (encryption "JULIUSCAESAR" enc-rule))
;; encrypted-message
;; (encryption encrypted-message dec-rule)


;; Crittazione di sequenze (liste) di parole:
(define words-encryption  ; lista di stringhe
  (lambda (enc words)     ; enc: procedura, words: lista di stringhe
    (if (null? words)
        null
        (cons (enc (car words)) (words-encryption enc (cdr words)))
        )))


;; Esempio:
;;
;;   (define caesar-rule (caesar-cipher 3))
;;   (words-encryption (lambda (w) (encryption w caesar-rule)) '("ALEA" "IACTA" "EST"))
;;   (words-encryption (lambda (w) (encryption w (compl-permutation caesar-rule))) '("DOHD" "LDFWD" "HVW"))
;;
;; Ma anche:
;;
;;   (map (lambda (w) (encryption w caesar-rule)) '("ALEA" "IACTA" "EST"))
;;   (map (lambda (w) (encryption w (compl-permutation caesar-rule))) '("DOHD" "LDFWD" "HVW"))


;; Applicazione della procedura "map":
;; Lista di tutte le sottosequenze comuni piu' lunghe
;(define all-lcs  ; valore: lista di stringhe
  ;(lambda (u v)  ; u, v: stringhe
    ;(cond ((or (string=? u "") (string=? v ""))
          ; (list ""))
          ;((char=? (string-ref u 0) (string-ref v 0))
          ; (map (lambda (x) (string-append (substring u 0 1) x))
           ;     (all-lcs (substring u 1) (substring v 1))))
          ;(else
         ;  (all-better (all-lcs (substring u 1) v)
         ;              (all-lcs u (substring v 1))))
         ; )))

(define all-better  ; valore: stringa
  (lambda (u v)     ; u, v: stringhe
    (let ((m (string-length (car u))) (n (string-length (car v))))
      (cond ((< m n)
             v)
            ((> m n)
             u)
            (else
             (append u v))
            ))
    ))

;; Esercizio: raffinamento di "all-better" per evitare le ripetizioni!


;; Procedure a valori procedurali:
;; Funzione P(x) denotata da un polinomio di grado n in una variabile,
;; data la lista dei coefficienti (c0 c1 c2 ... cn)

(define pol-fun  ; val: procedura (da reali a reali)
  (lambda (cs)   ; cs:  lista coefficienti di x^0, x^1, x^2...
    (lambda (x)                             ; P(x)
      (if (null? (cdr cs))
          (car cs)                          ; c0
          (+ (car cs)                       ; c0 +
             (* x ((pol-fun (cdr cs)) x)))  ; x * Q(x)
          ))
    ))

;; dove Q(x) e' la funzione denotata dal polinomio di grado n-1
;; la cui lista di coefficienti e' (c1 c2 ... cn)


;; Operatori di ordine superiore: composizione funzionale e iterazione

;; Composizione funzionale
(define composition     ;  composizione di funzioni g, f : D -> D
  (lambda (g f)         ;  rappresentate da procedure in Scheme
    (lambda (x) (g (f x)))
    ))


;; Composizione iterata di una funzione
(define iterate         ;  valore procedurale: D -> D
  (lambda (f i)         ;  f : D -> D funzione, i naturale
    (if (= i 0)
        (lambda (x) x)  ; funzione identica, per i=0
        (composition f (iterate f (- i 1)))
        )
    ))

;; Ma anche:
;; (define iterate       ;  valore procedurale: D -> D
;;   (lambda (f i)       ;  f : D -> D funzione, i naturale
;;     (lambda (x)
;;       (if (= i 0)
;;           x
;;           (f ((iterate f (- i 1)) x))
;;           ))
;;     ))