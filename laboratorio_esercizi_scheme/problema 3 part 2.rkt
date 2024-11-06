;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |problemo 3 part 2|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define (get-sign number-string)
  (if (char=? (string-ref number-string 0) #\-)
      -1
      1))

(define (find-char-index char base-string)
  (let loop ((chars (string->list base-string))
             (index 0))
    (cond ((null? chars) #f) ; Return #f if character is not found
          ((char=? char (car chars)) index)
          (else (loop (cdr chars) (+ index 1))))))

(define (convert-integer-part integer-string base-string)
  (let loop ((digits (string->list integer-string))
             (acc 0))
    (if (null? digits)
        acc
        (let* ((digit-value (find-char-index (car digits) base-string)))  ; Get index for the digit
          (if (eq? digit-value #f)
              (error "Invalid digit" (car digits)) ; Handle invalid digits
              (loop (cdr digits)
                    (+ (* acc (string-length base-string)) digit-value))))))) ; Calculate the integer value

(define (convert-fractional-part fractional-string base-string)
  (let loop ((digits (string->list fractional-string))
             (acc 0)
             (position 1)) ; Start position from 1
    (if (null? digits)
        acc
        (let* ((digit-value (find-char-index (car digits) base-string))) ; Get index for the digit
          (if (eq? digit-value #f)
              (error "Invalid digit" (car digits)) ; Handle invalid digits
              (loop (cdr digits)
                    (+ acc (/ digit-value (expt (string-length base-string) position)))
                    (+ position 1)))))))

(define (rep->number base-string number-string)
  (let* ((sign (get-sign number-string))
         (clean-string (if (or (char=? (string-ref number-string 0) #\+)
                               (char=? (string-ref number-string 0) #\-))
                           (substring number-string 1)
                           number-string))
         (dot-position (find-char-index #\. clean-string))
         (integer-part (if (eq? dot-position #f)
                           clean-string
                           (substring clean-string 0 dot-position)))
         (fractional-part (if (eq? dot-position #f)
                              ""
                              (substring clean-string (+ dot-position 1))))
         (integer-value (convert-integer-part integer-part base-string))
         (fractional-value (if (string=? fractional-part "")
                               0
                               (convert-fractional-part fractional-part base-string))))
    (* sign (+ integer-value fractional-value)))) ; Combine the values with the sign

(rep->number "zu" "-uuz")               ; => -12
(rep->number "0123" "+21.1")            ; => 9.25
(rep->number "01234" "-10.02")          ; => -5.08
(rep->number "0123456789ABCDEF" "0.A")  ; => 0.625
(rep->number "0123456789ABCDEF" "1CF.0") ; => 463