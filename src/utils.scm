;; Various utilities.

(define (ref x)
  (make-vector 1 x))

(define (deref ref)
  (vector-ref ref 0))

(define (assign! ref value)
  (vector-set! ref 0 value))

(define (variable? pattern)
  (and (symbol? pattern) (starts-with? #\? pattern)))

(define (starts-with? character symbol)
  (equal? character (car (string->list (symbol->string symbol)))))

(define (complimentary f)
  (lambda args (not (apply f args))))

(define not-member (complimentary member))
(define not-equal? (complimentary equal?))
(define not-false? (complimentary false?))
(define not-void? (complimentary void?))

(define (partial f . args)
  (lambda rest
    (apply f (append args rest))))

(define (tagged-list? tag list)
  (and (list? list) (equal? (list-ref list 0) tag)))

(define (any? pred lst)
  (if (empty? lst)
      #f
      (or (pred (car lst))
          (any? pred (cdr lst)))))
