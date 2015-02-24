;; Various utilities.

(define (id x) x)

(define (array size)
  (make-vector size '()))

(define (array-ref array index)
  (vector-ref array index))

(define (array-assign! array index value)
  (vector-set! array index value))

(define (slice array start end)
  (reverse (let loop ((index start)
                      (acc '()))
             (if (= index end)
                 acc
                 (loop (+ 1 index) (cons (array-ref array index) acc))))))

(define (partition array size start mid end)
  (let ((pre (if (< start mid)
                 (slice array start mid)
                 (append (slice array start size)
                         (slice array 0 mid))))
        (past (if (< (+ 1 mid) end)
                  (slice array (+ 1 mid) end)
                  (append (slice array (+ mid 1) size)
                          (slice array 0 end)))))
    (list pre (array-ref array mid) past)))

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

(define true? (partial equal? #t))

(define (tagged-list? tag list)
  (and (list? list) (equal? (list-ref list 0) tag)))

(define (any? pred lst)
  (if (empty? lst)
      #f
      (or (pred (car lst))
          (any? pred (cdr lst)))))

(define (wrap value max)
  (cond ((< value 0) (wrap (+ max value) max))
        ((>= value max) (wrap (- value max) max))
        ('else value)))
