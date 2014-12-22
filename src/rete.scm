(define *fact-store* '())

(define (add-facts! facts)
  (set! *fact-store* (append *fact-store* facts)))

(define-syntax assert!
  (syntax-rules ()
    ((assert! fact ...)
     (add-facts! (list (quote fact) ...)))))

(define (remove-facts! facts)
  (set! *fact-store*
        (filter (lambda (fact)
                  (not (member fact facts)))
                *fact-store*)))

(define-syntax retract!
  (syntax-rules ()
    ((retract! fact ...)
     (remove-facts! (list (quote fact) ...)))))

(define *rules* '())

(define (make-rule pattern body)
  (list pattern body))

(define (rule-pattern rule)
  (car rule))

(define (rule-body rule)
  (cadr rule))

(define (add-rule! rule)
  (set! *rules* (cons rule *rules*)))

(define (remove-rule! rule)
  (set! *rules*
        (filter (lambda (r)
                  (not (equal? (car r) (car rule))))
                *rules*)))
