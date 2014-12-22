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
