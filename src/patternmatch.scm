;; Pattern matching related stuff.

(load "utils.scm")

;; FIXME Needs better unification.
(define (unify pattern value)
  (cond ((and (variable? pattern) (variable? value)) (list (cons pattern value)))
        ((variable? pattern) (list (cons pattern value)))
        ((variable? value) (list (cons value pattern)))
        ((and (pair? pattern) (pair? value))
         (let ((first (unify (car pattern) (car value)))
               (rest (unify (cdr pattern) (cdr value))))
           (cond ((null? first) null)
                 ((null? rest) null)
                 ((true? first) rest)
                 ((true? rest) first)
                 ('else (append first rest)))))
        ((equal? pattern value) #t) ;; NOTE Indicates that value matches pattern but doesn't bind anything.
        ('else null)))

(define (consistent? b bindings)
  (let ((a (assoc (car b) bindings)))
    (cond ((equal? a b) (list b))
          ((false? a) (list b))
          ('else (let ((bs (cons b bindings))
                       (us (unify (cdr a) (cdr b))))
                   (cond ((null? us) null)
                         ((true? us) (list b))
                         ('else (apply append
                                       (map (lambda (u)
                                              (consistent? u bs))
                                            us)))))))))

(define (merge as bs)
  (cond ((null? as) bs)
        ((null? bs) as)
        ('else (let ((c (consistent? (car bs) as)))
                 (if (null? c)
                     #f
                     (merge (append c as)
                            (cdr bs)))))))
