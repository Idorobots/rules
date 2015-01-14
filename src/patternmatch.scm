;; Pattern matching related stuff.

(load "utils.scm")

(define (unify pattern value)
  (cond ((variable? pattern) (list (cons pattern value)))
        ((and (list? pattern)
              (list? value)
              (equal? (length pattern)
                      (length value)))
         (let ((bindings (map unify pattern value)))
           (if (memf null? bindings)
               null
               (apply append
                      (filter pair?
                              bindings)))))
        ((equal? pattern value) #t) ;; NOTE Indicates that value matches pattern but doesn't bind anything.
        ('else null)))

(define (binding<? a b)
  (string<? (symbol->string (car a))
            (symbol->string (car b))))

(define (merge as bs)
  ;; NOTE O(max(len(as), len(bs)))
  (cond ((null? as) bs)
        ((null? bs) as)
        ('else (let loop ((as (sort as binding<?))
                          (bs (sort bs binding<?))
                          (acc null)
                          (intersect? #f))
                 (cond ((or (null? as) (null? bs)) (if intersect?
                                                       (append acc as bs)
                                                       #f))
                       ((binding<? (car as) (car bs)) (loop (cdr as)
                                                            bs
                                                            (cons (car as) acc)
                                                            intersect?))
                       ((binding<? (car bs) (car as)) (loop as
                                                            (cdr bs)
                                                            (cons (car bs) acc)
                                                            intersect?))
                       ((equal? (car as) (car bs)) (loop (cdr as)
                                                         (cdr bs)
                                                         (cons (car as) acc)
                                                         #t))
                       ('else #f))))))
