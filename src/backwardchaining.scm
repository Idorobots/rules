;; An implementation of backward chaining on top of Rete networks.

(load "utils.scm")
(load "rete.scm")
(load "compiler.scm")

(define-syntax select
  (syntax-rules ()
    ((select variables pattern)
     (let* ((store (ref null))
            (rule (compile-rule 'pattern
                                (lambda (bindings)
                                  (define (get v)
                                    (let ((b (assoc v bindings)))
                                      (cond ((false? b) #f)
                                            ((variable? (cdr b)) (or (get (cdr b))
                                                                     (cdr b)))
                                            ('else (cdr b)))))
                                  (assign! store
                                           (cons (map get 'variables)
                                                 (deref store)))))))
       (map-facts (lambda (fact)
                    (assert-fact! rule fact)))
       (deref store)))))
