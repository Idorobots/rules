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
                                  (define (unfold v)
                                    (cond ((variable? v) (let ((a (assoc v bindings)))
                                                           (if a
                                                               (unfold (cdr a))
                                                               v))) ;; NOTE Most specific type is a free variable.
                                          ((list? v) (map unfold v))
                                          ((pair? v) (cons (unfold (car v))
                                                           (unfold (cdr v))))
                                          ('else v))) ;; NOTE Most specific type is a concrete value.
                                  (assign! store
                                           (cons (map unfold 'variables)
                                                 (deref store)))))))
       (map-facts (lambda (fact)
                    (assert-fact! rule fact)))
       (deref store)))))
