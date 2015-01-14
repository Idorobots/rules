;; Rule compiler.

(load "nodes.scm")

(define (compile-rule pattern action)
  (root-node (compile-pattern pattern (node-a action))))

(define (compile-pattern pattern next-node)
  (match pattern
    (`(and . ,_)
     (compile-conjunction pattern next-node))

    (`(reduce (,fun ,var ,acc) ,pattern)
     ;; FIXME Don't use eval.
     (compile-pattern pattern (node-r (eval fun) var acc next-node)))

    ;; FIXME Don't use eval.
    (`(filter (,fun . ,vars) ,pattern)
     (compile-pattern pattern (node-p (eval fun) vars next-node)))

    (_ (list (node-1 pattern next-node)))))

(define (compile-conjunction conj next-node)
  ((foldl (lambda (pattern build-prev)
            (lambda (nn)
              (let ((n2 (node-2 nn)))
                (append (build-prev (node-2l n2))
                        (compile-pattern pattern n2)))))
          (lambda (nn)
            (compile-pattern (conjunction-first conj) nn))
          (conjunction-rest conj))
   next-node))

(define (conjunction-first pattern)
  (cadr pattern))

(define (conjunction-rest pattern)
  (cddr pattern))
