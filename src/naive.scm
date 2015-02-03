;; Minimal, naïve Rule-Based System implementation.
;; Doesn't support reduction nor filtration nodes, nor complex patterns.

(load "utils.scm")
(load "patternmatch.scm")
(load "factstore.scm")

;; State:

(define *rules* (ref null))

(define (reset!)
  (reset-facts!)
  (assign! *rules* null))

;; RBS actions:

(define (run-rules rules facts)
  (for-each (lambda (rule)
              (run-rule (cadr rule) (caddr rule) facts))
            rules))

(define (run-rule pattern action facts)
  (for-each action
            (unify-all pattern facts)))

(define (unify-all pattern facts)
  (if (tagged-list? 'and pattern)
      (merge-all (map (lambda (subpattern)
                        (unify-all subpattern facts))
                      (cdr pattern)))
      (filter not-void?
              (map (lambda (fact)
                     (let ((bindings (unify pattern fact)))
                       (unless (null? bindings)
                         bindings)))
                   facts))))

(define (merge-all bindings)
  (if (null? bindings)
      (list null)
      (apply append
             (map (lambda (binding)
                    (filter not-false?
                            (map (partial merge binding)
                                 (merge-all (cdr bindings)))))
                  (car bindings)))))

;; Rule retraction:

(define (remove-rule! id)
  (assign! *rules*
           (filter (partial equal? id)
                   (deref *rules*))))

;; Syntax for convenience:

(define-syntax assert!
  (syntax-rules ()
    ((assert! fact)
     (unless (fact-asserted? 'fact)
       (add-fact! 'fact)
       (run-rules (deref *rules*)
                  (list-facts))))))

(define-syntax retract!
  (syntax-rules ()
    ((retract! fact)
     (remove-fact! 'fact))))

(define-syntax signal!
  (syntax-rules ()
    ((signal! fact)
     (run-rules (deref *rules*)
                (cons 'fact
                      (list-facts))))))

(define-syntax whenever
  (syntax-rules (=>)
    ((whenever pattern vars => action ...)
     (let ((id (gensym 'rule)))
       (assign! *rules*
                (cons (list id
                            'pattern
                            (let ((memory (ref null)))
                              (lambda (bindings)
                                (unless (member bindings (deref memory))
                                  (assign! memory (cons bindings (deref memory)))
                                  (apply (lambda vars action ...)
                                         (map (lambda (v)
                                                (let ((val (assoc v bindings)))
                                                  (when val
                                                    (cdr val))))
                                              'vars))))))
                      (deref *rules*)))
       id))))
