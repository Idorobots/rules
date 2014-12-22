;; Utils:
(define (atom? thing)
  (or (symbol? thing)
     (number? thing)))

(define (variable? pattern)
  (and (symbol? pattern) (starts-with? #\? pattern)))

(define (starts-with? character symbol)
  (equal? character (car (string->list (symbol->string symbol)))))

;; State:
(define *fact-store* null)
(define *rules* null)
(define *rete* null)

;; Fact store handling:
(define (add-facts! facts)
  (set! *fact-store* (append *fact-store* facts)))

(define (remove-facts! facts)
  (set! *fact-store*
        (filter (lambda (fact)
                  (not (member fact facts)))
                *fact-store*)))

;; Rule handling:
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

;; Rete network handling:
(define (extend-network! rule)
  (let ((network (compile-rule rule)))
    (set! *rete*
          (merge-networks *rete* network))))

(define (merge-networks original new)
  ;; TODO Actually merge the networks...
  (if (null? original)
      new
      (lambda (fact)
        (append (original fact)
                (new fact)))))

(define (compile-rule rule)
  (let ((nodes (compile-pattern (rule-pattern rule)
                                (rule-body rule))))
    (lambda (fact)
      (map (lambda (node)
             ;; NOTE Rete Network always starts with N Node1's.
             (node fact))
           nodes))))

(define (compile-pattern pattern next-node)
  (cond ((conjunction? pattern) (let ((nn (node-2 next-node)))
                                  ;; FIXME Allow more than two clauses here...
                                  (append (compile-pattern (conjunction-a pattern)
                                                           (car nn))
                                          (compile-pattern (conjunction-b pattern)
                                                           (cadr nn)))))
        ;; TODO Actually implement disjunction and negation nodes...
        ((disjunction? pattern) null)
        ((negation? pattern) null)
        ('else (list (node-1 pattern next-node)))))

(define (disjunction? pattern)
  (and (pair? pattern) (equal? (car pattern) 'or)))

(define (conjunction? pattern)
  (and (pair? pattern) (equal? (car pattern) 'and)))

(define (conjunction-a pattern)
  (cadr pattern))

(define (conjunction-b pattern)
  (caddr pattern))

(define (negation? pattern)
  (and (pair? pattern) (equal? (car pattern) 'not)))

;; Rete node types:
(define (node-1 pattern next-node)
  (lambda (fact)
    (let ((binding (match pattern fact)))
      (unless (null? binding)
          (next-node binding)))))

(define (node-2 next-node)
  (let ((l-mem null) ;; FIXME Should make this persistent.
        (r-mem null)
        (try-unify (lambda (l r)
                     (map (lambda (binding)
                            (unless (null? binding)
                              (next-node binding)))
                          (coherent? l r)))))
    (list (lambda (bindings)
            (unless (member bindings l-mem)
              (set! l-mem (cons bindings l-mem))
              (try-unify (list bindings) r-mem)))
          (lambda (bindings)
            (unless (member bindings r-mem)
              (set! r-mem (cons bindings r-mem))
              (try-unify l-mem (list bindings)))))))

(define (coherent? as bs)
  ;; FIXME O(K * L) * O(merge(K, L)), with K, L <= N.
  (map (lambda (a)
         (filter (lambda (b)
                   (merge a b))
                 bs))
       as))

(define (merge as bs)
  ;; NOTE O(max(len(as), len(bs)))
  (let loop ((as (sort as binding<?))
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
          ('else #f))))

(define (binding<? a b)
  (string<? (symbol->string (car a))
            (symbol->string (car b))))

(define (match pattern value)
  (cond ((variable? pattern) (list (cons pattern value)))
        ((list? pattern) (let ((bindings (map match pattern value)))
                           (if (memf null? bindings)
                               null
                               (apply append
                                      (filter pair?
                                              bindings)))))
        ((equal? pattern value) #t) ;; NOTE Indicates that value matches pattern but doesn't bind anything.
        ('else null)))

;; Syntax for convenience:
(define-syntax assert!
  (syntax-rules ()
    ((assert! fact ...)
     (let ((facts (list (quote fact) ...)))
       (add-facts! facts)
       (map *rete* facts)))))

(define-syntax retract!
  (syntax-rules ()
    ((retract! fact ...)
     (let ((facts (list (quote fact) ...)))
       (remove-facts! facts)
       ;; TODO Handle fact retraction.
       ))))

(define-syntax whenever
  (syntax-rules ()
    ((whenever pattern action ...)
     (let ((rule (make-rule (quote pattern)
                            (lambda bindings
                              ;; FIXME actually make the bindings usable.
                              action ...))))
       (add-rule! rule)
       (extend-network! rule)))))

;; TODO Add a way to remove rules from the network.

;; Exmaple usage:
(whenever (and (a ?x module) (provides ?x gps))
          (display "new gps!\n"))

(assert! (a A module))
(assert! (provides A foo))
(assert! (provides A bar))

(assert! (a B module))
(assert! (provides B foo))
(assert! (provides B gps))

(assert! (provides C gps))
(assert! (a C module))
