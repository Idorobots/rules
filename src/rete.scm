;; Utils:
(define (atom? thing)
  (or (symbol? thing) (number? thing)))

(define (variable? pattern)
  (and (symbol? pattern) (starts-with? #\? pattern)))

(define (starts-with? character symbol)
  (equal? character (car (string->list (symbol->string symbol)))))

(define (complimentary f)
  (lambda args (not (apply f args))))

(define not-member (complimentary member))

(define not-equal? (complimentary equal?))

;; State:
(define *fact-store* null)
(define *rules* null)
(define *rete* null)

(define (reset!)
  (set! *fact-store* null)
  (set! *rules* null)
  (set! *rete* null))

;; Fact store handling:
(define (add-facts! facts)
  (set! *fact-store* (append *fact-store* facts))
  (for-each (lambda (f) (*rete* 'assert f))
            facts))

(define (signal-facts! facts)
  (for-each (lambda (f) (*rete* 'signal f))
            facts))

(define (remove-facts! facts)
  (set! *fact-store*
        (filter (lambda (fact)
                  (not-member fact facts))
                *fact-store*))
  (for-each (lambda (f) (*rete* 'retract f))
            facts))

;; Rule handling:
(define (make-rule name pattern body)
  (list name pattern body (node-f name)))

(define (rule-name rule)
  (car rule))

(define (rule-pattern rule)
  (cadr rule))

(define (rule-body rule)
  (caddr rule))

(define (rule-node rule)
  (cadddr rule))

(define (add-rule! rule)
  (set! *rules* (cons rule *rules*))
  (extend-network! rule))

(define (remove-rule! rule)
  (set! *rules*
        (filter (lambda (r)
                  (not-equal? (rule-name rule)
                              (rule-name r)))
                *rules*)))

;; Rete network handling:
(define (extend-network! rule)
  (let ((network (compile-rule rule)))
    (set! *rete* (merge-networks *rete* network))))

(define (merge-networks original new)
  ;; TODO Actually merge the networks...
  (if (null? original)
      new
      (lambda (action fact)
        (original action fact)
        (new action fact))))

(define (compile-rule rule)
  (let ((nodes (compile-pattern (rule-pattern rule)
                                (rule-node rule))))
    (lambda (action fact)
      (map (lambda (node)
             ;; NOTE Rete Network always starts with N Node1's.
             (node action fact))
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
  (lambda (action fact)
    (let ((binding (unify pattern fact)))
      (unless (null? binding)
          (next-node action binding)))))

(define (node-2 next-node)
  (let ((l-mem null) ;; FIXME Should make this persistent.
        (r-mem null))
    (list (node-function next-node l-mem r-mem)
          (node-function next-node r-mem l-mem))))

(define-syntax node-function
  (syntax-rules ()
    ((node-function next-node my-memory other-memory)
     (lambda (action bindings)
       (let ((try-unify (lambda (bindings memory)
                          (map (lambda (b) (next-node action b))
                               (filter (lambda (b) (merge bindings b))
                                       memory)))))
         (cond ((equal? action 'signal) (unless (member bindings my-memory)
                                          (try-unify bindings other-memory)))
               ((equal? action 'assert) (unless (member bindings my-memory)
                                          (set! my-memory (cons bindings my-memory))
                                          (try-unify bindings other-memory)))
               ((equal? action 'retract) (when (member bindings my-memory)
                                           (try-unify bindings other-memory)
                                           ;; NOTE We need to remove bindings from the rest of the network.
                                           (set! my-memory (filter (lambda (b) (not-equal? bindings b))
                                                                   my-memory))))))))))

(define (node-f name)
  (lambda (action bindings)
    (let ((r (assoc name *rules*)))
      (when r
        (unless (equal? action 'retract)
          ((rule-body r) bindings))))))

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

(define (unify pattern value)
  (cond ((variable? pattern) (list (cons pattern value)))
        ((list? pattern) (let ((bindings (map unify pattern value)))
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
    ((assert! . facts)
     (add-facts! 'facts))))

(define-syntax signal!
  (syntax-rules ()
    ((signal! . facts)
     (signal-facts! 'facts))))

(define-syntax retract!
  (syntax-rules ()
    ((retract! . facts)
     (remove-facts! 'facts))))

(define-syntax whenever
  (syntax-rules ()
    ((whenever pattern action ...)
     (let ((r (make-rule (gensym 'rule)
                         'pattern
                         (lambda bindings
                           ;; FIXME actually make the bindings usable.
                           action ...))))
       (add-rule! r)
       r))))

;; TODO Add a way to remove rules from the network.

;; Exmaple usage:
(reset!)

(whenever (provides ?x foo)
          (display "new foo!\n"))

(define gps-appears
  (whenever (and (a ?x module) (provides ?x gps))
            (display "new gps!\n")))

(assert! (a A module))
(assert! (provides A foo))
(assert! (provides A bar))

(assert! (a B module))
(assert! (provides B foo))
(assert! (provides B gps))

(assert! (provides C gps))
(assert! (a C module))

(signal! (provides A gps))

(retract! (a B module))
(assert! (a B module))

(remove-rule! gps-appears)
