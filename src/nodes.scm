;; Rete nodes live here.

(define (ref x)
  (make-vector 1 x))

(define (deref ref)
  (vector-ref ref 0))

(define (assign! ref value)
  (vector-set! ref 0 value))

(define (dummy . args)
  (display "args: ")
  (display args)
  (newline))

(define (id x) x)

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
(define not-false? (complimentary false?))

(define (partial f . args)
  (lambda rest
    (apply f (append args rest))))

(define (tagged-list? tag list)
  (and (list? list) (equal? (list-ref list 0) tag)))

(define (fact? pattern)
  (tagged-list? 'fact pattern))

;; (node-type next-nodes node-data ...)

(define (node type next . data)
  (list* type next data))

(define (type node)
  (list-ref node 0))

(define (next node)
  (list-ref node 1))

(define (data node n)
  (list-ref node (+ 2 n)))

;; Actual nodes:

(define (root-node nodes)
  (node 'root-node (ref nodes)))

(define node-a? (partial tagged-list? 'node-a))

(define (node-a action)
  (node 'node-a
        null
        action))

(define (node-1 pattern next-node)
  (node 'node-1
        (ref (list next-node))
        pattern
        (ref null)))

(define (node-2 next-node)
  (node 'node-2
        (ref (list next-node))
        (ref null)
        (ref null)))

(define (node-2l node-2)
  (node 'node-2l
        null
        node-2))

;; Pattern matching & utilities:

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

;; Rete actions:

(define (call-next f nodes value)
  (for-each (lambda (n) (f n value)) nodes))

(define (unify-call binding memory f)
  (for-each f
            (filter not-false?
                    (map (partial merge binding)
                         memory))))

(define (assert-node2 nodes fact this-mem other-mem)
  (unless (member fact (deref this-mem))
    (assign! this-mem (cons fact (deref this-mem)))
    (unify-call fact
                (deref other-mem)
                (partial call-next
                         assert
                         (deref nodes)))))

(define (assert node fact)
  (case (type node)
    ('root-node (call-next assert
                           (deref (next node))
                           fact))
    ('node-a ((data node 0) fact))
    ('node-1 (let ((pattern (data node 0))
                   (memory (data node 1)))
               (unless (member fact (deref memory))
                 (let ((bindings (unify pattern fact)))
                   (unless (null? bindings)
                     (assign! memory (cons fact (deref memory)))
                     (call-next assert
                                (deref (next node))
                                bindings))))))
    ('node-2 (assert-node2 (next node)
                           fact
                           (data node 1)
                           (data node 0)))
    ('node-2l (let ((inner (data node 0)))
                (assert-node2 (next inner)
                              fact
                              (data inner 0)
                              (data inner 1))))))

(define (retract-node2 nodes fact this-mem other-mem)
  (when (member fact (deref this-mem))
    (unify-call fact
                (deref other-mem)
                (partial call-next
                         retract
                         (deref nodes)))
    ;; FIXME Since two different rules can introduce the same bindings
    ;; FIXME a global renaming scheme should be used or original facts
    ;; FIXME should be stored.
    (assign! this-mem
             (filter (partial not-equal? fact)
                     (deref this-mem)))))

(define (retract node fact)
  (case (type node)
    ('root-node (call-next retract
                           (deref (next node))
                           fact))
    ;; FIXME Deduplicate
    ('node-1 (let ((pattern (data node 0))
                   (memory (data node 1)))
               (when (member fact (deref memory))
                 (let ((bindings (unify pattern fact)))
                   (unless (null? bindings)
                     (call-next retract
                                (deref (next node))
                                bindings)))
                 (assign! memory
                          (filter (partial not-equal? fact)
                                  (deref memory))))))
    ('node-2 (retract-node2 (next node)
                            fact
                            (data node 1)
                            (data node 0)))
    ('node-2l (let ((inner (data node 0)))
                (retract-node2 (next inner)
                               fact
                               (data inner 0)
                               (data inner 1))))))

(define (signal-node2 nodes fact this-mem other-mem)
  (unless (member fact (deref this-mem))
    (unify-call fact
                (deref other-mem)
                (partial call-next
                         signal
                         (deref nodes)))))

(define (signal node fact)
  ;; (assert node fact)
  ;; (retract node fact)
  (case (type node)
    ('root-node (call-next signal
                           (deref (next node))
                           fact))
    ('node-a ((data node 0) fact))
    ('node-1 (unless (member fact (deref (data node 1)))
               (let ((bindings (unify (data node 0) fact)))
                 (unless (null? bindings)
                   (call-next signal
                              (deref (next node))
                              bindings)))))
    ('node-2 (signal-node2 (next node)
                           fact
                           (data node 1)
                           (data node 0)))
    ('node-2l (let ((inner (data node 0)))
                (signal-node2 (next inner)
                              fact
                              (data inner 0)
                              (data inner 1))))))

;; Rule compilation

(define (conjunction? pattern)
  (and (pair? pattern) (equal? (car pattern) 'and)))

(define (conjunction-a pattern)
  (cadr pattern))

(define (conjunction-b pattern)
  (caddr pattern))

(define (compile-pattern pattern next-node)
  (cond ((conjunction? pattern) (let* ((n2 (node-2 next-node))
                                       (n2l (node-2l n2)))
                                  (append (compile-pattern (conjunction-a pattern)
                                                           n2l)
                                          (compile-pattern (conjunction-b pattern)
                                                           n2))))
        ('else (list (node-1 pattern next-node)))))

(define (compile pattern action)
  (root-node (compile-pattern pattern (node-a action))))

;; Samples

(define (dummy-network)
  (compile '(and (module ?x) (provides ?x ?y))
           dummy))
