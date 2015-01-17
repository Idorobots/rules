;; Rete-based & naïve implementation benchmark.

(define (benchmark seed)
  (let loop ((modules '(benchmark
                        compiler
                        examples
                        naive
                        nodes
                        patternmatch
                        rete
                        utils))
             (done null))
    (random-seed seed)
    (if (empty? modules)
        null
        (let* ((to-infere (cons (car modules) done))
               (facts (infere-all (reverse to-infere))))
          (cons (list (cons 'facts (length facts))
                      (cons 'naive (test-naive facts))
                      (cons 'rete (test-rete facts)))
                (loop (cdr modules) to-infere))))))

(define (test-naive facts)
  (load "naive.scm")
  (test (eval (bench-function facts))))

(define (test-rete facts)
  (load "rete.scm")
  (test (eval (bench-function facts))))

(define (test bench-fun)
  ;; NOTE Inject mutation counters...
  (let ((old-ref ref)
        (old-assign! assign!)
        (old-deref deref)
        (refs 0)
        (assignments 0)
        (derefs 0))
    (set! ref (lambda args
                (set! refs (+ 1 refs))
                (apply old-ref args)))
    (set! deref (lambda args
                  (set! derefs (+ 1 derefs))
                  (apply old-deref args)))
    (set! assign! (lambda args
                    (set! assignments (+ 1 assignments))
                    (apply old-assign! args)))
    (collect-garbage)
    (let-values (((start) (current-memory-use))
                 ((_ cpu real gc) (time-apply bench-fun null))
                 ((stop) (current-memory-use)))
      (list (cons 'cpu-time cpu)
            (cons 'real-time real)
            (cons 'gc-time gc)
            (cons 'memory-used (- stop start))
            (cons 'refs refs)
            (cons 'derefs derefs)
            (cons 'assignments assignments)))))

;; Fact inference for the benchmark generator:

(define (infere-all modules)
  (let ((facts (apply append (map infere-module modules))))
    (map (lambda (f)
           (let ((th (random 100)))
             (cond ((< th 40) `(signal! ,f))
                   ((< th 75) `(assert! ,f))
                   ('else `(retract! ,f)))))
         facts)))

(define (infere-module name)
  (with-input-from-file (string-append (symbol->string name) ".scm")
    (lambda ()
      (cons `(module ,name)
            (let loop ()
              (let ((form (read)))
                (if (eof-object? form)
                    null
                    (append (infere name form)
                            (loop)))))))))

(define (infere name form)
  (match form
    (`(define (,function . ,args) . ,body)
     (list* `(provides ,name ,function)
            (if (list? args)
                `(arity ,name ,function ,(length args))
                `(arity ,name ,function variable))
            (map (lambda (arg)
                   `(argument ,name ,function ,arg))
                 (->list args))))
    (`(define ,variable . ,value)
     (list `(defines ,name ,variable)))
    (otherwise null)))

(define (->list improper-list)
  (cond ((pair? improper-list) (cons (car improper-list)
                                     (->list (cdr improper-list))))
        ((null? improper-list) null)
        ('else (list improper-list))))

;; The benchmark function generator:

(define (bench-function facts)
  `(lambda ()
     (reset!)
     ;; All providers of add-rule!
     (whenever (provides ?m add-rule!)
               () => null)
     ;; All modules that provide add-rule!
     (whenever (and (module ?m)
                    (provides ?m add-rule!))
               () => null)
     ;; All modules that provide a 2-ary function.
     (whenever (and (module ?m)
                    (provides ?m ?f)
                    (arity ?m ?f 2))
               () => null)
     ;; All function pairs in a module that have the same arity.
     (whenever (and (module ?m)
                    (provides ?m ?f1)
                    (provides ?m ?f2)
                    (arity ?m ?f1 ?a)
                    (arity ?m ?f2 ?a))
               () => null)
     ;; All functions pairs in module pairs that have the same arity.
     (whenever (and (module ?m1)
                    (module ?m2)
                    (provides ?m1 ?f)
                    (provides ?m2 ?f)
                    (arity ?m1 ?f ?a)
                    (arity ?m2 ?f ?a))
               () => null)
     ,@facts))
