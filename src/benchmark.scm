;; Rete-based & naïve implementation benchmark.

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

(define (test-naive)
  (load "naive.scm")
  (test common-bench))

(define (test-rete)
  (load "rete.scm")
  (test common-bench))

(define (benchmark)
  (list (cons 'naive (test-naive))
        (cons 'rete (test-rete))))

;; The large benchmark function:

(define (common-bench)
  ;; TODO Actually implement a meaningful benchmark.
  (reset!)

  (define new-foo
    (whenever (provides ?m foo)
              () => null))

  (whenever (and (module ?m)
                 (provides ?m gps))
            () => null)

  (whenever (and (module ?m)
                 (provides ?m ?f)
                 (tolerange ?m ?f 0.01))
            () => null)

  (whenever (and (module ?m)
                 (provides ?m ?f1)
                 (provides ?m ?f2)
                 (tolerange ?m ?f ?t)
                 (tolerance ?m ?f ?t))
            () => null)

  (whenever (and (module ?m1)
                 (module ?m2)
                 (provides ?m1 ?f)
                 (provides ?m2 ?f)
                 (tolerange ?m1 ?f ?t)
                 (tolerance ?m2 ?f ?t))
            () => null)

  (assert! (module A))
  (assert! (provides A foo))
  (assert! (provides A bar))
  (assert! (provides A baz))
  (signal! (provides A faz))

  (assert! (module B))
  (assert! (provides B foo))
  (assert! (provides B gps))
  (signal! (provides B bar))
  (signal! (provides B baz))

  (assert! (provides C foo))
  (assert! (provides C bar))
  (assert! (provides C baz))
  (assert! (provides C faz))
  (assert! (provides C gps))
  (assert! (module C))

  (signal! (provides A gps))
  (retract! (module B))
  (assert! (module B))

  (assert! (tolerance C gps 0.01))
  (assert! (tolerance B gps 0.001))
  (assert! (tolerance A gps 0.0001))

  (assert! (tolerance C foo 0.01))
  (assert! (tolerance B foo 0.001))
  (assert! (tolerance A foo 0.0001))

  (assert! (tolerance C bar 0.01))
  (assert! (tolerance B bar 0.001))
  (assert! (tolerance A bar 0.0001))

  (remove-rule! new-foo)
  (assert! (provides C foo)))
