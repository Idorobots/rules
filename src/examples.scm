;; This is an example usage of the Rete-based RBS.

(load "rete.scm")
(load "backwardchaining.scm")

(reset!)

(define new-foo
  (whenever (provides ?m foo)
            () => (display "New foo!\n")))

(whenever (and (module ?m)
               (provides ?m gps))
          () => (display "New GPS!\n"))

(whenever (reduce ?min-t
                  (min 0.1 ?t)
                  (and (module ?m)
                       (provides ?m gps)
                       (tolerance ?m gps ?t)))
          (?min-t) =>
          (display "Best GPS: ")
          (display ?min-t)
          (display "!\n"))

(whenever (filter (and (tolerance ?m1 ?gps ?t1)
                       (tolerance ?m2 ?gps ?t2))
                  (<= ?t1 ?t2)
                  (not-equal? ?m1 ?m2)
                  ((lambda (g) (equal? g 'gps)) ?gps))
          (?t1 ?t2) =>
          (display "Better GPS: ")
          (display ?t1)
          (display " vs. ")
          (display ?t2)
          (display "!\n"))

(assert! (module A))
(assert! (provides A foo)) ;; New foo!
(assert! (provides A bar))

(assert! (module B))
(assert! (provides B foo)) ;; New foo!
(assert! (provides B gps)) ;; New GPS!

(assert! (provides C gps))
(assert! (module C))       ;; New GPS!

;; Event signaling:
(signal! (provides A gps)) ;; New GPS!
(retract! (module B))
(assert! (module B))       ;; New GPS!

;; Reduction & filtration nodes:
(assert! (tolerance C gps 0.01))
(assert! (tolerance B gps 0.001))
(assert! (tolerance A gps 0.0001)) ;; A doesn't provide GPS.

;; Rule removal:
(remove-rule! new-foo)
(assert! (provides C foo))

;; Backward chaining:
(select (?f) ?f) ;; All facts.

(select (?m ?f)  ;; All module-function pairs.
        (and (module ?m)
             (provides ?m ?f)))

;; Relational log minning!
(reset!)

(define (assert-object! obj)
  (let ((id (gensym 'id)))
    (map (lambda (p)
           (eval `(assert! (,(car p) ,id ,(cdr p)))))
         obj)))

(assert-object! '((id . 0) (level . info)  (msg . "Checking core...")))
(assert-object! '((id . 0) (level . debug) (msg . "Core #23")))
(assert-object! '((id . 0) (level . info)  (msg . "Core ok.")))
(assert-object! '((id . 1) (level . info)  (msg . "Checking core...")))
(assert-object! '((id . 1) (level . error) (msg . "Core melting!")))
(assert-object! '((id . 1) (level . debug) (msg . "Core #13")))
(assert-object! '((id . 2) (level . info)  (msg . "Checking core...")))
(assert-object! '((id . 2) (level . debug) (msg . "Core #25")))
(assert-object! '((id . 2) (level . info)  (msg . "Core ok.")))
(assert-object! '((id . 3) (level . info)  (msg . "Checking core...")))
(assert-object! '((id . 3) (level . debug) (msg . "Core #5")))
(assert-object! '((id . 3) (level . info)  (msg . "Core ok.")))

(define (recombine-logs acc log attr val)
  (let ((l (assoc log acc)))
    (if l
        (cons (cons log
                    (list* (cons attr val)
                           (cdr l)))
              (filter (lambda (kv)
                        (not-equal? log (car kv)))
                      acc))
        (cons (cons log (list (cons attr val)))
              acc))))

;; Get all logs related to a critical failure.
(map cdr
     (cdaar (select (?errors)
                    (reduce ?errors
                            (recombine-logs () ?log2 ?attr ?val)
                            (and (id ?log1 ?id)
                                 (level ?log1 error)
                                 (msg ?log1 ?msg1)
                                 (id ?log2 ?id)
                                 (?attr ?log2 ?val))))))
