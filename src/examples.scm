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

(whenever (filter (let ((?gps (constantly 'gps)))
                    (and (tolerance ?m1 ?gps ?t1)
                         (tolerance ?m2 ?gps ?t2)))
                  (<= ?t1 ?t2)
                  (not-equal? ?m1 ?m2))
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
(display (select (?f) ?f)) ;; All facts.

(display (select (?m ?f)  ;; All module-function pairs.
                 (and (module ?m)
                      (provides ?m ?f))))

;; Trigger nodes for extended analysis of incomming events:
(reset!)

(whenever (trigger (?t 7)
                   (temperature ?t)
                   (>= ?t 25))
          (?t) =>
          (display "Captured temp: ")
          (display ?t)
          (newline))

(signal! (temperature 20))
(signal! (temperature 21))
(signal! (temperature 23))
(signal! (temperature 24))
(signal! (temperature 25)) ;; Triggered!
(signal! (temperature 26))
(signal! (temperature 24))
(signal! (temperature 23)) ;; Captured some events!
(signal! (temperature 21))
(signal! (temperature 20))

;; Relational log minning!
(reset!)

(assert! ((id . 0) (level . info)  (msg . "Checking core...")))
(assert! ((id . 0) (level . debug) (msg . "Core #23")))
(assert! ((id . 0) (level . info)  (msg . "Core ok.")))
(assert! ((id . 1) (level . info)  (msg . "Checking core...")))
(assert! ((id . 1) (level . error) (msg . "Core melting!")))
(assert! ((id . 1) (level . debug) (msg . "Core #13")))
(assert! ((id . 2) (level . info)  (msg . "Checking core...")))
(assert! ((id . 2) (level . debug) (msg . "Core #25")))
(assert! ((id . 2) (level . info)  (msg . "Core ok.")))
(assert! ((id . 3) (level . info)  (msg . "Checking core...")))
(assert! ((id . 3) (level . debug) (msg . "Core #5")))
(assert! ((id . 3) (level . info)  (msg . "Core ok.")))

;; Get all debug logs.
(display (map car
              (select (?log)
                      (filter ?log
                              ((lambda (log)
                                 (equal? (cdr (assoc 'level log))
                                         'debug))
                               ?log)))))

(display (filter (lambda (log)
                   (equal? (cdr (assoc 'level log))
                           'debug))
                 (map car (select (?log) ?log))))

;; Get all logs related to a critical failure.
(define (combine-logs acc id attrs)
  (if (member `((id ,id) ,@attrs) acc)
      acc
      (cons `((id ,id) ,@attrs) acc)))

(display (caar (select (?logs)
                       (reduce ?logs
                               (combine-logs () ?id ?attrs)
                               (and ((id . ?id) (level . error) . ?rest)
                                    ((id . ?id) . ?attrs))))))

;; Typesystems anybody?
(reset!)

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(assert! (: = (-> (?any ?any) bool)))
(assert! (: - (-> (int int) int)))
(assert! (: * (-> (int int) int)))
(assert! (: if (-> (bool ?t ?t) ?t)))

;; (: fact (-> ?arg ?ret))
(display (car (select (?arg ?ret)
                      (and (: = (-> (?arg int) ?eq))            ;; (= n 0)
                           (: - (-> (?arg int) ?arg))           ;; (- n 1) - passed as an argument to fact.
                           (: * (-> (?arg ?ret) ?mult))         ;; (* n fact-result)
                           (: if (-> (?eq int ?mult) ?ret)))))) ;; (if (= n 0) 1 (* n ...)) - returned as the result of fact.

;; Generic type inference:
(define (my-map f l)
  (if (null? l)
      null
      (cons (f (car l))
            (map f (cdr l)))))

(assert! (: null? (-> ((list ?a)) bool)))
(assert! (: car (-> ((list ?b)) ?b)))
(assert! (: cdr (-> ((list ?c)) (list ?c))))
(assert! (: cons (-> (?d (list ?d)) (list ?d))))
(assert! (: null (list ?e)))

;; (: my-map (-> ((-> ?fa ?fb) ?l) ?ret))
(display (car (select (?fa ?fb ?l ?ret)
                      (and (: null? (-> (?l) ?null))
                           (: null ?empty)
                           (: car (-> (?l) ?fa))
                           (: cdr (-> (?l) ?l))
                           (: cons (-> (?fb ?ret) ?cons))
                           (: if (-> (?null ?empty ?cons) ?ret))))))
