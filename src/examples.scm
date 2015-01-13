(load "rete.scm")

(reset!)

(define new-foo
  (whenever (provides ?m foo)
            () => (display "New foo!\n")))

(whenever (and (module ?m)
               (provides ?m gps))
          () => (display "New GPS!\n"))

(whenever (reduce (min ?t 0.1)
                  (and (module ?m)
                       (provides ?m gps)
                       (tolerance ?m gps ?t)))
          (?t) =>
          (display "Better GPS: ")
          (display ?t)
          (display "!\n"))

(assert! (module A))
(assert! (provides A foo))
(assert! (provides A bar))

(assert! (module B))
(assert! (provides B foo))
(assert! (provides B gps))

(assert! (provides C gps))
(assert! (module C))

(signal! (provides A gps))

(retract! (module B))
(assert! (module B))

(assert! (tolerance C gps 0.01))
(assert! (tolerance B gps 0.001))
(assert! (tolerance A gps 0.0001))

(remove-rule! new-foo)
