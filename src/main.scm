(load "rete.scm")

(reset!)

(whenever (provides ?x foo)
          () => (display "New foo!\n"))

(whenever (and (a ?x module)
               (provides ?x gps))
          () => (display "New GPS!\n"))

(whenever (reduce (min ?t 0.1)
                  (and (a ?x module)
                       (provides ?x gps)
                       (tolerance ?x gps ?t)))
          (?t) =>
          (display "Better GPS: ")
          (display ?t)
          (display "!\n"))

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

(assert! (tolerance B gps 0.01))
(assert! (tolerance C gps 0.001))
