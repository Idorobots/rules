;; Rete implementation.

(load "utils.scm")
(load "nodes.scm")
(load "patternmatch.scm")
(load "compiler.scm")

;; State:

(define *rete* (root-node null))

;; Utils:

(define (reset!)
  (set! *rete* (root-node null)))

(define (call-next f nodes value)
  (for-each (lambda (n) (f n value)) nodes))

(define (unify-call binding memory f)
  (for-each f
            (filter not-false?
                    (map (partial merge binding)
                         memory))))

;; Rete actions:

(define (assert-fact! node fact)
  (match node
    (`(root-node ,next)
     (call-next assert-fact! (deref next) fact))

    (`(node-a ,action)
     (action fact))

    (`(node-1 ,next ,pattern ,memory)
     (unless (member fact (deref memory))
       (let ((bindings (unify pattern fact)))
         (unless (null? bindings)
           (assign! memory (cons fact (deref memory)))
           (call-next assert-fact! (deref next) bindings)))))

    (`(node-r ,next ,fun ,var ,acc)
     (let ((val (assoc var fact)))
       (when val
         (let ((r (fun (cdr val) (deref acc))))
           (unless (equal? r (deref acc))
             (assign! acc r)
             (call-next assert-fact! (deref next) (cons var r)))))))

    (`(node-2 ,next ,l-mem ,r-mem)
     (assert-fact-node2! next fact r-mem l-mem))

    (`(node-2l (node-2 ,next ,l-mem ,r-mem))
     (assert-fact-node2! next fact l-mem r-mem))))

(define (assert-fact-node2! nodes fact this-mem other-mem)
  (unless (member fact (deref this-mem))
    (assign! this-mem (cons fact (deref this-mem)))
    (unify-call fact
                (deref other-mem)
                (partial call-next
                         assert-fact!
                         (deref nodes)))))

(define (retract-fact! node fact)
  (match node
    (`(root-node ,next)
     (call-next retract-fact! (deref next) fact))

    (`(node-1 ,next ,pattern ,memory)
     (when (member fact (deref memory))
       (let ((bindings (unify pattern fact)))
         (unless (null? bindings)
           (call-next retract-fact! (deref next) bindings)))
       (assign! memory
                (filter (partial not-equal? fact)
                        (deref memory)))))

    (`(node-r ,next ,fun ,var ,acc)
     (let ((val (assoc var fact)))
       (when val
         (let ((r (fun (cdr val) (deref acc))))
           (unless (equal? r (deref acc))
             ;; NOTE No need to retract anything but we still need to retract next node.
             (call-next retract-fact! (deref next) (cons var r)))))))

    (`(node-2 ,next ,l-mem ,r-mem)
     (retract-fact-node2! next fact r-mem l-mem))

    (`(node-2l (node-2 ,next ,l-mem ,r-mem))
     (retract-fact-node2! next fact l-mem r-mem))

    (_ null)))

(define (retract-fact-node2! nodes fact this-mem other-mem)
  (when (member fact (deref this-mem))
    (unify-call fact
                (deref other-mem)
                (partial call-next
                         retract-fact!
                         (deref nodes)))
    ;; FIXME Since two different rules can introduce the same bindings
    ;; FIXME a global renaming scheme should be used or original facts
    ;; FIXME should be stored.
    (assign! this-mem
             (filter (partial not-equal? fact)
                     (deref this-mem)))))

(define (signal-fact! node fact)
  (match node
    (`(root-node ,next)
     (call-next signal-fact! (deref next) fact))

    (`(node-a ,action)
     (action fact))

    (`(node-1 ,next ,pattern ,memory)
     (unless (member fact (deref memory))
       (let ((bindings (unify pattern fact)))
         (unless (null? bindings)
           (call-next signal-fact! (deref next) bindings)))))

    (`(node-r ,next ,fun ,var ,acc)
     (let ((val (assoc var fact)))
       (when val
         (let ((r (fun (cdr val) (deref acc))))
           (unless (equal? r (deref acc))
             ;; NOTE We need to store the new acc anyway.
             (assign! acc r)
             (call-next signal-fact! (deref next) (cons var r)))))))

    (`(node-2 ,next ,l-mem ,r-mem)
     (signal-fact-node2! next fact r-mem l-mem))

    (`(node-2l (node-2 ,next ,l-mem ,r-mem))
     (signal-fact-node2! next fact l-mem r-mem))))

(define (signal-fact-node2! nodes fact this-mem other-mem)
  (unless (member fact (deref this-mem))
    (unify-call fact
                (deref other-mem)
                (partial call-next
                         signal-fact!
                         (deref nodes)))))

;; Network merging & optimization:

(define (merge-networks node-a node-b)
  ;; FIXME Actually implement network merging...
  (root-node (append (deref (next-nodes node-a))
                     (deref (next-nodes node-b)))))

;; Syntax for convenience:
(define-syntax assert!
  (syntax-rules ()
    ((assert! fact)
     (assert-fact! *rete* 'fact))))

(define-syntax signal!
  (syntax-rules ()
    ((signal! fact)
     (signal-fact! *rete* 'fact))))

(define-syntax retract!
  (syntax-rules ()
    ((retract! fact)
     (retract-fact! *rete* 'fact))))

(define-syntax whenever
  (syntax-rules (=>)
    ((whenever pattern vars => action ...)
     (set! *rete*
           (merge-networks *rete*
                           (compile-rule 'pattern
                                         (lambda bindings
                                           (apply (lambda vars action ...)
                                                  (map (lambda (v)
                                                         (let ((val (assoc v bindings)))
                                                           (when val
                                                             (cdr val))))
                                                       'vars)))))))))

