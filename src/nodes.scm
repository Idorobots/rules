;; Rete nodes live here.
(load "utils.scm")

(define (node type next . data)
  (list* type next data))

(define (type node)
  (list-ref node 0))

(define (next-nodes node)
  (list-ref node 1))

(define (data node n)
  (list-ref node (+ 2 n)))

;; Nodes

(define (root-node nodes)
  (node 'root-node (ref nodes)))

(define (node-a action)
  (node 'node-a
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
        node-2))

(define (node-r fun var acc next-node)
  (node 'node-r
        (ref (list next-node))
        (lambda (fact acc)
          (let ((val (assoc var fact)))
            (when val
              (fun (cdr val) acc))))
        var
        (ref acc)))

(define (node-p fun vars next-node)
  (node 'node-p
        (ref (list next-node))
        (lambda (fact)
          (apply fun
                 (map (lambda (v)
                        (let ((b (assoc v fact)))
                          (if (pair? b)
                              (cdr b)
                              null)))
                      vars)))))
