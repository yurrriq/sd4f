(define arity-table (make-key-weak-eqv-hash-table))

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc))) ;arity not in table
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

(define (compose f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (assert (= 1 n))
    (define (the-composition . args)
      (assert (= m (length args)))
      (f (apply g args)))
    (restrict-arity the-composition m)))

(let ((composition
       (compose (lambda (x) (list 'foo x))
                (lambda (x) (list 'bar x)))))
  (assert (= 1 (get-arity composition)))
  (assert (equal? (composition 'z)
                  '(foo (bar z)))))

(define (parallel-combine h f g)
  ;; (assert (= 2 (get-arity h)))
  (let ((n (get-arity f)) (m (get-arity g)))
    (assert (= n m))
    (define (the-combination . args)
      (assert (= n (length args)))
      (h (apply f args) (apply g args)))
    (restrict-arity the-combination m)))

(let ((combination
       (parallel-combine list
                         (lambda (x y z) (list 'foo x y z))
                         (lambda (u v w) (list 'bar u v w)))))
  (assert (= 3 (get-arity combination)))
  (assert (equal? (combination 'a 'b 'c)
                  '((foo a b c) (bar a b c)))))

(exit)
