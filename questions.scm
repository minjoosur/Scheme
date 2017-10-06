(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items) nil (cons (proc (car items)) (map proc (cdr items))))
)

(define (cons-all first rests)
  (if (null? rests) nil (cons (cons first (car rests)) (cons-all first (cdr rests))))
)

(define (zip pairs)
  (if (null? (car pairs)) nil (cons (map car pairs) (zip (map cdr pairs))))
)

(define a (list (list 1 2 3) (list 2 4 6)))
;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
    (define (make x)
        (if (= x (length s)) 
          nil 
          (cons x (make (+ x 1)))))
  (zip (list (make 0) s))
  )

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS

(define (list-change total denoms)
  (cond
    ((= total 0) (list nil))
    ((null? denoms) nil)
    ((< total 0) nil)

    (else (append 
      (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) 
      (list-change total (cdr denoms)) 
      nil))
  ))

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
          expr
         )
        ((quoted? expr)
          expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
            (cons form (cons params (let-to-lambda body))))
          )
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
               (define params (car (zip values)))
               (define vals (cadr (zip values)))

               (cons 
                (cons 'lambda (cons params (let-to-lambda body)))
                (let-to-lambda vals)
               )
              ))
        (else
          (map let-to-lambda expr)
         )))
