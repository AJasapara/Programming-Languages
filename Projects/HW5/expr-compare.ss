#lang racket
(define greekLetterLambda (string->symbol "\u03BB"))
(define (cons* a b) (cons a b))
(define (base x) x)
(define (concat x y) (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))
(define (replace-helper x y copy fun) (cond ([equal? x y] copy) (else (fun x))))
(define (lambda-check firstLambda secondLambda) (if [not (equal? firstLambda secondLambda)] greekLetterLambda secondLambda))
(define (base-check x y) (cond ([equal? x y] x)
    ([and (equal? x #t) (equal? y #f)] '%)
    ([and (equal? x #f) (equal? y #t)] '(not %))
    (else (list 'if '% x y))))

(define (get-type expression) (if [not (pair? expression)] 'base (let ([first-element (car expression)])
	(cond ([equal? first-element 'if] 'if) ([equal? first-element 'quote] 'base) 
          ([and (pair? first-element) (or (equal? (car first-element) 'lambda) (equal? (car first-element) greekLetterLambda))] 'lambda)
          (else 'list)))))

(define (fix-expression right left) (cond ([empty? left] '()) ([not (pair? (car left))] (cons (right (car left)) (fix-expression right (cdr left))))
	([equal? (get-type left) 'lambda] (cons (car left) (fix-expression right (cdr left))))
    (else (cons (fix-expression right (car left)) (fix-expression right (cdr left))))))

(define (lambda-helper x y) (let ([firstChar (cdr x)] [secondChar (cdr y)] [lambdaChanged (lambda-check (car x) (car y))])
    (let* ([ret (lambda-compare base base (car firstChar) (car secondChar))] [returned (take-right ret 2)][result (drop-right ret 2)]
           [firstResult (fix-expression (first returned) (cdr firstChar))][secondResult (fix-expression (second returned) (cdr secondChar))])
    (cons* lambdaChanged (cons* result (expr-compare firstResult secondResult))))))

(define (lambda-compare firstParam secondParam firstParamChanged secondParamChanged)
	(cond ([not (empty? firstParamChanged)] (if (equal? (car firstParamChanged) (car secondParamChanged)) (cons(car firstParamChanged)
		(lambda-compare firstParam secondParam (cdr firstParamChanged) (cdr secondParamChanged)))
        (let ([firstParamReplaced (lambda (x) (replace-helper x (car firstParamChanged) (concat (car firstParamChanged) (car secondParamChanged)) firstParam))]
        	[secondParamReplaced (lambda (y) (replace-helper y (car secondParamChanged) (concat (car firstParamChanged) (car secondParamChanged)) secondParam))])
            (cons (concat (car firstParamChanged) (car secondParamChanged)) (lambda-compare firstParamReplaced secondParamReplaced (cdr firstParamChanged) (cdr secondParamChanged))))))
    	(else(list firstParam secondParam))))

(define (expr-compare x y) (let ([first-type (get-type x)] [second-type (get-type y)]) (cond ([not (equal? first-type second-type)] (list 'if '% x y))
	([equal? first-type 'base] (base-check x y)) ([equal? first-type 'quote] (base-check x y))
    ([not (equal? (length x) (length y))] (list 'if '% x y)) ([equal? first-type 'lambda] (cons (lambda-helper (car x) (car y)) (expr-compare (cdr x) (cdr y))))
    (else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))))))

(define (test-expr-compare x y) (let* ([result (expr-compare x y)] [firstResult (list 'let '([% #t]) result)] [secondResult (list 'let '([% #f]) result)])
	(and (equal? (eval x) (eval firstResult)) (equal? (eval y) (eval secondResult)))))

(define test-expr-x '((λ (x) (eqv? x ((lambda (y x) ((lambda (x y) (x y)) y x)) x (λ (y) x))))(lambda (x y) (x y))))
(define test-expr-y '((lambda (x) (eq? x ((λ (x y) ((λ (x y) (x y)) y x)) x (lambda (x) x)))) (lambda (y x) (y x))))