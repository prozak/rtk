; fib, range and map -- from Norvig's lis.py article
(define fib
  (lambda (n)
    (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))

(define range
  (lambda (a b)
    (if (= a b) (quote ()) (cons a (range (+ a 1) b)))))

(print (map fib (range 0 10)))   ; (1 1 2 3 5 8 13 21 34 55)

; Derived forms (let, when, and, or) are not built into the interpreter;
; they are macro-expanded into core forms by quasi-quotation rewrite rules
; in scheme-main.hs.
(let ((r 10))
  (when (> r 5)
    (print (* pi (* r r)))))     ; 314.1592653589793
