; factorial -- from Norvig's lis.py article
(define fact
  (lambda (n)
    (if (<= n 1) 1 (* n (fact (- n 1))))))

(print (fact 10))   ; 3628800
(print (fact 50))   ; 30414093201713378043612608166064768844377641568960512000000000000
