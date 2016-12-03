; 1.1
10 
12
8
3
6
a = 3
b = 4
19
false
16
6
16

; 1.2 
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 1 3)))))
   (* 3 
      (- 6 2) 
      (- 2 7))

; 1.3
(define (foo x y z) 
  (cond ((and (<= z x) 
              (<= z y))
         (+ (* x x) (* y y)))
        ((and (<= y x)
              (<= y z))
         (+ (* x x) (* z z)))
        ((and (<= x y)
              (<= x z))
         (+ (* y y) (* z z))))) 

; 1.4
; If b is bigger than zero, print a plus b, otherwise print a minus b

; 1.5
; In applicative-order, the evaluation will never stop, because interpreter will
; keep expand (define (p) (p)). In normal-order, the evaluation will output 0, 
; since (if (= x 0) 0 (p)) is 0, (p) won't be executed.

; 1.6
; It will meet a indefinitely loop, since applicative-order interpreter will 
; evaluate the argument first, thus, else-clause in function new-if will keep 
; been evaluated, which create a never ended loop.

; 1.7
; function good-enough? use function square, which is not effective for very 
; large and small numbers. Assume the smallest float number in Scheme is 0.001, 
; (square 0.001) will output mucher smaller number which can not presented by 
; Scheme, same as large number, assume the largest number in Scheme is 1000, 
; (square 1000) will output larger number.
; This function should works better, since it won't call function square on the
; x value. 
(define (good-enough? guess x)
  (< (abs (- (improve guess x) 
             guess)) 
     0.001))

; 1.8 
; curb-root 
(define (improve guess x)
  (/ (+ (/ x
           (* guess guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (- (improve guess x)
             guess))
     0.001))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x)
                 x)))

(define (cbrt x)
  (cbrt-iter 1.0 x))

; 1.9
; recursive
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
(9)
; iterative
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
(9)

; 1.10
; (A 1 10) = 2^10
; (A 2 4) = 2^16
; (A 3 3) = 2^16
; (f n) = 2n
; (g n) = 2^n
; (k n) = ((2^2)^2)...^2) (n-1 times)

; 1.11
; recursive function
(define (f n)
  (if (< n 3) n
  (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

; iterative function
(define (f n)
  
  (define (f-iter a b c n)
    ; a = f(n-1), b = f(n-2), c = f(n-3).
    (if (= n 2) a
        (f-iter (+ a (* 2 b) (* 3 c))
                a
                b
                (- n 1))))

  (if (< n 3) n
      (f-iter 2 1 0 n)))

; 1.12
(define (pascal row col)
  (cond ((= col 1) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1)))))) 

; 1.13
; Assume phi = (1+√5)/2, psi = (1-√5)/2
; Prove Fib(n) = (phi^n - psi^n)/√5
; 
; Basis: Fib(0) = (phi^0 - psi^0)/√5 = 0/√5 = 0
;        Fib(1) = (phi - psi)/√5 = √5/√5 = 1 
;        Fib(0) and Fib(1) holds.
;Inductive step:
;   if Fib(n-2) and Fib(n-1) holds. 
;   Fib(n) = Fib(n-1) + Fib(n-2) 
;          = (phi^(n-1)+phi^(n-2)) - (psi^(n-1)+psi^(n-2))/√5
;          = (phi^(n-2)*(phi+1) - psi^(n-2)*(psi+1))/√5
;   Known phi+1 = phi^2, psi+1 = psi^2
;   Fib(n) = (phi^(n-2)*(phi+1) - psi^(n-2)*(psi+1))/√5
;          = (phi^(n-2)*(phi^2) - psi^(n-2)*(psi^2))/√5
;          = (phi^n - psi^n)/√5
;   Fib(n) holds.
;
; Prove Fib(n) is the closest integer to (phi^n)/√5
; 1/2 >= Fib(n) - (phi^n)/√5 >= -(1/2) 
; (Fib(n) - (phi^n)/√5)^2 >= 1/4
; ((phi^n - psi^n)/√5 - (phi^n)/√5)^2 >= 1/4
; ((psi^n)/√5)^2 >= 1/4
; (psi^2n)/5 >= 1/4
; psi^2n >= 5/4
; ((3-√5)/2)^n >= 5/4
; Since (3-√5)/2 < 1, so ((3-√5)/2)^n < 1 < 5/4 
; Proved.

; 1.14
;                                                                                             /---------(cc 11 5)--------\
;                                                                                            /                            \
;                                                                                           /	                             \
;                                                                       /---------------(cc 11 4)-------------\        (cc -39 5)
;                                                                      /                                       \
;                                                                     /                                         \
;                                   /---------------------------(cc 11 3)----------------------\             (cc -14 4)
;                                  /                                                            \
;                                 /                                                              \
;                  /---------(cc 11 2)-------- \	                                               (cc 1 3)
;                 /                             \                                                 /     \
;                /                               \                                               /       \
;        (cc 11 1)	                      /------(cc 6 2)-----\	                          (cc 1 2)      (cc -9 3)
;       /        \                       /                     \                          /      \
;      /          \                     /                       \                        /        \
; (cc 11 0)   (cc 10 1)	          (cc 6 1)	                    (cc 1 2)	           (cc 1 1)	 (cc -4 2)
;             /       \           /      \                      /      \             /      \
;        (cc 10 0)	(cc 9 1)  (cc 6 0)  (cc 5 1)           (cc 1 1)  (cc -4 2)  (cc 1 0)	 (cc 0 1)*	
;                   /      \            /      \           /      \        
;               (cc 9 0)  (cc 8 1)	(cc 5 0)	(cc 4 1) (cc 1 0)	(cc 0 1)*
;                         /      \            /      \
;                    (cc 8 0)	 (cc 7 1)  (cc 4 0)   (cc 3 1)	
;                               /      \            /      \
;                          (cc 7 0)   (cc 6 1)	(cc 3 0)	(cc 2 1)	
;                                     /      \            /      \
;                                (cc 6 0)	  (cc 5 1)  (cc 2 0)	(cc 1 1)
;                                           /      \            /      \
;                                       (cc 5 0)  (cc 4 1)  (cc 1 0)  (cc 0 1)*
;                                                 /      \
;                                            (cc 4 0)   (cc 3 1)	
;                                                       /      \
;                                                  (cc 3 0)	  (cc 2 1)
;                                                             /      \
;                                                        (cc 2 0)	  (cc 1 1)
;                                                                   /	     \
;                                                               (cc 1 0)  (cc 0 1)*
; count-change(amount)
; Orders of Growth of the Space = amount
; Orders of Growth of the number of steps = 2^5

; 1.15
; 12.15 / 3 = 4.05
; 4.05  / 3 = 1.35
; 1.35  / 3 = 0.45
; 0.45  / 3 = 0.15
; 0.15  / 3 = 0.05
; p applied 5 times
; Order of growth in space: log(a, base=3)
; Order of growth in number of steps: log(a, base=3)

; 1.16
(define (even? n)
  (= (remainder n 2) 0))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
  (cond ((= n 1) (* a b))
        ((even? n) (expt-iter (* b b) (/ n 2) a))
        (else (expt-iter (* b b) (/ (- n 1) 2) (* a b)))))
 
; 1.17
(define (even? n)
  (= (remainder n 2) 0))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (fast-multi a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-multi (double a) (halve b)))
        (else (+ a (fast-multi a (- b 1))))))

; 1.18
(define (even? n)
  (= (remainder n 2) 0))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (fast-multi-iter a b result)
  (cond ((= b 0) 0)
        ((= b 1) (+ a result))
        ((even? b) (fast-multi-iter (double a) (halve b) result))
        (else (fast-multi-iter a (- b 1) (+ result a)))))

(define (fast-multi a b)
  (fast-multi-iter a b 0))

; 1.19
(define (even? n)
  (= (remainder n 2) 0))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))    ; compute p'
                   (+ (* 2 p q) (* q q))  ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; 1.20
; Normal order evaluation
(gcd 206 40)
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40))
(if (= (remainder 206 40)
(if (= 6) ; 1 
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40)) 0)
(if (= 4) ; 1 + 2
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
(if (= 2) ; 1 + 2 + 4
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
(if (= (remainder 4 2))  ; 1 + 2 + 4 + 6
(if (= 0)) ; 1 + 2 + 4 + 6 + 1
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (remainder 6 4) ; 1 + 2 + 4 + 6 + 1 + 3
    2 ; 1 + 2 + 4 + 6 + 1 + 3 + 1 = 18 remainder operations.

; Applicative order evaluation
(gcd 206 40)
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40)))
(gcd 40 6) ; 1 
(if (= 6 0)
    40 
    (gcd 6 (remainder 40 6)))

(gcd 6 (remainder 40 6)))
(gcd 6 4) ; 1 + 1
(if (= 4 0)
    6 
    (gcd 4 (remainder 6 4)))

(gcd 4 (remainder 6 4)))
(gcd 4 2) ; 1 + 1 + 1
(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))
  
(gcd 2 (remainder 4 2)))
(gcd 2 0) ; 1 + 1 + 1 + 1
(if (= 0 0) 
    2)
; 4 remainder operations.

; 1.21
(smallest-divisor 199)
199
(smallest-divisor 1999)
1999
(smallest-divisor 199)
7

; 1.22
#lang scheme
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square x) (* x x))

(define (runtime) (current-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (when (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (three-smallest-primes start-from)
  (smallest-primes start-from 3))

(define (smallest-primes start-from count)
  (cond ((= count 0) (newline))
        [(prime? start-from)
         (timed-prime-test start-from)
          (smallest-primes (+ start-from 1) (- count 1))]
        (else
         (smallest-primes (+ start-from 1) count))))

(three-smallest-primes 199)
; 199 *** 0
; 211 *** 0
; 223 *** 0
(three-smallest-primes 1999) 
; 1999 *** 0
; 2003 *** 0
; 2011 *** 0
(three-smallest-primes 19999)
; 20011 *** 0
; 20021 *** 0
; 20023 *** 0

; 1.23
(define (next num) 
  (if (= num 2) 
      3
      (+ num 2)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

; 1.24
(define (square x) (* x x))

(define (runtime) (current-milliseconds))

; Fast prime test using fermat test
(define (fermat-test n)
  (define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
  
  (define (try-it a)
    (= (expmod a n n) a))

  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Fast Prime using fermat test.
(define (timed-prime-test n)
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

  (define (start-prime-test n start-time)
    (when (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

  (newline)
  (display n)
  (start-prime-test n (runtime)))

; 1.25
; As foot note 46 said, the original (expmod base exp m) perform computation 
; without ever having to deal with numbers larger than m. If we use the 
; fast-expt function, we are going to compute very large numbers when the input 
; is bigger enough, which will cost more time.

; 1.26
; In Louis's code, the expmod fucntion will executed twice if they are not using 
; square function. Since the expmod function is a recursive function, the 
; recursion function changed from linear recursive into a exponential recursive.
; (log n) became (log 2^n) = n 

; 1.27
#lang scheme
; helper function
(define (square x) (* x x))

; prime? function
(define (prime? n)
  (define (divides? a b)
    (= (remainder b a) 0))

  (define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

  (define (smallest-divisor n)
    (find-divisor n 2))
  
  (= n (smallest-divisor n)))

; Fermat test
(define (fermat-test n b)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m))
                      m))
          (else
           (remainder (* base (expmod base (- exp 1) m))
                      m))))
  
  (define (try-it a)
    (= (expmod a n n) a))

  (cond ((= b n) true)
        ((try-it b) (fermat-test n (+ b 1)))
        (else false)))
 

; Carmichael test
(define (carmichael n)
  (cond ((prime? n) false)
        (else (fermat-test n 1))))

(carmichael 10)     ; f
(carmichael 11)     ; f
(carmichael 561)    ; t
(carmichael 1105)   ; t
(carmichael 1729)   ; t
(carmichael 2465)   ; t
(carmichael 2821)   ; t
(carmichael 6601)   ; t

; 1.28
(define (expmod base exp m)
  (define (square-then-mod x mod)
    (cond ((= x 1) 
            (remainder (* x x) mod))
          ((= x (- mod 1)) 
            (remainder (* x x) mod))
          ((= 1 (remainder (* x x) mod)) 
            0)
          (else (remainder (* x x) mod))))

  (cond ((= exp 0) 1)
        ((even? exp)
         (square-then-mod (expmod base (/ exp 2) m)
                          m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))) 

(define (miller-rabin-test n)
  (define (even? n)
    (= (remainder n 2) 0))

  (define (try-it-iter n a)
    (cond ((> a (/ n 2)) true)
          ((= (expmod a (- n 1) n) 0) false)
          (else (try-it-iter n (+ a 1)))))
    
  (define (try-it n)
    (try-it-iter n 2))
  
  (cond ((= n 2) true) 
        ((even? n) false)  
        (else (try-it n))))

(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)
