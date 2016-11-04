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