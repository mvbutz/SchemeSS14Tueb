;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname pyramid-edge) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; compute the circumference of a square with side length a
(: square-circumference (real -> real))

(check-expect (square-circumference 1) 4)
(check-within (square-circumference 0.25) 1 0)

(define square-circumference
  (lambda (side-length)
    (* 4 side-length)))

; compute the square of a number
(: square (number -> number))
(check-expect (square 3) 9)
(check-expect (square 1) 1)
(check-within (square 0.5) 0.25 0.000001)

(define square
  (lambda (x)
    (* x x)))

; solve formula of pythagoras
(: pythagoras (real real -> real))
(check-within (pythagoras 1 1) (sqrt 2) 0.0001)
(check-within (pythagoras 3 3) (sqrt 18) 0.00001)

(define pythagoras
  (lambda (a b)
    (sqrt (+ (square a) (square b)))))

; compute the pyramid side length s
(: pyramid-side-length (real real -> real))
(check-within (pyramid-side-length 5 10) 10.6 0.01)
(check-within (pyramid-side-length 2 1) 1.73 0.01)

(define pyramid-side-length
  (lambda (a h)
    (pythagoras (/ a 2) 
                (pythagoras (/ a 2) h))))

; compute the overall pyramid edge length
(: pyramid-overall-edge-length (real real -> real))
(check-within (pyramid-overall-edge-length 5 10) 62.42 0.01)
(check-within (pyramid-overall-edge-length 2 1) 14.92 0.01)
(check-within (pyramid-overall-edge-length 23 5)
              (+ (* 4 23)
                 (* 4 (pyramid-side-length 23 5)))
              0.0001)

(define pyramid-overall-edge-length
  (lambda (a h)
    (+ (square-circumference a)
       (* 4 (pyramid-side-length a h)))))
