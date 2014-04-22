;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname pipe-area) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
(define pi 3.1415)

; compute the square of a number
(: square (number -> number))
(check-expect (square 1) 1)
(check-expect (square 4) 16)
(check-within (square 0.5) 0.25 0.00001)

(define square
  (lambda (x)
    (* x x)))

; compute the area of a circle with radius r
(: circle-area (real -> real))
(check-within (circle-area 5) (* pi (square 5)) 0.00001)
(check-within (circle-area 3.5) 38.483 0.001)

(define circle-area
  (lambda (r)
    (* pi (square r))))

; compute the perimeter of a circle with radius r
(: circle-perimeter (real -> real))
(check-within (circle-perimeter 5) (* 2 pi 5) 0.000001)
(check-within (circle-perimeter 3.5) 21.9905 0.00001)

(define circle-perimeter
  (lambda (r)
    (* 2 pi r)))

; compute the side area of a cylinder with radius r and length l
(: cylinder-side-area (real real -> real))
(check-within (cylinder-side-area 3 8) 150.792 0.001)
(check-within (cylinder-side-area 1.2 7) 52.777 0.001)

(define cylinder-side-area
  (lambda (r l)
    (* l (circle-perimeter r))))

; compute the surface area of a cylinder with radius r and length l
(: cylinder-area (real real -> real))
(check-within (cylinder-area 3.5 8) 252.89 0.001)
(check-within (cylinder-area 23 42) 9393.085 0.000001)

(define cylinder-area
  (lambda (r l)
    (+ (* 2 (circle-area r))
       (cylinder-side-area r l))))

; compute the surface area of a pipe with length l, outer radius R
; and inner radius r (R > r!)
(: pipe-area (real real real -> real))
(check-within (pipe-area 10 4 3) 483.79 0.001)

(define pipe-area
  (lambda (length outer-radius inner-radius)
    (- (+ (cylinder-area outer-radius length)
          (cylinder-side-area inner-radius length))
       (* 2 (circle-area inner-radius)))))
   