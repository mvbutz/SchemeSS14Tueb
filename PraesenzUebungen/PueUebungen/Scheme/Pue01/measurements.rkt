;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname measurements) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; conversion factors for the different units of measurement
(define inch->cm-factor 2.54)
(define foot->inch-factor 12)
(define yard->foot-factor 3)
(define rod->yard-factor (+ 5 1/2))

; convert inches to cm
(: inches->cm (real -> real))
(check-within (inches->cm 1) inch->cm-factor 0.0001)
(check-within (inches->cm 2) 5.08 0.0001)

(define inches->cm
  (lambda (l)
    (* l inch->cm-factor)))

; convert feet to inches
(: feet->inches (real -> real))
(check-within (feet->inches 1) foot->inch-factor 0.0001)
(check-within (feet->inches 5) 60 0.0001)

(define feet->inches
  (lambda (l)
    (* l foot->inch-factor)))

; convert yards to feet
(: yards->feet (real -> real))
(check-within (yards->feet 1) yard->foot-factor 0.00001)
(check-within (yards->feet 23) 69 0.00001)

(define yards->feet
  (lambda (l)
    (* l yard->foot-factor)))

; convert rods to yards
(: rods->yards (real -> real))
(check-within (rods->yards 1) rod->yard-factor 0.00001)
(check-within (rods->yards 42) 231 0.00001)

(define rods->yards
  (lambda (l)
    (* l rod->yard-factor)))

; convert feet to cm
(: feet->cm (real -> real))
(check-within (feet->cm 3) 91.44 0.00001)
(check-within (feet->cm 12.4) 377.952 0.0001)

(define feet->cm
  (lambda (l)
    (inches->cm (feet->inches l))))

; convert yards to cm
(: yards->cm (real -> real))
(check-within (yards->cm 254) 23225.76 0.0001)
(check-within (yards->cm 1) 91.44 0.0001)

(define yards->cm
  (lambda (l)
    (inches->cm (feet->inches (yards->feet l)))))
    
; convert rods to inches
(: rods->inches (real -> real))
(check-within (rods->inches 1) 198 0.000001)
(check-within (rods->inches 23.5) 4653 0.0001)

(define rods->inches
  (lambda (l)
    (feet->inches (yards->feet (rods->yards l)))))

