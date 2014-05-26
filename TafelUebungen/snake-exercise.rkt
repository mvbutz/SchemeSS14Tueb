;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname snake-exercise) (read-case-sensitive #f) (teachpacks ((lib "universe.ss" "teachpack" "deinprogramm") (lib "image2.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "universe.ss" "teachpack" "deinprogramm") (lib "image2.ss" "teachpack" "deinprogramm")))))
; Snake-Spiel - von P. Schulz, modifiziert von M. Butz 2012/2014

; ----------------------------------------------------------------
; Das Spiel wird als record von records dargestellt --------------
; ----------------------------------------------------------------

; Das Spiel selbst besteht aus 
;  -- der Schnlange snake, 
;  -- der aktuellen Bewegungsrichtung dir, 
;  -- der Position des Futters candy, 
;  -- dem Spielfeld field
;  -- dem Spielstand score. 
(define-record-procedures game
  make-game game?
  (game-snake game-dir game-candy game-field game-score))
; Verträge von welcher Signatur die Spielbausteine des Records sind.
(: make-game (snake direction segment field natural -> game))
(: game? (any -> boolean))
(: game-snake (game -> snake))
(: game-dir (game -> direction))
(: game-candy (game -> segment))
(: game-field (game -> field))
(: game-score (game -> natural))

; Die Schlange besteht aus dem 
;  -- Kopfsegment head 
;  -- Restsegment als liste
(define-record-procedures snake
  make-snake
  snake?
  (head tail))
; Verträge: Die Schlange besteht aus Segmenten...
(: make-snake (segment (list-of segment) -> snake))
(: snake? (any -> boolean))
(: head (snake -> segment))
(: tail (snake -> (list-of segment)))

; Ein Segment besteht aus 
;   -- X-Koordinate
;   -- Y-Koordinate
(define-record-procedures segment
  make-segment segment?
  (segment.x segment.y))
(: make-segment (natural natural -> segment))
(: segment? (any -> boolean))
(: segment.x (segment -> natural))
(: segment.y (segment -> natural))

; Die Bewegungsrichtung einer Schlange wird 
;   als String kodiert (insbesondere weil die Tasten auch Strings liefern).
;   Nur die 4 Richtungsstrings sind erlaubt.
(define direction
  (signature (one-of "up" "down" "right" "left")))

; Das Spielfeld besteht aus:
; - Breite
; - Höhe
(define-record-procedures field
  make-field field?
  (field.x field.y))
(: make-field (natural natural -> field))
(: field? (any -> boolean))
(: field.x (field -> natural))
(: field.y (field -> natural))

; ------------------------------------------------------
; Segmentprozeduren, die für das Spiel benötigt werden. 
; ------------------------------------------------------

; Prüft ob zwei Segmente an der selben Position sind.
; (notwendig um zu prüfen, ob sich die Schlange selbst beisst).
(: segment=? (segment segment -> boolean))
(define segment=?
  (lambda (seg1 seg2)
    (let ((x1 (segment.x seg1))
          (x2 (segment.x seg2))
          (y1 (segment.y seg1))
          (y2 (segment.y seg2)))
      (and (= x1 x2)
           (= y1 y2)))))

; Prüft ob ein Segment im Spielfeld ist:
; (notwendig um zu prüfen, ob die Schlange nicht gegen den Rand läuft)
(: inside? (segment field -> boolean))
(define inside?
  (lambda (seg f)
    (let ((sx (segment.x seg))
          (sy (segment.y seg))
          (fx (field.x f))
          (fy (field.y f)))
      (and (< 0 sx fx)
           (< 0 sy fy)))))

; Prüft of Segment in einer liste von Segmenten ist
; (um zu prüfen, ob sich die Schlange selbst beisst)
(: segment-in-list? (segment (list-of segment) -> boolean))
(define segment-in-list?
  (lambda (seg lis)
    (cond
      ((empty? lis) #f)
      (else 
       (if (and (= (segment.x seg) (segment.x (first lis)))
                (= (segment.y seg) (segment.y (first lis))))
           #t
           (segment-in-list? seg (rest lis)))))))

; Prüft ob sich die Schlange sich selbst beisst:
(: bite? (snake -> boolean))
(define bite?
  (lambda (s)
    (segment-in-list? (head s) (tail s))))

; Bewegt ein Segment abhängig von der Richtung dir weiter:
(: move-segment (segment direction -> segment))
(define move-segment
  (lambda (seg k)
    (cond ((string=? k "up") (make-segment (segment.x seg)
                                           (- (segment.y seg) 1)))
          ((string=? k "down") (make-segment (segment.x seg)
                                             (+ (segment.y seg) 1)))
          ((string=? k "right") (make-segment (+ (segment.x seg) 1)
                                              (segment.y seg)))
          ((string=? k "left")  (make-segment (- (segment.x seg) 1)
                                              (segment.y seg))))))

; Bewegt die Schlange:
(: move-snake (snake direction -> snake))
(define move-snake
  (lambda (s d)
    (make-snake (move-segment (head s) d)
                (append (rest (tail s)) (list (head s)) ))))

; Tastaturereignis - prüft ob die Richtung angepast werden sollte. 
; Stellt sicher, dass die Schlange nicht zurück in sich selbst läuft.
(: on-key-pressed (game direction -> game))
(define on-key-pressed
  (lambda (g dir)
    (make-game (game-snake g)
               dir (game-candy g) (game-field g) (game-score g))
    ))

; Zeichnet das Spiel
(: draw.game (game -> image))
(define draw.game
  (lambda (g)
    (let ((s (game-snake g))
          (d (game-dir g))
          (c (game-candy g))
          (f (game-field g))
          (score (game-score g)))
      (fold
       (underlay/xy
        (underlay/xy
         (underlay/xy
          (underlay/xy
           (rectangle (* 10 (- (field.x f) 1)) (- (* 10 (+ 1 (field.y f))) 5) "solid" "black")
           0 0
           (rectangle (* 10 (- (field.x f) 1)) 15 "solid" "blue"))
          5 1 (text (string-append "Score: " (number->string score)) 12 "yellow"))
         (- (* 10 (segment.x c)) 10)
         (* 10 (segment.y c))
         (text "◌" 15 "yellow"))
        (- (* 10 (segment.x (head s))) 10)
        (* 10 (segment.y (head s)))
        (text "□" 15 "gray"))
       (lambda (t img)
         (underlay/xy
          img
          (- (* 10 (segment.x t)) 10)
          (* 10 (segment.y t))
          (text "□" 15 "white")))
       (tail s)))))


; Initialisierung des Anfangszustandes des Spiels...
;  Schlange ist in der Mitte fest definiert und läuft initial nach links
(: initial-game game)
(define initial-game
  (make-game (make-snake (make-segment 15 15)
                         (list (make-segment 16 15)
                               (make-segment 17 15)
                               (make-segment 18 15)))
             "left"
             (make-segment 5 15)
             (make-field 29 29)
             0))

; Frame errechnen:
(: on-tick-move-snake (game -> game))
(define on-tick-move-snake
  (lambda (g)
    (let ((s (game-snake g))
          (d (game-dir g))
          (c (game-candy g))
          (f (game-field g))
          (score (game-score g)))
      (if (or 
           (not (inside? (head s) f))
           (bite? s))
          initial-game
          (if (segment=? c (head s))
              (make-game (move-snake (make-snake (head s)
                                                 (append (tail s)
                                                         (list (head s))))
                                     d)
                         d
                         (make-segment (+ 1 (random 28)) (+ 2 (random 27)))
                         f 
                         (+ 1 score))
              (make-game (move-snake s d)
                         d
                         c
                         f
                         score))))))

; Spiel-Start
(big-bang initial-game
          (on-key on-key-pressed)
          (to-draw draw.game)
          (on-tick on-tick-move-snake 1/10))
