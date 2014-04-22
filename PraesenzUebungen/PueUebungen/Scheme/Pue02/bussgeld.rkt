;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname bussgeld) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Berechnet das Verwarngeld, wenn zu lange geparkt wird.
(: zu-langes-parken (natural -> natural))
(check-expect (zu-langes-parken 0) 0)
(check-expect (zu-langes-parken 15) 5)
(check-expect (zu-langes-parken 30) 5)
(check-expect (zu-langes-parken 55) 10)
(check-expect (zu-langes-parken 65) 15)
(check-expect (zu-langes-parken 120) 15)
(check-expect (zu-langes-parken 180) 20)
(check-expect (zu-langes-parken 181) 25)

(define zu-langes-parken
  (lambda (ueberschreitung)
    (cond ((= ueberschreitung 0) 0)
          ((<= ueberschreitung 30) 5)
          ((<= ueberschreitung 60) 10)
          ((<= ueberschreitung 120) 15)
          ((<= ueberschreitung 180) 20)
          (else 25))))

; gibt das Bussgeld beim Überfahren einer roten Ampel aus
; (abhaengig von der Zeit, die die Ampel schon rot ist und
; dem Vorliegen von Gefährdungen und Sachbeschädigungen)
(: rote-ampel-bussgeld (natural boolean boolean -> integer))

(check-expect (rote-ampel-bussgeld 0 #t #t) 0)
(check-expect (rote-ampel-bussgeld 1 #f #f) 50)
(check-expect (rote-ampel-bussgeld 1 #f #t) 125)
(check-expect (rote-ampel-bussgeld 1 #t #t) 125)
(check-expect (rote-ampel-bussgeld 15 #f #f) 125)
(check-expect (rote-ampel-bussgeld 13 #t #f) 200)
(check-expect (rote-ampel-bussgeld 23 #t #t) 200)

(define rote-ampel-bussgeld
  (lambda (zeit-rot gefährdung sachbeschädigung)
    (cond ((= zeit-rot 0) 0)
          ((= zeit-rot 1)
           (if (and (not gefährdung) (not sachbeschädigung))
               50
               125))
          ((> zeit-rot 1)
           (if (and (not gefährdung) (not sachbeschädigung))
               125
               200)))))

; Berechnet, ob ein Fahrverbot erteilt wird.
(: rote-ampel-fahrverbot (natural boolean boolean -> boolean))

(check-expect (rote-ampel-fahrverbot 0 #f #f) #f)
(check-expect (rote-ampel-fahrverbot 1 #f #f) #f)
(check-expect (rote-ampel-fahrverbot 1 #t #f) #t)
(check-expect (rote-ampel-fahrverbot 1 #t #t) #t)
(check-expect (rote-ampel-fahrverbot 15 #f #f) #t)
(check-expect (rote-ampel-fahrverbot 13 #t #f) #t)
(check-expect (rote-ampel-fahrverbot 23 #t #t) #t)

(define rote-ampel-fahrverbot
  (lambda (zeit-rot gefährdung sachbeschädigung)
    (not (or (= zeit-rot 0) 
             (and (= zeit-rot 1) 
                  (not gefährdung) 
                  (not sachbeschädigung))))))

; Berechnet, wie viele Punkte in Flensburg erteilt werden
(: rote-ampel-punkte (natural boolean boolean -> natural))
               
(check-expect (rote-ampel-punkte 0 #f #f) 0)
(check-expect (rote-ampel-punkte 1 #f #f) 3)
(check-expect (rote-ampel-punkte 1 #t #f) 4)
(check-expect (rote-ampel-punkte 1 #t #t) 4)
(check-expect (rote-ampel-punkte 15 #f #f) 4)
(check-expect (rote-ampel-punkte 13 #t #f) 4)
(check-expect (rote-ampel-punkte 23 #t #t) 4)

(define rote-ampel-punkte
  (lambda (zeit-rot gefährdung sachbeschädigung)
    (cond ((= zeit-rot 0) 0)
          ((and (= zeit-rot 1) (not gefährdung) (not sachbeschädigung)) 3)
          (else 4))))