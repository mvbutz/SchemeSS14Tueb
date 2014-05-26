;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname ParkingLot) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
(define even-natural (signature (combined natural (predicate even?))))

; Anzahl PKWs auf einem Parkplatz mit n Fahrzeugen und r Rädern

(: parking-lot-cars (natural even-natural -> natural))
(check-expect (parking-lot-cars 6 12) 0)
(check-expect (parking-lot-cars 3 12) 3)

(define parking-lot-cars 
         (lambda (n r)
           (- (/ r 2) n )))

