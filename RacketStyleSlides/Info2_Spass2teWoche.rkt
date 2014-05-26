;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname Info2_Spass2teWoche) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
(define hello "Hallo Studierende!")

(define f1 "Wie geht es heute?")

(define resp1 
  (lambda (resp) 
    (cond 
      ((equal? resp "gut") "Freut mich zu hoeren! :-)")
      ((equal? resp "schlecht") "Schade - Remember: Always look on the bright side of life :-)")
      ((equal? resp "42") "Und kennst Du auch die Frage?")
      (else "Das verstehe ich leider nicht :-("))))

