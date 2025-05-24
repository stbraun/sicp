; Creates a calendar with relevant dates for a given year.
; The calendar is created as a list of entries containing the date, the day of week, and the event.

(module cal-year racket
        (require "calendar.scm")
        (provide create-calendar
                 print-calendar)

        (define (create-calendar year)
          (define (create-entry date event)
            (list date (day-of-week-short-string (day-of-week date)) event))
          (define (create-date day month year)
            (gdate year month day))
          (define calendar '())
          (set! calendar (cons (create-entry (gdate year 1 1) "Neujahr") calendar))
          (set! calendar (cons (create-entry (gdate year 2 4) "*Lara") calendar))
          (set! calendar (cons (create-entry (gdate year 5 19) "*Noah") calendar))
          (set! calendar (cons (create-entry (gdate year 9 16) "*Holger") calendar))
          (set! calendar (cons (create-entry (good-friday year) "Karfreitag") calendar))
          (set! calendar (cons (create-entry (easter-sunday year) "Ostersonntag") calendar))
          (set! calendar (cons (create-entry (easter-monday year) "Ostermontag") calendar))
          (set! calendar (cons (create-entry (whit-sunday year) "Pfingstsonntag") calendar))
          (set! calendar (cons (create-entry (whit-monday year) "Pfingstmontag") calendar))
          (set! calendar (cons (create-entry (gdate year 11 29) "*Meral") calendar))
          (set! calendar (cons (create-entry (advent year 1) "1. Advent") calendar))
          (set! calendar (cons (create-entry (advent year 2) "2. Advent") calendar))
          (set! calendar (cons (create-entry (advent year 3) "3. Advent") calendar))
          (set! calendar (cons (create-entry (advent year 4) "4. Advent") calendar))
          (set! calendar (cons (create-entry (gdate year 12 24) "Heilig Abend") calendar))
          (set! calendar (cons (create-entry (gdate year 12 25) "1. Weihnachtstag") calendar))
          (set! calendar (cons (create-entry (gdate year 12 26) "2. Weihnachtstag") calendar))
          (set! calendar (cons (create-entry (gdate year 12 31) "Silvester") calendar))
          (sort calendar gdate< #:key car))

        ;; Print the calendar in a readable format
        (define (print-calendar calendar)
          (for-each (lambda (entry)
                      (printf "~a ~a - ~a\n" (format-date (car entry)) (cadr entry) (caddr entry)))
                    calendar))

        ) ; end module
