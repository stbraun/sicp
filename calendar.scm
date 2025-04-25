; Calendrical calculations.
; This module provides functions for calculating dates, day of the week,
; leap years, and holidays in the Gregorian calendar.
; The Gregorian calendar is the calendar used in most of the world today.
; It was introduced by Pope Gregory XIII in 1582 and is a modification of the Julian calendar.
; The Gregorian calendar is a solar calendar with 12 months of 28 to 31 days each.
; The year is divided into 12 months, with 30 or 31 days in each month, except for February,


(module Calendar racket
        (provide 
                 easter-sunday
                 good-friday
                 easter-monday
                 palm-sunday
                 whit-sunday
                 whit-monday
                 day-of-week
                 day-of-week-from-fixed
                 day-of-week-string
                 month-string
                 gregorian->fixed
                 fixed->gregorian
                 gregorian-date-difference
                 day-number-in-year
                 gregorian-date-n-days-from-date
                 kday-on-or-before
                 kday-on-or-after
                 kday-before
                 kday-after
                 kday-nearest
                 leap-year?
                 )

        ; The Gregorian epoch is defined as 1 even though the Gregorian calendar was introduced in 1582.
        (define gregorian-epoch 1)

        ; Define the months of the year as numbers.
        (define january 1)
        (define february (+ january 1))
        (define march (+ february 1))
        (define april (+ march 1))
        (define may (+ april 1))
        (define june (+ may 1))
        (define july (+ june 1))
        (define august (+ july 1))
        (define september (+ august 1))
        (define october (+ september 1))
        (define november (+ october 1))
        (define december (+ november 1))

        ; Define the months of the year as strings.
        (define months-of-year
          (list "January" "February" "March" "April" "May" "June"
                "July" "August" "September" "October" "November" "December"))

        ; Return the given month as a string.
        ; The months of the year are defined as numbers from 1 (January) to 12 (December).
        ; The function checks if the given month is valid and returns the corresponding string.
        ; If the month is invalid, an error is raised.
        (define (month-string month)
          (if (or (< month january) (> month december))
              (error "Invalid month")
              ; Use a cond statement to return the corresponding string.
              (list-ref months-of-year (- month 1))))

        (module+ test
                 (require rackunit)
                 (check-equal? (month-string january) "January")
                 (check-equal? (month-string february) "February")
                 (check-equal? (month-string march) "March")
                 (check-equal? (month-string april) "April")
                 (check-equal? (month-string may) "May")
                 (check-equal? (month-string june) "June")
                 (check-equal? (month-string july) "July")
                 (check-equal? (month-string august) "August")
                 (check-equal? (month-string september) "September")
                 (check-equal? (month-string october) "October")
                 (check-equal? (month-string november) "November")
                 (check-equal? (month-string december) "December"))

        ; Define the days of the week as numbers.
        (define sunday 0)
        (define monday (+ sunday 1))
        (define tuesday (+ monday 1))
        (define wednesday (+ tuesday 1))
        (define thursday (+ wednesday 1))
        (define friday (+ thursday 1))
        (define saturday (+ friday 1))

        ; Define names for the days of the week.
        (define days-of-week
          (list "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

        ; Return the given day of week as a string.
        ; The days of the week are defined as numbers from 0 (Sunday) to 6 (Saturday).
        ; The function checks if the given day is valid and returns the corresponding string.
        ; If the day is invalid, an error is raised.
        (define (day-of-week-string day)
          (if (or (< day sunday) (> day saturday))
              (error "Invalid day of week")
              ; Use a cond statement to return the corresponding string.
              (list-ref days-of-week day)))

        (module+ test
                 (require rackunit)
                 (check-equal? (day-of-week-string sunday) "Sunday")
                 (check-equal? (day-of-week-string monday) "Monday")
                 (check-equal? (day-of-week-string tuesday) "Tuesday")
                 (check-equal? (day-of-week-string wednesday) "Wednesday")
                 (check-equal? (day-of-week-string thursday) "Thursday")
                 (check-equal? (day-of-week-string friday) "Friday")
                 (check-equal? (day-of-week-string saturday) "Saturday"))

        ; Determine the day of week for a given date.
        ; https://de.wikipedia.org/wiki/Wochentagsberechnung
        ; The day of the week is calculated as a number from 0 (Sunday) to 6 (Saturday).
        ; The formula is based on Zeller's Congruence.
        (define (day-of-week day month year)
          ; Adjust month and year for Zeller's algorithm 
          (define m-julian (if (< month 3) (+ month 10) (- month 2)))
          (define year-fixed (if (< month 3) (- year 1) year))
          (define y (modulo year-fixed 100))
          (define c (quotient year-fixed 100))
          (define wd (modulo (- (+ day (floor (- (* 13/5 m-julian) 1/5)) y (quotient y 4) (quotient c 4)) (* 2 c)) 7))  
          ;; Return the day of the week as a symbol.
          wd)

        (module+ test
                 (require rackunit)
                 (check-equal? (day-of-week 1 1 1900) monday)
                 (check-equal? (day-of-week 4 2 1900) sunday)
                 (check-equal? (day-of-week 4 6 2006) sunday)
                 (check-equal? (day-of-week 12 6 2006) monday)
                 (check-equal? (day-of-week 1 1 2023) sunday)
                 (check-equal? (day-of-week 25 12 2023) monday)
                 (check-equal? (day-of-week 4 7 2023) tuesday)
                 (check-equal? (day-of-week 12 4 2025) saturday))

        ; True if the given year is a leap year.
        ; A year is a leap year if it is divisible by 4, but not divisible by 100,
        ; unless it is also divisible by 400.:w
        ; https://en.wikipedia.org/wiki/Leap_year#Gregorian_calendar
        (define (leap-year? year)
          (or (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))
              (= (modulo year 400) 0)))

        (module+ test
                 (require rackunit)
                 (check-true (leap-year? 2000))
                 (check-false (leap-year? 1700))
                 (check-false (leap-year? 1800))
                 (check-false (leap-year? 1900))
                 (check-true (leap-year? 2020))
                 (check-false (leap-year? 2021))
                 (check-true (leap-year? 2024))
                 (check-false (leap-year? 2025)))

        ; Gregorian date difference.
        ; The difference between two Gregorian dates in days.
        (define (gregorian-date-difference day1 month1 year1 day2 month2 year2)
          (define fixed1 (gregorian->fixed day1 month1 year1))
          (define fixed2 (gregorian->fixed day2 month2 year2))
          (- fixed2 fixed1))

        (module+ test
                 (require rackunit)
                 (check-equal? (gregorian-date-difference 1 1 2000 1 1 2001) 366)
                 (check-equal? (gregorian-date-difference 1 1 2000 1 1 2002) 731)
                 (check-equal? (gregorian-date-difference 1 1 2000 1 1 2020) 7305)
                 (check-equal? (gregorian-date-difference 1 1 2000 1 1 2024) 8766))

        ; Calculate the ordinal day number (R.D.) of a date within its year.
        ; The ordinal day number is the number of days since the beginning of the year.
        ; The first day of the year is 1, the second day is 2, and so on.
        (define (day-number-in-year day month year)
          (gregorian-date-difference 31 12 (- year 1) day month year))
        (module+ test
                 (require rackunit)
                 (check-equal? (day-number-in-year 1 1 2000) 1)
                 (check-equal? (day-number-in-year 1 2 2000) 32)
                 (check-equal? (day-number-in-year 1 3 2000) 61)
                 (check-equal? (day-number-in-year 1 4 2000) 92)
                 (check-equal? (day-number-in-year 1 5 2000) 122)
                 (check-equal? (day-number-in-year 1 6 2000) 153)
                 (check-equal? (day-number-in-year 1 7 2000) 183)
                 (check-equal? (day-number-in-year 1 8 2000) 214)
                 (check-equal? (day-number-in-year 1 9 2000) 245)
                 (check-equal? (day-number-in-year 1 10 2000) 275)
                 (check-equal? (day-number-in-year 1 11 2000) 306)
                 (check-equal? (day-number-in-year 1 12 2000) 336)
                 (check-equal? (day-number-in-year 1 1 2001) 1)        
                 (check-equal? (day-number-in-year 1 3 2001) 60)        
                 (check-equal? (day-number-in-year 13 3 2001) 72))

        ; Calculate the remaining days in the year from a given date.
        ; The remaining days are the difference between the given date and the last day of the year.
        ; The last day of the year is always 31 December.
        (define (days-remaining day month year)
          (gregorian-date-difference day month year 31 12 year))
        (module+ test
                 (require rackunit)
                 (check-equal? (days-remaining 1 1 2000) 365)
                 (check-equal? (days-remaining 1 2 2000) 334)
                 (check-equal? (days-remaining 1 3 2000) 305)
                 (check-equal? (days-remaining 1 4 2000) 274)
                 (check-equal? (days-remaining 1 5 2000) 244)
                 (check-equal? (days-remaining 1 6 2000) 213)
                 (check-equal? (days-remaining 1 7 2000) 183)
                 (check-equal? (days-remaining 1 8 2000) 152)
                 (check-equal? (days-remaining 1 9 2000) 121)
                 (check-equal? (days-remaining 1 10 2000) 91)
                 (check-equal? (days-remaining 1 11 2000) 60)
                 (check-equal? (days-remaining 1 12 2000) 30))

        ; Determine the date a given number of days in the future.
        ; The date is calculated by adding the number of days to the given date.
        (define (gregorian-date-n-days-from-date day month year days)
          (define fixed-date (gregorian->fixed day month year))
          (define future-date (+ fixed-date days))
          (fixed->gregorian future-date))
        (module+ test
                 (require rackunit)
                 (check-equal? (gregorian-date-n-days-from-date 1 1 2000 1) '(2 1 2000))
                 (check-equal? (gregorian-date-n-days-from-date 1 1 2000 30) '(31 1 2000))
                 (check-equal? (gregorian-date-n-days-from-date 1 1 2000 60) '(1 3 2000))
                 (check-equal? (gregorian-date-n-days-from-date 1 1 2000 365) '(31 12 2000))
                 (check-equal? (gregorian-date-n-days-from-date 31 12 2000 -366) '(31 12 1999)))

        ; Determine the k-th day of the week on or after a given date.
        (define (day-of-week-from-fixed day-number)
          (modulo day-number 7))

        (module+ test
                 (require rackunit)
                 (check-equal? (day-of-week-from-fixed 1) monday)
                 (check-equal? (day-of-week-from-fixed 2) tuesday)
                 (check-equal? (day-of-week-from-fixed 3) wednesday)
                 (check-equal? (day-of-week-from-fixed 4) thursday)
                 (check-equal? (day-of-week-from-fixed 5) friday)
                 (check-equal? (day-of-week-from-fixed 6) saturday)
                 (check-equal? (day-of-week-from-fixed 7) sunday)
                 (check-equal? (day-of-week-from-fixed (gregorian->fixed 4 2 1900)) sunday))

        ; Determine the k-th day of the week on or before a given date.
        ; Where k is the day of the week (0 = Sunday, 1 = Monday, ..., 6 = Saturday).
        (define (kday-on-or-before day-number k)
          (- day-number (day-of-week-from-fixed (- day-number k))))

        (module+ test
                 (require rackunit)
                 (check-equal? (kday-on-or-before 1 sunday) 0)
                 (check-equal? (kday-on-or-before 2 sunday) 0)
                 (check-equal? (kday-on-or-before 3 sunday) 0)
                 (check-equal? (kday-on-or-before 4 sunday) 0)
                 (check-equal? (kday-on-or-before 5 sunday) 0)
                 (check-equal? (kday-on-or-before 6 sunday) 0)
                 (check-equal? (kday-on-or-before 7 sunday) 7)
                 (check-equal? (kday-on-or-before 8 sunday) 7)
                 (check-equal? (kday-on-or-before 9 sunday) 7)
                 (check-equal? (kday-on-or-before 10 sunday) 7)
                 (check-equal? (kday-on-or-before (gregorian->fixed 4 2 1900) wednesday) (gregorian->fixed 31 1 1900)))

        ; Determine the k-th day of the week on or after a given date.
        ; Where k is the day of the week (0 = Sunday, 1 = Monday, ..., 6 = Saturday).
        (define (kday-on-or-after day-number k)
          (kday-on-or-before (+ day-number 6) k))

        (module+ test
                 (require rackunit)
                 (check-equal? (kday-on-or-after 1 sunday) 7)
                 (check-equal? (kday-on-or-after 2 sunday) 7)
                 (check-equal? (kday-on-or-after 3 sunday) 7)
                 (check-equal? (kday-on-or-after 4 sunday) 7)
                 (check-equal? (kday-on-or-after 5 sunday) 7)
                 (check-equal? (kday-on-or-after 6 sunday) 7)
                 (check-equal? (kday-on-or-after 7 sunday) 7)
                 (check-equal? (kday-on-or-after 8 sunday) 14)
                 (check-equal? (kday-on-or-after 9 sunday) 14)
                 (check-equal? (kday-on-or-after 10 sunday) 14)
                 (check-equal? (kday-on-or-after (gregorian->fixed 4 2 1900) wednesday) (gregorian->fixed 7 2 1900)))

        ; Determine the k-th day of the week before a given date.
        ; Where k is the day of the week (0 = Sunday, 1 = Monday, ..., 6 = Saturday).
        (define (kday-before day-number k)
          (kday-on-or-before (- day-number 1) k))

        (module+ test
                 (require rackunit)
                 (check-equal? (kday-before 1 sunday) 0)
                 (check-equal? (kday-before 2 sunday) 0)
                 (check-equal? (kday-before 3 sunday) 0)
                 (check-equal? (kday-before 4 sunday) 0)
                 (check-equal? (kday-before 5 sunday) 0)
                 (check-equal? (kday-before 6 sunday) 0)
                 (check-equal? (kday-before 7 sunday) 0)
                 (check-equal? (kday-before 8 sunday) 7)
                 (check-equal? (kday-before 9 sunday) 7)
                 (check-equal? (kday-before 10 sunday) 7)
                 (check-equal? (kday-before (gregorian->fixed 4 2 1900) wednesday) (gregorian->fixed 31 1 1900)))

        ; Determine the k-th day of the week after a given date.
        ; Where k is the day of the week (0 = Sunday, 1 = Monday, ..., 6 = Saturday).
        (define (kday-after day-number k)
          (kday-on-or-before (+ day-number 7) k))

        (module+ test
                 (require rackunit)
                 (check-equal? (kday-after 1 sunday) 7)
                 (check-equal? (kday-after 2 sunday) 7)
                 (check-equal? (kday-after 3 sunday) 7)
                 (check-equal? (kday-after 4 sunday) 7)
                 (check-equal? (kday-after 5 sunday) 7)
                 (check-equal? (kday-after 6 sunday) 7)
                 (check-equal? (kday-after 7 sunday) 14)
                 (check-equal? (kday-after 8 sunday) 14)
                 (check-equal? (kday-after 9 sunday) 14)
                 (check-equal? (kday-after 10 sunday) 14)
                 (check-equal? (kday-after (gregorian->fixed 4 2 1900) wednesday) (gregorian->fixed 7 2 1900)))

        ; Determine the k-th day of the week nearest a given date.
        ; Where k is the day of the week (0 = Sunday, 1 = Monday, ..., 6 = Saturday).
        (define (kday-nearest day-number k)
          (kday-on-or-before (+ day-number 3) k))

        (module+ test
                 (require rackunit)
                 (check-equal? (kday-nearest 1 sunday) 0)
                 (check-equal? (kday-nearest 2 sunday) 0)
                 (check-equal? (kday-nearest 3 sunday) 0)
                 (check-equal? (kday-nearest 4 sunday) 7)
                 (check-equal? (kday-nearest 5 sunday) 7)
                 (check-equal? (kday-nearest 6 sunday) 7)
                 (check-equal? (kday-nearest 7 sunday) 7)
                 (check-equal? (kday-nearest 8 sunday) 7)
                 (check-equal? (kday-nearest 9 sunday) 7)
                 (check-equal? (kday-nearest 10 sunday) 7)
                 (check-equal? (kday-nearest 11 sunday) 14)
                 (check-equal? (kday-nearest (gregorian->fixed 4 2 1900) wednesday) (gregorian->fixed 7 2 1900)))

        ;; ----- conversions -----

        ; Convert a Gregorian date to the fixed number of days since the Gregorian epoch.
        ; Calendrical Calculations pg. 51
        ; The fixed day number is the number of days since the Gregorian epoch.
        (define (gregorian->fixed day month year)
          (+ (- gregorian-epoch 1)                 ; days before the epoch of the calendar
             (* 365 (- year 1))                    ; days in the years before the current year 
             (floor (/ (- year 1) 4))              ; consider leapyears
             (- (floor (/ (- year 1) 100))) 
             (floor (/ (- year 1) 400))
             (floor (/ (- (* 367 month) 362) 12))  ; days of the partial year up the current month
             (cond ((<= month 2) 0)                ; consider length of February
                   ((and (> month 2) (leap-year? year)) -1)
                   (else -2))
             day))                                 ; days in the current month

        (module+ test
                 (require rackunit)
                 ; See https://www.vcalc.com/equation/?uuid=9e167c54-77aa-11e5-a3bb-bc764e2038f2 for an RD calculator to get test data.
                 (check-equal? (gregorian->fixed 1 1 1) 1)
                 (check-equal? (gregorian->fixed 12 11 1945) 710347)
                 (check-equal? (gregorian->fixed 29 10 1958) 715081)
                 (check-equal? (gregorian->fixed 1 1 2000) 730120)
                 (check-equal? (gregorian->fixed 15 4 2025) 739356))

        ; Convert a fixed day number to a Gregorian date.
        ; Calendrical Calculations pg. 52ff
        ; The fixed day number is the number of days since the Gregorian epoch.
        (define (fixed->gregorian day-number)
          (define (gregorian-year-from-fixed day-num)
            (define d0 (- day-num gregorian-epoch))
            (define d1 (modulo d0 146097))
            (define n400 (quotient d0 146097))
            (define n100 (quotient d1 36524))
            (define d2 (modulo d1 36524))
            (define n4 (quotient d2 1461))
            (define d3 (modulo d2 1461))
            (define n1 (quotient d3 365))
            (define year (+ (* n400 400) (* n100 100) (* n4 4) n1))
            (if (or (equal? n100 4) (equal? n1 4))
              year
              (+ year 1)))
          (define year (gregorian-year-from-fixed day-number))
          (define prior-days (- day-number (gregorian->fixed 1 1 year)))
          (define correction (cond ((< day-number (gregorian->fixed 1 3 year)) 0)
                                   ((and (leap-year? year) (>= day-number (gregorian->fixed 1 3 year))) 1)
                                   (else 2)))
          (define month (quotient (+ (* 12 (+ prior-days correction)) 373) 367))
          (define day (+ (- day-number (gregorian->fixed 1 month year)) 1))
          `(,day ,month ,year)) ; Return the date as a list of the form '(day month year)

        (module+ test
                 (require rackunit)
                 ; See https://www.vcalc.com/equation/?uuid=9e167c54-77aa-11e5-a3bb-bc764e2038f2 for an RD calculator to get test data.
                 (check-equal? (fixed->gregorian 1) '(1 1 1))
                 (check-equal? (fixed->gregorian 710347) '(12 11 1945))
                 (check-equal? (fixed->gregorian 715081) '(29 10 1958))
                 (check-equal? (fixed->gregorian 730120) '(1 1 2000))
                 (check-equal? (fixed->gregorian 739356) '(15 4 2025)))

        ;; ----- holidays -----

        ; Determine the easter sunday date for a given year using Spencer's algorithm.
        ; https://de.wikipedia.org/wiki/Spencers_Osterformel
        (define (easter-sunday year)
          (define a (modulo year 19))
          (define b (quotient year 100))
          (define c (modulo year 100))
          (define d (quotient b 4))
          (define e (modulo b 4))
          (define f (quotient (+ b 8) 25))
          (define g (quotient (- b (+ f 1)) 3))
          (define h (modulo (+ (* 19 a) b (- d) (- g) 15) 30))
          (define i (quotient c 4))
          (define k (modulo c 4))
          (define l (modulo (+ 32 (* 2 e) (* 2 i) (- h) (- k)) 7))
          (define m (quotient (+ a (* 11 h) (* 22 l)) 451))
          (define n (quotient (+ h l (- (* 7 m)) 114) 31))
          (define o (modulo (+ h l (- (* 7 m)) 114) 31))
          (define day (+ o 1))
          (define month n)
          ;; Return the date of Easter Sunday
          ;; as a list of the form '(month day year)
          `(,day ,month ,year))

        (module+ test
                 (require rackunit)
                 (check-equal? (easter-sunday 2000) '(23 4 2000))
                 (check-equal? (easter-sunday 2021) '(4 4 2021))
                 (check-equal? (easter-sunday 2022) '(17 4 2022))
                 (check-equal? (easter-sunday 2023) '(9 4 2023))
                 (check-equal? (easter-sunday 2024) '(31 3 2024))
                 (check-equal? (easter-sunday 2025) '(20 4 2025))
                 (check-equal? (easter-sunday 2026) '(5 4 2026)))

        ; Determine the good friday date for a given year.
        ; Good Friday is the Friday before Easter Sunday.
        (define (good-friday year)
          (define easter-date (easter-sunday year))
          (fixed->gregorian
           (- (gregorian->fixed (car easter-date) (cadr easter-date) (caddr easter-date)) 2))) ; Subtract 2 days from Easter Sunday

        (module+ test
                 (require rackunit)
                 (check-equal? (good-friday 2000) '(21 4 2000))
                 (check-equal? (good-friday 2021) '(2 4 2021))
                 (check-equal? (good-friday 2022) '(15 4 2022))
                 (check-equal? (good-friday 2023) '(7 4 2023))
                 (check-equal? (good-friday 2024) '(29 3 2024))
                 (check-equal? (good-friday 2025) '(18 4 2025))
                 (check-equal? (good-friday 2026) '(3 4 2026)))

        ; Determine the easter monday date for a given year.
        ; Easter Monday is the Monday after Easter Sunday.
        (define (easter-monday year)
          (define easter-date (easter-sunday year))
          (fixed->gregorian
           (+ (gregorian->fixed (car easter-date) (cadr easter-date) (caddr easter-date)) 1))) ; Add 1 days to Easter Sunday

        (module+ test
                 (require rackunit)
                 (check-equal? (easter-monday 2000) '(24 4 2000))
                 (check-equal? (easter-monday 2021) '(5 4 2021))
                 (check-equal? (easter-monday 2022) '(18 4 2022))
                 (check-equal? (easter-monday 2023) '(10 4 2023))
                 (check-equal? (easter-monday 2024) '(1 4 2024))
                 (check-equal? (easter-monday 2025) '(21 4 2025))
                 (check-equal? (easter-monday 2026) '(6 4 2026)))

        ; Determine the date of palm sunday for a given year.
        ; Palm Sunday is the Sunday before Easter Sunday.
        (define (palm-sunday year)
          (define easter-date (easter-sunday year))
          (fixed->gregorian
           (- (gregorian->fixed (car easter-date) (cadr easter-date) (caddr easter-date)) 7))) ; Subtract 7 days from Easter Sunday

        (module+ test
                 (require rackunit)
                 (check-equal? (palm-sunday 2000) '(16 4 2000))
                 (check-equal? (palm-sunday 2021) '(28 3 2021))
                 (check-equal? (palm-sunday 2022) '(10 4 2022))
                 (check-equal? (palm-sunday 2023) '(2 4 2023))
                 (check-equal? (palm-sunday 2024) '(24 3 2024))
                 (check-equal? (palm-sunday 2025) '(13 4 2025))
                 (check-equal? (palm-sunday 2026) '(29 3 2026)))

        ; Determine the date of whit sunday for a given year.
        ; Whit Sunday is the 7th Sunday after Easter Sunday.
        (define (whit-sunday year)
          (define easter-date (easter-sunday year))
          (fixed->gregorian
           (+ (gregorian->fixed (car easter-date) (cadr easter-date) (caddr easter-date)) 49))) ; Add 49 days to Easter Sunday

        (module+ test
                 (require rackunit)
                 (check-equal? (whit-sunday 2000) '(11 6 2000))
                 (check-equal? (whit-sunday 2021) '(23 5 2021))
                 (check-equal? (whit-sunday 2022) '(5 6 2022))
                 (check-equal? (whit-sunday 2023) '(28 5 2023))
                 (check-equal? (whit-sunday 2024) '(19 5 2024))
                 (check-equal? (whit-sunday 2025) '(8 6 2025))
                 (check-equal? (whit-sunday 2026) '(24 5 2026)))

        ; Determine the date of whit monday for a given year.
        ; Whit Monday is the day after Whit Sunday.
        (define (whit-monday year)
          (define whit-date (whit-sunday year))
          (fixed->gregorian
           (+ (gregorian->fixed (car whit-date) (cadr whit-date) (caddr whit-date)) 1))) ; Add 1 day to Whit Sunday

        (module+ test
                 (require rackunit)
                 (check-equal? (whit-monday 2000) '(12 6 2000))
                 (check-equal? (whit-monday 2021) '(24 5 2021))
                 (check-equal? (whit-monday 2022) '(6 6 2022))
                 (check-equal? (whit-monday 2023) '(29 5 2023))
                 (check-equal? (whit-monday 2024) '(20 5 2024))
                 (check-equal? (whit-monday 2025) '(9 6 2025))
                 (check-equal? (whit-monday 2026) '(25 5 2026)))

        ) ; end module
