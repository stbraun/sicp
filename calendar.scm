; Calendrical calculations.
; This module provides functions for calculating dates, day of the week,
; leap years, and holidays in the Gregorian calendar.
; The Gregorian calendar is the calendar used in most of the world today.
; It was introduced by Pope Gregory XIII in 1582 and is a modification of the Julian calendar.
; The Gregorian calendar is a solar calendar with 12 months of 28 to 31 days each.
; The year is divided into 12 months, with 30 or 31 days in each month, except for February,


(module Calendar racket
        (provide 
                 gdate
                 gdate-year
                 gdate-month
                 gdate-day
                 gdate<
                 gdate>=
                 gdate=
                 format-date
                 easter-sunday
                 good-friday
                 easter-monday
                 palm-sunday
                 whit-sunday
                 whit-monday
                 advent
                 day-of-week
                 day-of-week-from-fixed
                 day-of-week-string
                 day-of-week-short-string
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
                 dst-start-de
                 dst-end-de
                 dst-start-us
                 dst-end-us
                 )

        ; The type describing a Gregorian date.
        ; The date is represented as a structure with three fields: year, month, and day.
        ; The year is an integer, the month is an integer from 1 to 12, and the day is an integer from 1 to 31.
        ; The date structure is defined as follows:
        (struct gdate (year month day) #:inspector #f)

        ; Format a date as a string.
        (define (format-date date)
          (define month (gdate-month date))
          (define day (gdate-day date))
          (string-append (number->string (gdate-year date)) "-"
                         (if (< month 10) "0" "") (number->string month) "-"
                         (if (< day 10) "0" "") (number->string day)))

        (module+ test
                 (require rackunit)
                 (check-equal? (format-date (gdate 2023 1 1)) "2023-01-01")
                 (check-equal? (format-date (gdate 2023 12 31)) "2023-12-31")
                 (check-equal? (format-date (gdate 2000 2 29)) "2000-02-29"))

        ; Compare the given dates and return true if the first date is earlier.
        (define (gdate< date1 date2)
          (define year1 (gdate-year date1))
          (define month1 (gdate-month date1))
          (define day1 (gdate-day date1))
          (define year2 (gdate-year date2))
          (define month2 (gdate-month date2))
          (define day2 (gdate-day date2))
          (cond ((< year1 year2) #t)
                ((and (= year1 year2) (< month1 month2)) #t)
                ((and (= year1 year2) (= month1 month2) (< day1 day2)) #t)
                (else #f)))

        (module+ test
                 (require rackunit)
                 (check-true (gdate< (gdate 2000 1 1) (gdate 2000 1 2)))
                 (check-false (gdate< (gdate 2000 1 2) (gdate 2000 1 1)))
                 (check-true (gdate< (gdate 2000 1 1) (gdate 2000 2 1)))
                 (check-false (gdate< (gdate 2000 2 1) (gdate 2000 1 1)))
                 (check-true (gdate< (gdate 2000 1 1) (gdate 2001 1 1)))
                 (check-false (gdate< (gdate 2001 1 1) (gdate 2000 1 1))))

        ; Compare the given dates and return true if the first date is later or equal.
        (define (gdate>= date1 date2)
          (not (gdate< date1 date2)))
        (module+ test
                 (require rackunit)
                 (check-true (gdate>= (gdate 2000 1 1) (gdate 2000 1 1)))
                 (check-true (gdate>= (gdate 2000 1 2) (gdate 2000 1 1)))
                 (check-false (gdate>= (gdate 2000 1 1) (gdate 2000 1 2)))
                 (check-true (gdate>= (gdate 2000 2 1) (gdate 2000 1 1)))
                 (check-false (gdate>= (gdate 2000 1 1) (gdate 2000 2 1)))
                 (check-true (gdate>= (gdate 2001 1 1) (gdate 2000 1 1)))
                 (check-false (gdate>= (gdate 2000 1 1) (gdate 2001 1 1))))

        ; Compare the given dates and return true if they are equal.
        (define (gdate= date1 date2)
          (and (= (gdate-year date1) (gdate-year date2))
               (= (gdate-month date1) (gdate-month date2))
               (= (gdate-day date1) (gdate-day date2))))

        (module+ test
                 (require rackunit)
                 (check-true (gdate= (gdate 2000 1 1) (gdate 2000 1 1)))
                 (check-false (gdate= (gdate 2000 1 2) (gdate 2000 1 1)))
                 (check-false (gdate= (gdate 2000 1 1) (gdate 2000 2 1)))
                 (check-false (gdate= (gdate 2000 1 1) (gdate 2001 1 1))))
        
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

        ; Define short names for the days of the week.
        (define days-of-week-short
          (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

        ; Return the given day of week as a string.
        ; The days of the week are defined as numbers from 0 (Sunday) to 6 (Saturday).
        ; The function checks if the given day is valid and returns the corresponding string.
        ; If the day is invalid, an error is raised.
        (define (day-of-week-string dow)
          (if (or (< dow sunday) (> dow saturday))
              (error "Invalid day of week")
              ; Use a cond statement to return the corresponding string.
              (list-ref days-of-week dow)))

        (module+ test
                 (require rackunit)
                 (check-equal? (day-of-week-string sunday) "Sunday")
                 (check-equal? (day-of-week-string monday) "Monday")
                 (check-equal? (day-of-week-string tuesday) "Tuesday")
                 (check-equal? (day-of-week-string wednesday) "Wednesday")
                 (check-equal? (day-of-week-string thursday) "Thursday")
                 (check-equal? (day-of-week-string friday) "Friday")
                 (check-equal? (day-of-week-string saturday) "Saturday"))

        ; Return the given day of week as a short string.
        ; The days of the week are defined as numbers from 0 (Sun) to 6 (Sat).
        ; The function checks if the given day is valid and returns the corresponding string.
        ; If the day is invalid, an error is raised.
        (define (day-of-week-short-string dow)
          (if (or (< dow sunday) (> dow saturday))
              (error "Invalid day of week")
              ; Use a cond statement to return the corresponding string.
              (list-ref days-of-week-short dow)))

        (module+ test
                 (require rackunit)
                 (check-equal? (day-of-week-short-string sunday) "Sun")
                 (check-equal? (day-of-week-short-string monday) "Mon")
                 (check-equal? (day-of-week-short-string tuesday) "Tue")
                 (check-equal? (day-of-week-short-string wednesday) "Wed")
                 (check-equal? (day-of-week-short-string thursday) "Thu")
                 (check-equal? (day-of-week-short-string friday) "Fri")
                 (check-equal? (day-of-week-short-string saturday) "Sat"))

        ; Determine the day of week for a given date.
        ; https://de.wikipedia.org/wiki/Wochentagsberechnung
        ; The day of the week is calculated as a number from 0 (Sunday) to 6 (Saturday).
        ; The formula is based on Zeller's Congruence.

        (define (day-of-week date)
          ; Adjust month and year for Zeller's algorithm 
          (define m-julian (if (< (gdate-month date) 3) (+ (gdate-month date) 10) (- (gdate-month date) 2)))
          (define year-fixed (if (< (gdate-month date) 3) (- (gdate-year date) 1) (gdate-year date)))
          (define y (modulo year-fixed 100))
          (define c (quotient year-fixed 100))
          (define wd (modulo (- (+ (gdate-day date) (floor (- (* 13/5 m-julian) 1/5)) y (quotient y 4) (quotient c 4)) (* 2 c)) 7))  
          wd)

        (module+ test
                 (require rackunit)
                 (check-equal? (day-of-week (gdate 1900 1 1)) monday)
                 (check-equal? (day-of-week (gdate 1900 2 4)) sunday)
                 (check-equal? (day-of-week (gdate 2006 6 4)) sunday)
                 (check-equal? (day-of-week (gdate 2006 6 12)) monday)
                 (check-equal? (day-of-week (gdate 2023 1 1)) sunday)
                 (check-equal? (day-of-week (gdate 2023 12 25)) monday)
                 (check-equal? (day-of-week (gdate 2023 7 4)) tuesday)
                 (check-equal? (day-of-week (gdate 2025 4 2)) wednesday))

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
        (define (gregorian-date-difference date1 date2)
          (define fixed1 (gregorian->fixed date1))
          (define fixed2 (gregorian->fixed date2))
          (- fixed2 fixed1))

        (module+ test
                 (require rackunit)
                 (check-equal? (gregorian-date-difference (gdate 2000 1 1) (gdate 2001 1 1)) 366)
                 (check-equal? (gregorian-date-difference (gdate 2000 1 1) (gdate 2002 1 1)) 731)
                 (check-equal? (gregorian-date-difference (gdate 2000 1 1) (gdate 2020 1 1)) 7305)
                 (check-equal? (gregorian-date-difference (gdate 2000 1 1) (gdate 2024 1 1)) 8766))

        ; Calculate the ordinal day number (R.D.) of a date within its year.
        ; The ordinal day number is the number of days since the beginning of the year.
        ; The first day of the year is 1, the second day is 2, and so on.
        (define (day-number-in-year date)
          (gregorian-date-difference (gdate (- (gdate-year date) 1) 12 31) date))

        (module+ test
                 (require rackunit)
                 (check-equal? (day-number-in-year (gdate 2000 1 1)) 1)
                 (check-equal? (day-number-in-year (gdate 2000 2 1)) 32)
                 (check-equal? (day-number-in-year (gdate 2000 3 1)) 61)
                 (check-equal? (day-number-in-year (gdate 2000 4 1)) 92)
                 (check-equal? (day-number-in-year (gdate 2000 5 1)) 122)
                 (check-equal? (day-number-in-year (gdate 2000 6 1)) 153)
                 (check-equal? (day-number-in-year (gdate 2000 7 1)) 183)
                 (check-equal? (day-number-in-year (gdate 2000 8 1)) 214)
                 (check-equal? (day-number-in-year (gdate 2000 9 1)) 245)
                 (check-equal? (day-number-in-year (gdate 2000 10 1)) 275)
                 (check-equal? (day-number-in-year (gdate 2000 11 1)) 306)
                 (check-equal? (day-number-in-year (gdate 2000 12 1)) 336)
                 (check-equal? (day-number-in-year (gdate 2001 1 1)) 1)      
                 (check-equal? (day-number-in-year (gdate 2001 3 1)) 60)        
                 (check-equal? (day-number-in-year (gdate 2001 3 13)) 72))

        ; Calculate the remaining days in the year from a given date.
        ; The remaining days are the difference between the given date and the last day of the year.
        ; The last day of the year is always 31 December.
        (define (days-remaining date)
          (gregorian-date-difference date (gdate (gdate-year date) 12 31)))

        (module+ test
                 (require rackunit)
                 (check-equal? (days-remaining (gdate 2000 1 1)) 365)
                 (check-equal? (days-remaining (gdate 2000 2 1)) 334)
                 (check-equal? (days-remaining (gdate 2000 3 1)) 305)
                 (check-equal? (days-remaining (gdate 2000 4 1)) 274)
                 (check-equal? (days-remaining (gdate 2000 5 1)) 244)
                 (check-equal? (days-remaining (gdate 2000 6 1)) 213)
                 (check-equal? (days-remaining (gdate 2000 7 1)) 183)
                 (check-equal? (days-remaining (gdate 2000 8 1)) 152)
                 (check-equal? (days-remaining (gdate 2000 9 1)) 121)
                 (check-equal? (days-remaining (gdate 2000 10 1)) 91)
                 (check-equal? (days-remaining (gdate 2000 11 1)) 60)
                 (check-equal? (days-remaining (gdate 2000 12 1)) 30))

        ; Determine the date a given number of days in the future.
        ; The date is calculated by adding the number of days to the given date.
        (define (gregorian-date-n-days-from-date date days)
          (define fixed-date (gregorian->fixed date))
          (define future-date (+ fixed-date days))
          (fixed->gregorian future-date))

        (module+ test
                 (require rackunit)
                 (check-equal? (gregorian-date-n-days-from-date (gdate 2000 1 1) 1) (gdate 2000 1 2))
                 (check-equal? (gregorian-date-n-days-from-date (gdate 2000 1 1) 30) (gdate 2000 1 31))
                 (check-equal? (gregorian-date-n-days-from-date (gdate 2000 1 1) 60) (gdate 2000 3 1))
                 (check-equal? (gregorian-date-n-days-from-date (gdate 2000 1 1) 365) (gdate 2000 12 31))
                 (check-equal? (gregorian-date-n-days-from-date (gdate 2000 1 1) -366) (gdate 1998 12 31)))

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
                 (check-equal? (day-of-week-from-fixed (gregorian->fixed (gdate 1900 2 4))) sunday))

        ; Determine the k-th day of the week on or before a given date.
        ; Where k is the day of the week (0 = Sunday, 1 = Monday, ..., 6 = Saturday).
        ; Returns the day number of the k-th day of the week on or before the given date.
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
                 (check-equal? (kday-on-or-before (gregorian->fixed (gdate 1900 2 4)) wednesday) (gregorian->fixed (gdate 1900 1 31))))

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
                 (check-equal? (kday-on-or-after (gregorian->fixed (gdate 1900 2 4)) wednesday) (gregorian->fixed (gdate 1900 2 7))))

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
                 (check-equal? (kday-before (gregorian->fixed (gdate 1900 2 4)) wednesday) (gregorian->fixed (gdate 1900 1 31))))

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
                 (check-equal? (kday-after (gregorian->fixed (gdate 1900 2 4)) wednesday) (gregorian->fixed (gdate 1900 2 7))))

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
                 (check-equal? (kday-nearest (gregorian->fixed (gdate 1900 2 4)) wednesday) (gregorian->fixed (gdate 1900 2 7))))

        ;; ----- daylight saving time -----

        ; The start of DST in Germany for a given year.
        ; Returns the Gregorian date.
        (define (dst-start-de year)
          (fixed->gregorian
            (kday-before (gregorian->fixed (gdate year 4 1)) sunday)))

        (module+ test
                 (check-equal? (dst-start-de 2025) (gdate 2025 3 30))
                 (check-equal? (dst-start-de 2024) (gdate 2024 3 31)))

        ; The end of DST in Germany for a given year.
        ; Returns the Gregorian date.
        (define (dst-end-de year)
          (fixed->gregorian
            (kday-before (gregorian->fixed (gdate year 11 1)) sunday)))
        
        (module+ test
                 (check-equal? (dst-end-de 2025) (gdate 2025 10 26))
                 (check-equal? (dst-end-de 2024) (gdate 2024 10 27)))

        ; The start of DST in the USA for a given year.
        ; Returns the Gregorian date.
        (define (dst-start-us year)
          (fixed->gregorian
            (kday-on-or-after (gregorian->fixed (gdate year 4 1)) sunday)))

        (module+ test
                 (check-equal? (dst-start-us 2025) (gdate 2025 4 6))
                 (check-equal? (dst-start-us 2024) (gdate 2024 4 7)))

        ; The end of DST in the USA for a given year.
        ; Returns the Gregorian date.
        (define (dst-end-us year)
          (fixed->gregorian
            (kday-before (gregorian->fixed (gdate year 11 1)) sunday)))
        
        (module+ test
                 (check-equal? (dst-end-us 2025) (gdate 2025 10 26))
                 (check-equal? (dst-end-us 2024) (gdate 2024 10 27)))


        ;; ----- conversions -----

        ; Convert a Gregorian date to the fixed number of days since the Gregorian epoch.
        ; Calendrical Calculations pg. 51
        ; The fixed day number is the number of days since the Gregorian epoch.
        (define (gregorian->fixed date)
          (define year (gdate-year date))
          (define month (gdate-month date))
          (define day (gdate-day date))
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
                 (check-equal? (gregorian->fixed (gdate 1 1 1)) 1)
                 (check-equal? (gregorian->fixed (gdate 1945 11 12)) 710347)
                 (check-equal? (gregorian->fixed (gdate 1958 10 29)) 715081)
                 (check-equal? (gregorian->fixed (gdate 2000 1 1)) 730120)
                 (check-equal? (gregorian->fixed (gdate 2025 4 15)) 739356))

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
          (define prior-days (- day-number (gregorian->fixed (gdate year 1 1))))
          (define correction (cond ((< day-number (gregorian->fixed (gdate year 3 1))) 0)
                                   ((and (leap-year? year) (>= day-number (gregorian->fixed (gdate year 3 1)))) 1)
                                   (else 2)))
          (define month (quotient (+ (* 12 (+ prior-days correction)) 373) 367))
          (define day (+ (- day-number (gregorian->fixed (gdate year month 1))) 1))
          (gdate year month day))

        (module+ test
                 (require rackunit)
                 ; See https://www.vcalc.com/equation/?uuid=9e167c54-77aa-11e5-a3bb-bc764e2038f2 for an RD calculator to get test data.
                 (check-equal? (fixed->gregorian 1) (gdate 1 1 1))
                 (check-equal? (fixed->gregorian 710347) (gdate 1945 11 12))
                 (check-equal? (fixed->gregorian 715081) (gdate 1958 10 29))
                 (check-equal? (fixed->gregorian 730120) (gdate 2000 1 1))
                 (check-equal? (fixed->gregorian 739356) (gdate 2025 4 15)))

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
          (gdate year month day))

        (module+ test
                 (require rackunit)
                 (check-equal? (easter-sunday 2000) (gdate 2000 4 23))
                 (check-equal? (easter-sunday 2021) (gdate 2021 4 4))
                 (check-equal? (easter-sunday 2022) (gdate 2022 4 17))
                 (check-equal? (easter-sunday 2023) (gdate 2023 4 9))
                 (check-equal? (easter-sunday 2024) (gdate 2024 3 31))
                 (check-equal? (easter-sunday 2025) (gdate 2025 4 20))
                 (check-equal? (easter-sunday 2026) (gdate 2026 4 5)))

        ; Determine the good friday date for a given year.
        ; Good Friday is the Friday before Easter Sunday.
        (define (good-friday year)
          (fixed->gregorian
           (- (gregorian->fixed (easter-sunday year)) 2))) ; Subtract 2 days from Easter Sunday

        (module+ test
                 (require rackunit)
                 (check-equal? (good-friday 2000) (gdate 2000 4 21))
                 (check-equal? (good-friday 2024) (gdate 2024 3 29))
                 (check-equal? (good-friday 2025) (gdate 2025 4 18))
                 (check-equal? (good-friday 2026) (gdate 2026 4 3)))

        ; Determine the easter monday date for a given year.
        ; Easter Monday is the Monday after Easter Sunday.
        (define (easter-monday year)
          (fixed->gregorian
           (+ (gregorian->fixed (easter-sunday year)) 1))) ; Add 1 days to Easter Sunday

        (module+ test
                 (require rackunit)
                 (check-equal? (easter-monday 2000) (gdate 2000 4 24))
                 (check-equal? (easter-monday 2024) (gdate 2024 4 1))
                 (check-equal? (easter-monday 2025) (gdate 2025 4 21))
                 (check-equal? (easter-monday 2026) (gdate 2026 4 6)))

        ; Determine the date of palm sunday for a given year.
        ; Palm Sunday is the Sunday before Easter Sunday.
        (define (palm-sunday year)
          (fixed->gregorian
           (- (gregorian->fixed (easter-sunday year)) 7))) ; Subtract 7 days from Easter Sunday

        (module+ test
                 (require rackunit)
                 (check-equal? (palm-sunday 2000) (gdate 2000 4 16))
                 (check-equal? (palm-sunday 2024) (gdate 2024 3 24))
                 (check-equal? (palm-sunday 2025) (gdate 2025 4 13))
                 (check-equal? (palm-sunday 2026) (gdate 2026 3 29)))

        ; Determine the date of whit sunday for a given year.
        ; Whit Sunday is the 7th Sunday after Easter Sunday.
        (define (whit-sunday year)
          (fixed->gregorian
           (+ (gregorian->fixed (easter-sunday year)) 49))) ; Add 49 days to Easter Sunday

        (module+ test
                 (require rackunit)
                 (check-equal? (whit-sunday 2000) (gdate 2000 6 11))
                 (check-equal? (whit-sunday 2024) (gdate 2024 5 19))
                 (check-equal? (whit-sunday 2025) (gdate 2025 6 8))
                 (check-equal? (whit-sunday 2026) (gdate 2026 5 24)))

        ; Determine the date of whit monday for a given year.
        ; Whit Monday is the day after Whit Sunday.
        (define (whit-monday year)
          (fixed->gregorian
           (+ (gregorian->fixed (whit-sunday year)) 1))) ; Add 1 day to Whit Sunday

        (module+ test
                 (require rackunit)
                 (check-equal? (whit-monday 2000) (gdate 2000 6 12))
                 (check-equal? (whit-monday 2024) (gdate 2024 5 20))
                 (check-equal? (whit-monday 2025) (gdate 2025 6 9))
                 (check-equal? (whit-monday 2026) (gdate 2026 5 25)))

        ; Determine the date of the advent days for a given year.
        ; The function takes the integers 1 to 4 for the 1st to the 4th advent.
        (define (advent year n)
          (fixed->gregorian
            (kday-on-or-before (- (gregorian->fixed (gdate year 12 24)) (* 7 (- 4 n))) sunday)))

        (module+ test
                 (require rackunit)
                 (check-equal? (advent 2024 1) (gdate 2024 12 1))
                 (check-equal? (advent 2024 2) (gdate 2024 12 8))
                 (check-equal? (advent 2024 3) (gdate 2024 12 15))
                 (check-equal? (advent 2024 4) (gdate 2024 12 22))
                 (check-equal? (advent 2025 1) (gdate 2025 11 30))
                 (check-equal? (advent 2025 2) (gdate 2025 12 7))
                 (check-equal? (advent 2025 3) (gdate 2025 12 14))
                 (check-equal? (advent 2025 4) (gdate 2025 12 21)))

        ) ; end module
