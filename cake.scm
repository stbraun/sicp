(module cake racket
        (provide print-cake)

        (define (print-cake n)
          (show "   ~a   " n #\.)
          (show " .-~a-. " n #\|)
          (show " | ~a | " n #\space)
          (show "---~a---" n #\-))

        (define (show fmt n ch)
          (printf fmt (make-string n ch))
          (newline))

        (module+ test
                 (print-cake 5))

        (module* main #f
                 (print-cake 10)))
