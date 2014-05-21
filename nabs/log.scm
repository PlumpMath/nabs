(define-module (nabs log)
               )

(define log-port *unspecified*)

(define-public (open-log)
  (if (equal? *unspecified* log-port)
    (begin
      ;(display "Start logging in nabs.log")
      ;(newline)
      (set! log-port (open-output-file "nabs.log")))))

(define-public (close-log)
  (if (not (equal? *unspecified* log-port))
    (begin
      ;(display "Stop logging in nabs.log")
      ;(newline)
      (close log-port)
      (set! log-port *unspecified*))))

(define-public (log-write x)
  (if (not (or (equal? *unspecified* log-port) (equal? x "")))
    (begin
      (display x log-port)
      (newline log-port)
      x)
    x))

