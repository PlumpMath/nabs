(define-module (nabs invoke)
               :use-module (nabs tools)
               :use-module (ice-9 popen)
               :use-module (ice-9 rdelim)
               :use-module (ice-9 regex)
               :use-module (rnrs io ports)
               :use-module (nabs log)
               :export (match-one-line?
                        match-one-line-by-re?
                        match-one-line-by-substring?
                        match-one-line-by-prefix?
                        read-lines
                        read-all
                        command-output-and-status
                        command-output
                        command-status
                        command-success?)
               )


;; MATCH OUTPUT OF COMMANDS

(define (match-one-line? port predicate?)
  (let ((l (read-line port)))
    (cond ((eof-object? l) #f)
          ((predicate? l) #t)
          (#t (match-one-line? port predicate?)))))

(define (match-one-line-by-re? port regex) (match-one-line? port (lambda (l) (regexp-exec regex l))))
(define (match-one-line-by-substring? port str) (match-one-line? port (lambda (l) (string-contains l str))))
(define (match-one-line-by-prefix? port str) (match-one-line? port (lambda (l) (string-prefix? str l))))
(define (append-suffix name suffix) (concat name suffix))

;; GENERIC INVOCATION ROUTINES

(define (read-all-rec port)
  (let ((l (read-line port)))
    (if (eof-object? l)
      '()
      (cons l (read-all-rec port)))))

(define (read-all port)
  (string-join (read-all-rec port) "\n"))
        

(define (read-lines port)
  (let ((l (read-line port)))
    (if (eof-object? l)
    '()
    (begin 
      ;(print l "\n")
      (cons l (read-lines port))))))

(define (command-output-and-status command input)
  (let* ((input-port (mkstemp! (string-copy "guile-temp-file-XXXXXX")))
         (input-filename (port-filename input-port))
         (error-port (mkstemp! (string-copy "guile-temp-file-XXXXXX")))
         (error-filename (port-filename error-port))
         (!write-input (display input input-port))
         (!flush-input (force-output input-port))
         (close-input (close-port input-port))
         (full-command (concat command " < " input-filename " 2> " error-filename))
         (port (open-input-pipe full-command))
         (lines (read-lines port))
         (status (status:exit-val (close-pipe port)))
         (error-buf (read-all error-port))
         (close-error (close-port error-port))
         (clean-up1 (delete-file input-filename))
         (clean-up2 (delete-file error-filename)))
    (log-write (concat "[nabs] Invoking command " command))
    (log-write "----------input-----------")
    (log-write input)
    (log-write "----------status----------")
    (log-write status)
    (log-write "----------output----------")
    (log-write (string-join lines "\n" 'infix))
    (log-write "----------error-----------")
    (log-write error-buf)
    (log-write "--------------------------")
    (list lines status error-buf)))

(define (command-output command input) (car (command-output-and-status command input)))

(define (command-status command input) (cadr (command-output-and-status command input)))

(define (command-success? command input) (equal? (command-status command input) 0))

