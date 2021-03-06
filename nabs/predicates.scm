(define-module (nabs predicates)
               :use-module (ice-9 match)
               :use-module (nabs tools)
               :use-module (nabs invoke)
               )

;(define (expand-checks ARGS)
;  (if (null? ARGS)
;    '()
;    (let ((ARG (car ARGS)))
;      (cons (match ARG ((op args ...) (cons* op 'query args)))
;            (expand-checks (cdr ARGS))))))
;
;(define (checks . ARGS)
;  (let ((expr (cons* 'and (expand-checks ARGS))))
;    ;(print expr "\n")
;    (primitive-eval (list 'lambda '(query) expr))))
;


(define (split-version-string version) (map string->number (string-split version #\.)))

(define (rec-supeq l1 l2)
  (cond ((null? l1) (null? l2))
        ((null? l2) #t)
        ((not (car l1)) #f)
        ((not (car l2)) #t)
        ((> (car l1) (car l2)) #t)
        ((= (car l1) (car l2)) (rec-supeq (cdr l1) (cdr l2)))
        (#t #f)))

(define (version>=? version1 version2)
  (let ((v1 (split-version-string version1))
        (v2 (split-version-string version2)))
    (rec-supeq v1 v2)))


(define-public (compiler-version>=? program-name version)
  (let* ((command (concat program-name " -dumpversion"))
         ;(dump-cmd (print "command: " command "\n"))
         (output (command-output command ""))
         ;(dump (print "command output: " output "\n"))
         )
    (if (null? output)
      #f
      (version>=? (car output) version))))

(define-public (compiles? program-name options static-body main-body)
  (let* ((command (concat program-name " " options " -o .test.compilation -"))
         (input (concat static-body "\n\nint main(int argc, char** argv)\n{\n"
                        main-body ";\n    return 0;\n}\n"))
         (ret (command-success? command input)))
    (if ret (delete-file ".test.compilation"))
    ret))

