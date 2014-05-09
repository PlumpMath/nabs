(define-module (nabs configure)
               :use-module (nabs tools)
               :use-module (nabs find)
               :use-module (nabs predicates)
               :use-module (ice-9 match)
               :use-module (srfi srfi-1)
               :use-module (gnu make)
               :re-export (checks)
               :re-export (find-file-by-prefix)
               :re-export (find-file-by-path-suffix)
               :re-export (find-file-by-exact-name)
               :export (declare
                        configure-include
                        configure-frontend
                        write-frontend
                        ))

(define configurables '())
(define-public (dump-configurables) configurables)

(define (declare name make-var-name search-path find-mode-tag find-target predicates)
  (set! configurables
        (append configurables
                (list (list make-var-name
                            (lambda () (pick-unique name
                                                    (filter predicates
                                                            (find-file search-path
                                                                       find-mode-tag
                                                                       find-target))
                                                    predicates)))))))
(define (eval-configurables)
  (string-concatenate
    (map (lambda (p) (concat (car p) "=" ((cadr p)) "\n"))
         configurables)))


(define (write-configuration filename)
  (let ((f (open-output-file filename)))
    (write f (eval-configurables))
    (close f)))

(define (gmk-var var-name)
  (gmk-expand (concat "$(" var-name ")")))

(define (configure-include)
  (gmk-eval "
.configuration:
\t$(guile (write-configuration \".configuration\"))

include .configuration
"))

(define (configure-frontend)
  (let* ((makefiles (gmk-var "MAKEFILE_LIST"))
         (project-path (string-join (map dirname (string-split makefiles #\ )) ":" 'infix)))
    ;(print "makefiles " makefiles "\n")
    ;(print "project-path " project-path "\n")
    (gmk-eval (concat "
.configure:
\t$(guile (write-frontend \"Makefile\" \"" project-path "\" '(" makefiles ")))

"))))

(define (write-frontend filename vpath makefiles)
  (let ((f (open-output-file filename)))
    (display (eval-configurables) f)
    ;(print "vpath " vpath " vpath " vpath "\n")
    (display (concat "vpath % " vpath "\n") f)
    (display (string-join (map (lambda (m) (concat "include " (symbol->string m))) makefiles) "\n" 'infix) f)
    (close f)))

