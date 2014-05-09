(define-module (nabs configure)
               :use-module (nabs tools)
               :use-module (nabs find)
               :use-module (nabs predicates)
               :use-module (ice-9 match)
               :use-module (srfi srfi-1)
               :re-export (checks)
               :re-export (find-file-by-prefix)
               :re-export (find-file-by-path-suffix)
               :re-export (find-file-by-exact-name)
               :export (declare
                        configure
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
                                                                       find-target)))))))))

(define (configure)
  (string-concatenate
    (map (lambda (p) (concat (car p) "=" ((cadr p)) "\n"))
         configurables)))
