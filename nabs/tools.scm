(define-module (nabs tools)
               :use-module (ice-9 readline)
               :export (print
                        concat
                        pick-unique
                        in-list?
                         ))


(define (print . args) (for-each display args))
(define (concat . args) (string-concatenate args))


(define (print-alternatives index alternatives)
  (cond ((not (null? alternatives))
         (begin (print "[" index "] " (car alternatives) "\n")
                (print-alternatives (+ 1 index) (cdr alternatives))))
        (#t index)))

(define (pick index alternatives) (if (< index 2) (car alternatives) (pick (- index 1) (cdr alternatives))))

(define (user-choice what alternatives)
  (let* ((header (print "Multiple versions of " what " found.\n"))
         (count (print-alternatives 1 alternatives))
         (input (readline (concat "Which one do you want to use? ")))
         (value (if input (string->number input) 0)))
    (if (or (< value 1) (> value count))
      (user-choice what alternatives)
      (pick value alternatives))))

(define (pick-unique what alternatives predicates)
  (cond ((null? alternatives)
         (pick-unique ((user-pick what
                                  (filter predicates
                                          (list (readline (concat "Couldn't find a suitable version of "
                                                                  what ".\n Please input the path to use: "))))
                                  predicates))))
        ((null? (cdr alternatives)) (car alternatives))
        (#t (user-choice what alternatives))))

(define (in-list? needle haystack) (cond ((null? haystack) #f) ((equal? needle (car haystack)) #t) (#t (in-list? needle (cdr haystack)))))

