(define-module (nabs find)
               :use-module (nabs tools)
               :use-module (ice-9 ftw)
               :export (find-file-by-path-suffix
                        ;find-file-impl
                        find-file-by-exact-name
                        find-file-by-prefix
                        find-file)
               )

;; FIND FILES IN FILE SYSTEM TREE

(define (find-file-impl path file-predicate? enter-directory? init)
  (define (leaf name stat result)
    (let ((good (file-predicate? name stat)))
      (if good
        (cons good result)
        result)))
  (define (enter? name stat result) (enter-directory? name stat))
  (define (up name stat result) result)
  (define (skip name stat result) result)
  (define (down name stat result) result)
  ;; Ignore unreadable files/directories but warn the user.
  (define (error name stat errno result)
    (format (current-error-port) "warning: ~a: ~a~%"
            name (strerror errno))
    result)

  (cond ((string? path) (file-system-fold enter? leaf down up skip error init path))
        ((equal? path '()) init)
        ((list? path) (find-file-impl (car path) file-predicate? enter-directory?
                                 (find-file-impl (cdr path) file-predicate? enter-directory? init)))))

(define (find-file-by-prefix path prefix)
  (find-file-impl path
             (lambda (name statinfo) (if (string-prefix? prefix (basename name)) name #f))
             (lambda (name statinfo) #t)
             '()))

(define (find-file-by-path-suffix path suffix)
  (find-file-impl path
             (lambda (name statinfo) (if (string-suffix? suffix name) name #f))
             (lambda (name statinfo) #t)
             '()))

(define (find-file-by-exact-name path target-name)
  (find-file-impl path
             (lambda (name statinfo) (if (equal? target-name name) name #f))
             (lambda (name statinfo) #t)
             '()))

(define (find-file search-path mode-tag target)
  "Find files in any subdirectory of the given search path. search-path is a list of path names, mode-tag is one of 'prefix, 'path-suffix, or 'exact-name. target is the respectively the file name prefix, path name suffix, or exact file name."
  (if (null? search-path)
    '()
    (let ((finder (primitive-eval (symbol-append 'find-file-by- mode-tag))))
      (append (finder (car search-path) target)
              (find-file (cdr search-path) mode-tag target)))))
