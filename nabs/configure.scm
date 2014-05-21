(define-module (nabs configure)
               :use-module (nabs tools)
               :use-module (nabs find)
               :use-module (nabs predicates)
               :use-module (nabs targets)
               ;:use-module (ice-9 match)
               :use-module (ice-9 vlist)
               :use-module (srfi srfi-1)
               :use-module (gnu make)
               ;:re-export (checks)
               :re-export (find-file-by-prefix)
               :re-export (find-file-by-path-suffix)
               :re-export (find-file-by-exact-name)
               ;:export-syntax (add-search-path declare verifies)
               :export (~add-search-path
                        configure-include
                        configure-frontend
                        write-frontend
                        add-search-path declare verifies
                        ))

; export predicates
;(module-for-each (lambda (k v) (print k " --> " v "\n")(re-export (k v))) (resolve-interface '(nabs predicates)))

(define configurables '())
(define-public (dump-configurables) configurables)

(define ~search-path vlist-null)
(define-public (dump-search-path) (vhash-fold-right (lambda (k v r) (cons (list k v) r)) '() ~search-path))

(define (~add-search-path tag . path)
  (let (;(flat (append-list path))
        (exists? (vhash-assoc tag ~search-path)))
    ;(print "path " path " exists? " exists? "\n")
    (set! ~search-path
      (if exists?
        (vhash-fold-right (lambda (k v r)
                            (if (equal? k tag)
                              (vhash-cons k (append v path) r))) vlist-null ~search-path)
        (vhash-cons tag path ~search-path)))
    ))

(define-syntax add-search-path
  (syntax-rules ()
    ((_ key path ...)
     (~add-search-path
         (if (string? (quote key)) (string->symbol (quote key)) (quote key))
         (symbol->string (quote path)) ...))))


(define (get-search-path tag)
  (let ((exists? (vhash-assoc tag ~search-path)))
    (if exists?
      (cdr exists?)
      (begin (print (dump-search-path) "\n")
      (error (concat "Search path for '" (symbol->string tag) " was not defined!"))))))

(define (find-candidates search-path find-mode-tag find-target predicates . other)
  (filter predicates (find-file search-path find-mode-tag find-target)))

(define-syntax verifies
  (syntax-rules ()
    ((verifies (pred-name . args) ...) (begin
                                         (lambda (f) (and (pred-name f . args) ...))
                                         ))))

(define-syntax format-find-candidates
  (syntax-rules ()
    ((format-find-candidates search-path find-mode-tag find-target predicates)
     (find-candidates search-path (quote find-mode-tag) (symbol->string (quote find-target)) predicates))))

(define-syntax format-find-multiple-candidates-impl
  (syntax-rules ()
    ((_ sp fmt ft p) (format-find-candidates sp fmt ft p))
    ((_ sp fmt ft p . rest)
     (append (format-find-candidates sp fmt ft p) (format-find-multiple-candidates-impl sp . rest)))))

(define-syntax format-find-multiple-candidates
  (syntax-rules ()
    ((_ . whatever) (lambda () (format-find-multiple-candidates-impl . whatever)))))

(define (declare-impl tag name make-var-name candidates)
  (set! configurables
        (append configurables
                (list (list make-var-name
                            (lambda ()
                              (let ((value (pick-unique name (candidates))))
                                (print "[CONF] Variable " make-var-name " set to \"" value "\"\n")
                                (pick-unique name (candidates)))))))))

(define-syntax declare
  (syntax-rules ()
    ((declare t n mvn . rest)
     (declare-impl (quote t) n (symbol->string (quote mvn))
                   (format-find-multiple-candidates (get-search-path (quote t)) . rest)))))


;; RENDER CONFIGURATION

(define (eval-configurables)
  "Turn the list of configurables into a string that can be directly fed to Make."
  (string-concatenate
    (map (lambda (p) (concat (car p) "=" ((cadr p)) "\n"))
         configurables)))

(define-public (write-configuration filename)
  (let ((f (open-output-file filename)))
	(display "Configuring the build...\n")
    (display (eval-configurables) f)
    (close f)
    (concat "Configuration written in " filename)))

(define (gmk-var var-name)
  (gmk-expand (concat "$(" var-name ")")))

(define (configure-include)
  (gmk-eval "
NABS_MODE_EMBED:=1
.build-configuration: $(MAKEFILE_LIST)
	@echo $(guile (write-configuration \".build-configuration\"))

-include .build-configuration

reconfigure:
	rm .build-configuration && $(MAKE)
"))

(define (configure-frontend)
  (let* ((makefiles (gmk-var "MAKEFILE_LIST"))
         (project-path (string-join (map dirname (string-split makefiles #\ )) ":" 'infix)))
    ;(print "makefiles " makefiles "\n")
    ;(print "project-path " project-path "\n")
    (gmk-eval (concat "
NABS_MODE_FRONTEND:=1
configure:
	@$(guile (write-frontend \"Makefile\" \"" project-path "\" '(" makefiles ")))

"))))

(define (write-frontend filename vpath makefiles)
;  (if (false-if-exception
        (let ((f (open-output-file filename)))
          (display (eval-configurables) f)
          ;(print "vpath " vpath " vpath " vpath "\n")
          (display (concat "vpath % " vpath "\n") f)
          (display (string-join (map (lambda (m) (concat "include " (symbol->string m))) makefiles) "\n" 'infix) f)
          (close f)))
;    "true"
;    "false"))


(gmk-eval "
NABS_MODE_EMBED:=0
NABS_MODE_FRONTEND:=0
nabs-help:
	@echo This Makefile is powered by nabs. Nabs: not a build system.
	@export PFX='$$'
	@test $(NABS_MODE_EMBED) -ne 0 -o $(NABS_MODE_FRONTEND) -ne 0 || (echo \" * This Makefile does not yet define a build mode.\\n   You should invoke either ${PFX}(guile (configure-frontend)) or ${PFX}(guile (configure-include)) in the Makefile.\" && false)
	@test $(NABS_MODE_EMBED) -eq 1 && echo \" * The build has automatic configuration. You don't need to do anything manually.\\n   You can reconfigure the build by invoking '$(MAKE) reconfigure'.\" || true
	@test $(NABS_MODE_FRONTEND) -eq 1 && echo \" * Out-of-source build capability.\\n   Invoke '$(MAKE) -f <relative-path-to-this-makefile> configure' in another directory to set up the build there.\" || true
	@echo \" * Top-level targets: $(guile (top-level-targets))\"
")

