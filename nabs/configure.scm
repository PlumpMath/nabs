(define-module (nabs configure)
               :use-module (nabs tools)
               :use-module (nabs find)
               :use-module (nabs predicates)
               :use-module (nabs targets)
               ;:use-module (ice-9 match)
               :use-module (ice-9 vlist)
               :use-module (srfi srfi-1)
               :use-module (gnu make)
               ;:use-module (ice-9 session)
               ;:re-export (checks)
               :re-export (find-file-by-prefix)
               :re-export (find-file-by-path-suffix)
               :re-export (find-file-by-exact-name)
               ;:re-export (help)
               ;:export-syntax (add-search-path declare verifies)
               :export (~add-search-path
                        configure-include
                        configure-frontend
                        write-frontend
                        add-search-path declare verifies
                        nabs-help
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
                                (print "[nabs] * Variable " make-var-name " set to \"" value "\"\n")
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
	(display "[nabs] Configuring the build...\n")
    (display (eval-configurables) f)
    (close f)
    (concat "[nabs] Configuration written in " filename)))

(define (gmk-var var-name)
  (gmk-expand (concat "$(" var-name ")")))

(define (configure-include)
  (gmk-eval "
NABS_MODE_EMBED:=1
.build-configuration: $(MAKEFILE_LIST)
	@echo $(guile (write-configuration \".build-configuration\"))

-include .build-configuration

reconfigure:
	@rm .build-configuration && $(MAKE) -s .build-configuration
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


(define (debug str)
  (display str)
  (newline)
  str)

(gmk-eval "

NABS_MODE_EMBED:=0
NABS_MODE_FRONTEND:=0
about:
	@(echo This Makefile is powered by nabs. Nabs: not a build system. ;\
    echo;\
	test $(NABS_MODE_EMBED) -eq 1 && echo \"The build has automatic configuration. You don't need to do anything manually.\\n   You can reconfigure the build by invoking '$(MAKE) reconfigure'.\" || (\
	test $(NABS_MODE_FRONTEND) -eq 1 && echo \" * Out-of-source build capability.\\n   Invoke '$(MAKE) -f <relative-path-to-this-makefile> configure' in another directory to set up the build there.\" || (\
	echo \" * This Makefile does not yet define a build mode. You should use one of those commands in your Makefile :\" ;\
	echo \"   (configure-include) to enable automatic configuration\" ;\
	echo \"   (configure-frontend) to enable out-of-source build\" ;\
	));\
    echo;\
	echo \"Top-level targets: $(guile (top-level-targets))\" ;\
    echo;\
	echo \"You may use $(MAKE) help-TOPIC to get help on this topic.\" ;\
	)

help: help-nabs


help-%::;@#$(guile (begin (display \"### Nabs help ###\") (newline) (newline) (display (nabs-help (quote $@)))))


")

(define (nabs-help x)
  (case x
    ('help-nabs
     "NABS: Not a build system.

  Nabs is a guile module to help create configurable Makefiles, with optional out-of-source build, while still retaining full control over the power of GNU Make.
  Help is available for the following topics:
  - nabs
  - add-search-path
  - declare
  - configure-include
  - configure-frontend
  - predicates
")

    ('help-add-search-path
     "(add-search-path key path...)

  Add [path...] to the list of directories that will be searched for targets of type [key].
  Example:
    (add-search-path bin /usr/bin /usr/local/bin /home/memyselfandi/bin)
")

    ('help-declare
     "(declare key \"Descriptive name\" MAKE_VAR_NAME
         search-mode what (verifies ...) ...)

  Declares a Make variable to be configured.
  Nabs will search for [what] in the search path corresponding to [key].
  [search-mode] can be one of:
    - prefix        a candidate is selected if the base name starts with [what]
                    (for example, 'prefix gcc' will match gcc-4.9)
    - path-suffix   a candidate is selected if the full path ends with [what]
                    (for example 'path-suffix foo/bar' will match /search/path/foo/bar)
    - exact-name    a candidate is selected if the base name is exactly [what].
  The list of found files is filtered with the list of predicates defined in (verifies ...) (see help-predicates).
  If there is more than one viable candidate, the user is asked to choose one.
  If there is no viable candidate, the user is asked to input one.
")

    ('help-configure-include
     "(configure-include)

  Enables the auto-configuration of the Makefile.
  To this effect, a file named .build-configuration is included and the rule to create it is added to the Make data base.
  Upon creation of this file, all declared Make variables (see help-declare) are configured and saved in the configuration file.
  Reconfiguration can be performed by invoking make reconfigure.
")

    ('help-configure-frontend
     (gmk-expand "(configure-frontend)

  Enables out-of-source builds.
  To configure an out-of-source build, move to another directory (preferably empty and dedicated to the build), and invoke:
  $(MAKE) -f relative/path/to/Makefile configure
  A Makefile will be created in this directory, including all the configured Make variables.
  Just invoke $(MAKE) again in this directory to perform the build.

  Example:
  $$ mkdir build
  $$ cd build
  $$ make -f ../Makefile configure
  $$ make
"))

    ('help-predicates
     "(verifies ...)

  Specified a chain of predicates for use in declare (see help-declare).
  The existing predicates are:
  - (compiler-version>=? \"VERSION.STRING\")
    example: (compiler-version>=? \"4.4\")
")
    (else (concat (nabs-help 'help-nabs) "\n\nNo topic found for " (symbol->string x)))
))

;(add-name-help-handler! nabs-help)
