;;; Guile Tree-IL extraction script for organ-bank.
;;; Compiles Scheme source to Tree-IL and emits OrganIR JSON.

(use-modules (system base compile)
             (language tree-il)
             (ice-9 rdelim)
             (ice-9 pretty-print))

(define (json-escape str)
  "Escape a string for JSON."
  (let ((out (open-output-string)))
    (string-for-each
     (lambda (ch)
       (case ch
         ((#\") (display "\\\"" out))
         ((#\\) (display "\\\\" out))
         ((#\newline) (display "\\n" out))
         ((#\tab) (display "\\t" out))
         ((#\return) (display "\\r" out))
         (else (display ch out))))
     str)
    (get-output-string out)))

(define (json-str s)
  (format #f "\"~a\"" (json-escape (format #f "~a" s))))

(define (tree-il->json tree)
  "Convert a Tree-IL node to a JSON string."
  (let ((s (format #f "~a" (unparse-tree-il tree))))
    (json-str s)))

(define (extract-definitions forms)
  "Extract top-level define forms and compile each to Tree-IL."
  (let ((defs '()))
    (for-each
     (lambda (form)
       (when (and (pair? form) (eq? (car form) 'define))
         (let* ((name-part (cadr form))
                (name (if (pair? name-part) (car name-part) name-part))
                (arity (if (pair? name-part) (length (cdr name-part)) 0)))
           (catch #t
             (lambda ()
               (let ((til (compile form #:from 'scheme #:to 'tree-il
                                   #:opts '(#:warnings ()))))
                 (set! defs (cons (list name arity til) defs))))
             (lambda (key . args)
               ;; If compilation fails, store the source as-is
               (set! defs (cons (list name arity form) defs)))))))
     forms)
    (reverse defs)))

(define (read-all-forms port)
  "Read all S-expressions from a port."
  (let loop ((forms '()))
    (let ((form (read port)))
      (if (eof-object? form)
          (reverse forms)
          (loop (cons form forms))))))

(define (emit-definition def stream first?)
  "Emit a single OrganIR definition."
  (let ((name (car def))
        (arity (cadr def))
        (tree (caddr def)))
    (unless first? (display ",\n" stream))
    (format stream "    {\n")
    (format stream "      \"name\": {\"module\": \"\", \"text\": ~a, \"unique\": 0},\n"
            (json-str name))
    (format stream "      \"type\": {\"tag\": \"any\"},\n")
    ;; Emit Tree-IL as the expression body
    (let ((body-str (if (tree-il? tree)
                        (format #f "~a" (unparse-tree-il tree))
                        (format #f "~a" tree))))
      (format stream "      \"expr\": {\"tag\": \"tree-il\", \"text\": ~a},\n"
              (json-str body-str)))
    (format stream "      \"sort\": \"fun\",\n")
    (format stream "      \"visibility\": \"public\",\n")
    (format stream "      \"arity\": ~a\n" arity)
    (format stream "    }")))

(define (main args)
  (when (< (length args) 2)
    (format (current-error-port) "Usage: extract.scm <file.scm>\n")
    (exit 1))
  (let* ((file (cadr args))
         (port (open-input-file file))
         (forms (read-all-forms port))
         (defs (extract-definitions forms))
         (mod-name (basename file ".scm")))
    (close-port port)
    (format #t "{\n")
    (format #t "  \"source_language\": \"scheme\",\n")
    (format #t "  \"module_name\": ~a,\n" (json-str mod-name))
    (format #t "  \"source_file\": ~a,\n" (json-str file))
    (format #t "  \"compiler_version\": \"guile-~a\",\n" (version))
    (format #t "  \"definitions\": [\n")
    (let loop ((ds defs) (first? #t))
      (when (pair? ds)
        (emit-definition (car ds) (current-output-port) first?)
        (loop (cdr ds) #f)))
    (format #t "\n  ]\n")
    (format #t "}\n")))

(main (command-line))
