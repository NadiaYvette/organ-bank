;;; SBCL extraction script for organ-bank.
;;; Loads a Lisp file, discovers user-defined functions, and emits OrganIR JSON.

(defun json-escape (str)
  "Escape a string for JSON output."
  (with-output-to-string (out)
    (loop for ch across str do
      (case ch
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Tab (write-string "\\t" out))
        (#\Return (write-string "\\r" out))
        (otherwise (write-char ch out))))))

(defun json-str (s)
  (format nil "\"~a\"" (json-escape (princ-to-string s))))

(defun get-function-lambda-list (fn)
  "Get the lambda list of a function, if possible."
  (handler-case
    (sb-introspect:function-lambda-list fn)
    (error () nil)))

(defun get-function-type (fn)
  "Get the declared type of a function."
  (handler-case
    (sb-introspect:function-type fn)
    (error () nil)))

(defun get-disassembly (fn)
  "Get disassembly as a string."
  (with-output-to-string (s)
    (disassemble fn :stream s)))

(defun emit-definition (name fn stream first-p)
  "Emit a single OrganIR definition for a function."
  (let* ((lambda-list (get-function-lambda-list fn))
         (ftype (get-function-type fn))
         (disasm (get-disassembly fn))
         (arity (if lambda-list (length lambda-list) 0)))
    (unless first-p (write-string ",\n" stream))
    (format stream "    {~%")
    (format stream "      \"name\": {\"module\": \"COMMON-LISP-USER\", \"text\": ~a, \"unique\": 0},~%"
            (json-str (string-downcase (symbol-name name))))
    ;; Type
    (if ftype
        (format stream "      \"type\": {\"tag\": \"con\", \"name\": ~a},~%" (json-str ftype))
        (format stream "      \"type\": {\"tag\": \"any\"},~%"))
    ;; Disassembly as body
    (format stream "      \"expr\": {\"tag\": \"disasm\", \"text\": ~a},~%" (json-str disasm))
    (format stream "      \"sort\": \"fun\",~%")
    (format stream "      \"visibility\": \"public\",~%")
    (format stream "      \"arity\": ~d~%" arity)
    (format stream "    }")))

(defun extract-organ-ir (file)
  "Load FILE, find user-defined functions, emit OrganIR JSON on stdout."
  ;; Record symbols before loading
  (let ((before (make-hash-table :test 'eq)))
    (do-symbols (s (find-package "COMMON-LISP-USER"))
      (when (fboundp s) (setf (gethash s before) t)))
    ;; Load the file
    (handler-case
      (load file :verbose nil :print nil)
      (error (e)
        (format *error-output* "Error loading ~a: ~a~%" file e)
        (sb-ext:exit :code 1)))
    ;; Find new functions
    (let ((new-fns nil))
      (do-symbols (s (find-package "COMMON-LISP-USER"))
        (when (and (fboundp s)
                   (not (gethash s before))
                   (not (macro-function s))
                   (not (special-operator-p s)))
          (push s new-fns)))
      ;; Emit JSON
      (format t "{~%")
      (format t "  \"source_language\": \"common-lisp\",~%")
      (format t "  \"module_name\": ~a,~%" (json-str (pathname-name (pathname file))))
      (format t "  \"source_file\": ~a,~%" (json-str file))
      (format t "  \"compiler_version\": \"sbcl-~a\",~%"
              (lisp-implementation-version))
      (format t "  \"definitions\": [~%")
      (let ((first-p t))
        (dolist (name (sort new-fns #'string< :key #'symbol-name))
          (emit-definition name (symbol-function name) *standard-output* first-p)
          (setf first-p nil)))
      (format t "~%  ]~%")
      (format t "}~%"))))

;; Entry point
(let ((args sb-ext:*posix-argv*))
  ;; args: ("sbcl" ... "--" "file.lisp") or via --eval
  ;; When invoked via our Haskell wrapper, the file path is passed as the last arg
  (when (>= (length args) 1)
    (let ((file (car (last args))))
      (when (and (stringp file) (probe-file file))
        (extract-organ-ir file)
        (sb-ext:exit :code 0)))))
