;; ob-mixal.el
;; Chris Vig (chris@invictus.so)

;; -- Provide --

(provide 'ob-mixal)

;; -- Variables --

(defvar ob-mixal--mixasm-path "/usr/bin/mixasm"
  "The path to the mixasm installation on this machine.")

(defvar ob-mixal--mixvm-path "/usr/bin/mixvm"
  "The path to the mixvm installation on this machine.")

;; -- Org Babel Required Functions --

(defun org-babel-expand-body:mixal (body params &optional processed-params)
  "Processes the MIXAL code in BODY using PARAMS."
  ;; Process parameters if we don't already have processed parameters
  (or processed-params
      (setq processed-params (org-babel-process-params params)))
  (let ((vars nil)
	(macros nil))
    ;; Get lists of variables and macros
    (dolist (param processed-params)
      (when (eq (car param) :var)
	(let ((key (format "%s" (cadr param)))
	      (value (format "%s" (cddr param))))
	  (cond
	   ((string-match "^[A-Z]+$" key) (push (cons key value) vars))
	   ((string-match "^%[A-Z]+%$" key) (push (cons key value) macros))
	   (t (user-error "Invalid var name: %s" key))))))
    ;; Assemble source in temporary buffer
    (with-temp-buffer
      ;; First, insert the body as is
      (insert body)
      ;; Expand each macro
      (dolist (macro macros)
	(goto-char (point-min))
	(while (re-search-forward (regexp-quote (car macro)) nil t)
	  (replace-match (cdr macro) nil nil)))
      ;; Insert vars as EQU directives at beginning of file
      (goto-char (point-min))
      (dolist (var (reverse vars))
	(insert (format "%s\tEQU\t%s\n" (car var) (cdr var))))
      ;; Insert a newline at the end of the file (mixasm requires this)
      (goto-char (point-max))
      (newline)
      ;; Finally, return the completed buffer
      (buffer-string))))

(defun org-babel-execute:mixal (body params)
  "Executes the MIXAL code in BODY using PARAMS."
  (let* ((processed-params (org-babel-process-params params))
	 (expanded-body (org-babel-expand-body:mixal body params processed-params))
	 (mix-file (ob-mixal--compile expanded-body)))
    (when mix-file
      (prog1
	  (ob-mixal--run mix-file processed-params)
	(delete-file mix-file)))))

(defun org-babel-prep-session:mixal (session params)
  (error "MIXAL does not currently support sessions."))

;; -- Helper Functions --

(defun ob-mixal--compile (body)
  "Saves BODY to a temporary file, and attempts to compile that file with mixasm.
If the compilation succeeds, the name of the compiled binary file is returned.
Otherwise, `nil' is returned, and a buffer is displayed showing the stderr
output from the mixasm process."
  ;; Generate needed file names and arguments
  (let* ((mixal-file (make-temp-file "ob-mixal-" nil ".mixal"))
	 (mix-file (make-temp-file "ob-mixal-" nil ".mix"))
	 (compile-cmd (format "%s %s -o %s" ob-mixal--mixasm-path mixal-file mix-file)))
    ;; Build the MIXAL file
    (with-temp-file mixal-file
      (insert body))
    ;; Compile the MIXAL file
    (with-temp-buffer
      (prog1
	  (if (zerop (shell-command compile-cmd nil (current-buffer)))
	      ;; Compilation succeeded - return the binary
	      (progn
		(delete-file mixal-file)
		mix-file)
	    ;; Compilation failed - show buffer to user and return nil
	    (let ((stderr-buffer (generate-new-buffer "*mixasm*")))
	      (copy-to-buffer stderr-buffer (point-min) (point-max))
	      (with-current-buffer stderr-buffer
		(compilation-mode))
	      (pop-to-buffer stderr-buffer)
	      nil))))))

(defun ob-mixal--run (file processed-params)
  "Runs the specified compiled MIX file in mixvm, and returns the results."
  (let ((script (ob-mixal--build-script file processed-params)))
    (with-temp-buffer
      (prog2
	  (call-process ob-mixal--mixvm-path script (current-buffer))
	  (ob-mixal--postprocess-results (current-buffer) processed-params)
	(delete-file script)))))

(defun ob-mixal--build-script (file processed-params)
  "Builds a mixvm script running FILE using the :mixvm arg from PROCESSED-PARAMS."
  (let* ((script (make-temp-file "ob-mixal-" nil ".mixvm"))
	 (mixvm-param (cdr (assoc :mixvm processed-params)))
	 (requested-outputs (and mixvm-param (split-string mixvm-param))))
    (with-temp-file script
      (insert "load " file "\n")
      (insert "run\n")
      (dolist (output requested-outputs)
	(pcase output
	  ("input" nil)
	  ("output" nil)
	  ("time" nil)
	  (other (user-error "Invalid output requested: %s" other))))
      (insert "quit\n"))
    script))

(defun ob-mixal--postprocess-results (buffer processed-params)
  "Post-processes mixvm results in BUFFER using PROCESSED-PARAMS."
  (with-current-buffer buffer
    (ob-mixal--replace "^MIX\> quit\nQuitting \.\.\.\n")
    (buffer-string)))

(defun ob-mixal--replace (regexp &optional replacement)
  "Replaces text matching REGEXP in current buffer if REPLACEMENT is not `nil', or
deletes it completely if REPLACEMENT is `nil'."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match (if replacement replacement "")))))
