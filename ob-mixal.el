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
  ;; Add a newline to the end of the file
  (concat body "\n"))

(defun org-babel-execute:mixal (body params)
  "Executes the MIXAL code in BODY using PARAMS."
  (let* ((processed-params (org-babel-process-params params))
	 (expanded-body (org-babel-expand-body:mixal body params processed-params))
	 (mix-file (ob-mixal--compile expanded-body)))
    (when mix-file
      (ob-mixal--run mix-file))))

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
      (if (zerop (shell-command compile-cmd nil (current-buffer)))
	  mix-file
	(let ((stderr-buffer (generate-new-buffer "*mixasm*")))
	  (copy-to-buffer stderr-buffer (point-min) (point-max))
	  (with-current-buffer stderr-buffer
	    (compilation-mode))
	  (pop-to-buffer stderr-buffer)
	  nil)))))

(defun ob-mixal--run (file)
  "Runs the specified compiled MIX file in mixvm, and returns the results."
  (with-temp-buffer
    (call-process ob-mixal--mixvm-path		; execute mixvm
		  nil				; no stdin
		  (current-buffer)		; stdout to current buffer
		  nil				; don't display
		  "-d"				; dump VM status at end of run
		  "-t"				; dump timing status
		  "-r"				; run in batch mode
		  file)				; input file name
    (buffer-string)))
