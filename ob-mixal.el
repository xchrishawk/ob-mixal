;; ob-mixal.el
;; Chris Vig (chris@invictus.so)

;; -- Provide --

(provide 'ob-mixal)

;; -- Variables --

(defvar ob-mixal--mixasm-command "/usr/bin/mixasm"
  "The path to the mixasm installation on this machine.")

(defvar ob-mixal--mixvm-command "/usr/bin/mixvm"
  "The path to the mixvm installation on this machine.")

;; -- Org Babel Required Functions --

(defun org-babel-expand-body:mixal (body params)
  nil)

(defun org-babel-execute:mixal (body params)
  nil)

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
	 (compile-cmd (format "%s %s -o %s" ob-mixal--mixasm-command mixal-file mix-file)))
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
