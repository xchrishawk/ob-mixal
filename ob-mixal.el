;; ob-mixal.el
;; Chris Vig (chris@invictus.so)

;; -- Provide --

(provide 'ob-mixal)

;; -- Variables --

(defvar ob-mixal--mixvm-command "/usr/bin/mixvm"
  "The path to the mixvm installation on this machine.")

;; -- Org Babel Required Functions --

(defun org-babel-expand-body:mixal (body params)
  nil)

(defun org-babel-execute:mixal (body params)
  nil)

(defun org-babel-prep-session:mixal (session params)
  (error "MIXAL does not currently support sessions."))
