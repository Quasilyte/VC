;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; `XVC' -- extras for Vector Compiling programming language ;;;;;

;;;; [ OPTIONS ] ;;;;

(setq xvc:options (make-hash-table :test 'eq))

(defun xvc:set-option (option)
  (puthash (car option) (cdr option) xvc:options))

(defun xvc:set-options (&rest options)
  (dolist (option options)
    (xvc:set-option option)))

;; set default options
(xvc:set-options '(return-stack-after-eval . t)
		 '(flush-stack-before-eval . t))

;;;; [ API ] ;;;;

(defun xvc:stack-flush ()
  "make `vc' stack empty"
  (setq vc-stack '()))

(defmacro xvc: (&rest words)
  "take some `vc' tokens, evaluate them (with respect to `xvc:options')"
  (append '(progn)
	  (when (gethash 'flush-stack-before-eval xvc:options)
	    '((xvc:stack-flush)))
	  `((vc:eval ',words))
	  (when (gethash 'return-stack-after-eval xvc:options)
	    '(vc-stack))))
