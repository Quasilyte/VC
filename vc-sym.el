;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; `VC' -- Vector Compiling programming language ;;;;;

;;;; [ DATA SECTION ] ;;;;

;;; [ CONSTANTS ]

(defconst vc-TRUE -1 "usign Forth convention, -1 for strict `true'")
(defconst vc-FALSE 0 "zero is the only falsy value")

;;; [ RUNTIME VARIABLES ]

(setq vc-stack '() ; main data stack for execution
      vc-tokens '() ; usually this is an input data for interpreter
      vc-token nil ; token being processed
      vc-bindings (make-hash-table :test 'equal)) ; all defined words

;;;; [ CORE ] ;;;;

;;; [ MISCELLANEOUS ]

(defun vc-get-type (any)
  "like `type-of', but returns `integer' instead of `float'"
  (let ((type (type-of any)))
    (if (eq 'float type)
	'integer
      type)))

(defmacro vc-vector-convert (type-name vec)
  (let ((type-p (intern (concat type-name "p"))))
    `(let ((token (aref ,vec 0)))
       (if (,type-p token)
	   token
	 (error "failed to convert vector into %s" ,type-name)))))

;;; [ STACK ]

(defun vc-stack-drop ()
  "drop top element from the stack"
  (setq vc-stack (cdr vc-stack)))

(defun vc-stack-ndrop (n)
  "drop n top elements from the stack"
  (setq vc-stack (nthcdr n vc-stack)))

(defun vc-stack-pop ()
  "take top element from the stack"
  (pop vc-stack))

(defun vc-stack-npop (n)
  "take n top elements from the stack and return them as a list"
  (vc-stack-collect 3 (push el li)))

(defun vc-stack-push (scalar)
  "push scalar on top of the stack"
  (push (pcase scalar
	  (`t vc-TRUE) 
	  (`nil vc-FALSE)
	  (_ scalar))
	vc-stack))

(defun vc-stack-npush (scalars)
  "push given list of scalars on the data stack"
  (setq vc-stack (append scalars vc-stack)))

;;; [ BINDINGS ]

(defmacro vc-bindings-defun (sym io prog)
  "wrapper for registering builtin bindings"
  (let ((in (car io)) (out (car (cdr io))))
    (when out
      (setq prog (list (if (listp out)
			   'vc-stack-npush
			 'vc-stack-push)
		       prog)))
    (setq in (mapcar (lambda (name)
		       (if (listp name)
			   (let ((name (car name)) (n (car (cdr name))))
			     (list name (list 'vc-stack-npop n)))
			 (list name '(vc-stack-pop))))
		     (if (listp in)
			 in
		       (list in))))
    (list 'vc-bindings-add
	  sym
	  (list 'lambda nil
		(if in
		    `(let ,in ,prog)
		  prog)))))

(defun vc-bindings-remove (sym)
  (remhash sym vc-bindings))

(defun vc-bindings-add (sym vec)
  "insert or replace dictionary binding" 
  (puthash sym vec vc-bindings))

(defun vc-bindings-fetch (sym)
  "return dictionary binding, if it is stored (nil otherwise)"
  (gethash sym vc-bindings))

;;; [ TOKENS ]

(defun vc-tokens-drop ()
  "throw away one (current) token"
  (setq vc-tokens (cdr vc-tokens)))

(defun vc-tokens-prepend (tokens)
  "prepend new `tokens' to the current `vc-tokens' queue"
  (setq vc-tokens (append tokens vc-tokens)))

(defmacro vc-tokens-do (&rest forms)
  "evaluate passed forms through tokens (updates `vc-token' at each iteration)"
  `(while vc-tokens
     (setq vc-token (car vc-tokens))
     (vc-tokens-drop) ; never look at same token twice, ensure loop termination
     ,@forms))

(defmacro vc-tokens-skip-until (termination-p)
  "skip current token and keep skipping until `termination-p' is t"
  `(catch 'break
     (vc-tokens-do
      (when ,termination-p (throw 'break nil)))))

;;; [ EVALUATION ]

(defun vc-eval-prepare (tokens)
  "prepare interpreter for upcoming eval"
  (setq vc-tokens tokens))

(defun vc-eval-symbol ()
  "launch builtin or user defined funtion"
  (let ((binding (vc-bindings-fetch vc-token)))
    (if binding
	(if (functionp binding)
	    (funcall binding) ; call predefined function
	  (vc-tokens-prepend binding))
      (error "can not resolve `%s' symbol" vc-token))))

(defun vc-eval-token ()
  "process current token"
  (if (symbolp vc-token)
      (vc-eval-symbol)
    (vc-stack-push vc-token)))

(defun vc-eval-quotation ()
  "check if the token is a comment or a symbol"
  (if (and (eq 'quote (car vc-token))
	   (= 2 (length vc-token))) ; comment! ignore it
      (vc-stack-push (car (cdr vc-token))))) ; symbol! collect it
  
(defun vc:eval (tokens)
  "takes `tokens' as a list, reloads environment and evaluates each token"
  (vc-eval-prepare tokens)
  (vc-tokens-do
   (if (listp vc-token)
       (vc-eval-quotation)
     (vc-eval-token))))

;;; [ QUERY ]

(defun vc-query-bash (msg)
  "pass `msg' to bash interpreter, return printed output"
  (let ((output (shell-command-to-string msg)))
    (when (not (string= "" output)) 
      (message "> `%s'" output)) ; not empty output is printed
    (vc-stack-push output)))

(defun vc-command-describe (name)
  "print internal representation of `name' definition contents"
  (let* ((sym (intern-soft name))
	 (vec (vc-bindings-fetch (vc-symbol-convert sym))))
    (message "`%s' %s" sym (if vec
			       vec
			     "empty or undefined"))))

(defun vc-query-command (msg)
  "parse `msg' and execute special command"
  (let* ((msg (split-string msg))
	 (cmd (car msg))
	 (arg (car (cdr msg))))
    (funcall (pcase cmd
	       ("describe" 'vc-command-describe))
	     arg)))

;;;; [ BUILTINS ] ;;;;

;;; [ OPERATORS ]

(dolist (binary-op '(- / + * < >))
  (eval `(vc-bindings-defun ',binary-op ((1st 2nd) t) (,binary-op 2nd 1st))))

(dolist (unary-op '(1+ 1-))
  (eval `(vc-bindings-defun ',unary-op (top t) (,unary-op top))))

(vc-bindings-defun 'neg (top t) (- top))

(vc-bindings-defun 'not (top t) (if (and (numberp top) (zerop top))
				    vc-TRUE
				  vc-FALSE))

(vc-bindings-defun '= ((1st 2nd) t) (let ((type (vc-get-type 1st)))
				      (if (eq 'integer type)
					  (= 1st 2nd)
					(equal 1st 2nd))))

(vc-bindings-defun
 'eval (token nil) (vc-tokens-prepend (if (vectorp token)
					  token
					(vector token))))

;;; [ STACK ]
  
(vc-bindings-defun
 'shake (n (t)) (let ((fmt (mapcar (lambda (c) (- c ?0)) ; fmt list
				   (vc-stack-pop))) ; fmt string
		      (slice (vc-stack-npop n)))
		  (mapcar (lambda (pos)
			    (nth pos slice))
			  fmt)))

(vc-bindings-defun 'count (nil t) (length vc-stack))

;;; [ CONDITIONALS ]

(vc-bindings-defun
 'if (top nil) (when (= vc-FALSE top)
		 (vc-tokens-skip-until (or (eq 'endif vc-token)
					   (eq 'else vc-token)))))

(vc-bindings-defun
 'else (nil nil) (vc-tokens-skip-until (or (eq 'endif vc-token)
					   (eq 'else vc-token))))

(vc-bindings-defun 'endif (nil nil) '())

;;; [ OPERATIONS ON SEQUENCES ]

(vc-bindings-defun 'nth (index t) (elt (car vc-stack) index))

(vc-bindings-defun 'len (nil t) (length (car vc-stack)))

(vc-bindings-defun 'set ((val index) nil) (aset (car vc-stack) index val))

(vc-bindings-defun 'split (seq nil) (setq vc-stack (append seq vc-stack)))

(vc-bindings-defun 'copy (nil t) (copy-sequence (car vc-stack)))

(vc-bindings-defun 'vec (n t) (cond ((vectorp n) (vconcat n (vc-stack-pop)))
				    ((> n 0) (vconcat (vc-stack-npop n)))
				    ((< n 0) (make-vector (abs n) 0))))

(vc-bindings-defun 'str (n t) (cond ((stringp n) (concat n (vc-stack-pop)))
				    ((> n 0) (concat (vc-stack-npop n)))
				    ((< n 0) (make-string (abs n) 0))))

;;; [ TYPE ASSERTS ]

(vc-bindings-defun
 'num! (top t) (pcase (vc-get-type top)
		 (`integer top)
		 (`string (string-to-number top))
		 (`symbol (error "symbol can not be coerced with `num!'"))
		 (`vector (vc-vector-convert "number" top))))

(vc-bindings-defun
 'str! (top t) (pcase (vc-get-type top)
		 (`integer (number-to-string top))
		 (`string top)
		 (`symbol (symbol-name top))
		 (`vector (vc-vector-convert "string" top))))

(vc-bindings-defun
 'sym! (top t) (pcase (vc-get-type top)
		 (`integer (error "integer can not be coerced with `sym!'"))
		 (`string (intern top))
		 (`symbol top)
		 (`vector (vc-vector-convert "symbol" top))))

(vc-bindings-defun 'vec! (top t) (if (vectorp top)
				     top
				   (vector top)))

;;; [ BINDINGS ]

(vc-bindings-defun
 'bind ((sym binding) nil) (if (symbolp sym)
			       (vc-bindings-add sym (if (vectorp binding)
							(append binding nil)
						      (cons binding nil)))
			     (error "data can be binded only to a symbol")))

(vc-bindings-defun
 'unbind (sym nil) (if (symbolp sym)
		       (vc-bindings-remove sym)
		     (error "unbind expects symbol as topmost stack element")))

;;; [ ENVIRONMENT COMMUNICATION ]

(vc-bindings-defun
 'query (msg nil) (if (= ?$ (aref msg 0))
		      (vc-query-bash (substring msg 1))
		    (vc-query-command msg)))

;;;; [ FRIEND IMPORTS ] ;;;;

(load (expand-file-name "xvc.el" (file-name-directory (or load-file-name
							  buffer-file-name)))
      nil t)
