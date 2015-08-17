;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; `VCI' -- Vector Compiling programming language Interface ;;;;;

;;; [ CONSTANTS ]

(defconst vci-TRUE -1 "usign Forth convention, -1 for strict `true'")
(defconst vci-FALSE 0 "zero is the only falsy value")

;;; [ FUNCTIONS ]

(defun vci:scalar-filter (scalar)
  (pcase scalar
    (`t vci-TRUE) 
    (`nil vci-FALSE)
    (_ scalar)))

(defun vci:get-type (any)
  "like `type-of', but returns `integer' instead of `float'"
  (let ((type (type-of any)))
    (if (eq 'float type)
	'integer
      type)))

(defun vci:hs-p (token)
  "can given token be considered as hs?"
  (and (stringp token)
       (= ?\# (aref token 0))))

(defun vci:hs-to-sym (hs)
  "take hs, return symbol"
  (intern (substring hs 1)))

(defun vci:sym-to-hs (sym)
  "take symbol, return hs"
  (concat "#" (symbol-name sym)))

(defun vci:hs-unpacked (maybe-hs)
  "take variable, return symbol if it is hs, unchanged otherwise"
  (if (vci-hs-p maybe-hs)
      (vci-hs-to-sym maybe-hs)
    maybe-hs))

(defun vci:to-sym-soft (token)
  "take `token', try to symbol-coerce it, if not possible, return unchanged"
  (cond ((symbolp token) token)
	((vci-hs-p token) (vci-hs-to-sym token))
	(t token)))
