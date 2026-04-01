
;; https://github.com/atlas-engineer/nyxt/issues/1970
;; not working
;; (defun meta-translate-modifiers (modifier-state &optional event)
;;   "Custom modifier translator mapping Meta to what Alt usually means."
;;   (declare (ignore event))
;;   (let ((plist '(:control-mask "control"
;;                  ;; :mod1-mask nil ;; Unmap Alt.
;;                  ;; :meta-mask "meta" ;; Map Meta to Alt.
;;                  ;; :shift-mask "shift"
;;                  ;; :super-mask "super"
;;                  :mod1-mask nil ;; Unmap Alt.
;;                  :meta-mask "super" ;; Map Meta to Alt.
;;                  :shift-mask "shift"
;;                  :super-mask "meta"
;;                  :hyper-mask "hyper")))
;;     (delete nil (mapcar (lambda (mod) (getf plist mod)) modifier-state))))
;; ;; Configure the browser to use those modifiers.
;; (define-configuration browser
;;   ((modifier-translator #'meta-translate-modifiers)))

;; swap alt & meta
(define-configuration (buffer)
  ((modifier-plist '(:shift "shift" :control "control" :alt "super" :meta "meta"))))

