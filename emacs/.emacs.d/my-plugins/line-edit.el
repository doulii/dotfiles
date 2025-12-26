;;; line-edit.el --- Minor-mode: Inline edit a single line from another file -*- lexical-binding: t; -*-


(defvar-local line-edit-target-buffer nil)
(defvar-local line-edit-line-num -1)


;; 读取目标buffer的目标行
(defun line-edit--read-line (buf line)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (if (eobp) "" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

(defun line-edit-refresh ()
  (interactive)
  (unless line-edit-target-buffer (user-error "No target file"))
  (delete-region (line-beginning-position) (line-end-position))
  (insert (line-edit--read-line line-edit-target-buffer
								line-edit-line-num)))

(defun line-edit--ensure-target-buffer (file)
  (or (find-buffer-visiting file)
      (find-file-noselect file)))

(defun line-edit-open-file (file)
  (interactive
   (list
    (read-file-name "File: ")))
    ;; (read-number "Line: " 1)
  (setq line-edit-line-num 1)
  (setq line-edit-target-buffer (line-edit--ensure-target-buffer file))
  (line-edit-refresh))

(defun line-edit-quit ()
  (interactive)
  (setq line-edit-target-buffer nil)
  (setq line-edit-line-num -1)
  (message "Line-edit quit"))


(defun line-edit--change-line (d)
  (setq line-edit-line-num (max 1 (+ line-edit-line-num d)))
  (line-edit-refresh))

(defun line-edit-next-line (&optional arg)
  (interactive "p")
  (line-edit--change-line (or arg 1)))

(defun line-edit-previous-line (&optional arg)
  (interactive "p")
  (line-edit--change-line (- (or arg 1))))


;; 替换目标buffer的指定line为指定text
(defun line-edit--replace-line (buf line text)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (delete-region beg end)
        (goto-char beg)
        (insert text)))))

;; 获取当前buffer的当前行
(defun line-edit--current-line-text ()
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun line-edit-commit-line ()
  (interactive)
  (unless line-edit-target-buffer (user-error "No target file"))
  (line-edit--replace-line line-edit-target-buffer
						   line-edit-line-num
						   (line-edit--current-line-text))
  (message "Line committed to target buffer (not saved yet)"))


;; insert text at line begin
(defun line-edit--insert-text (line)
  (unless line-edit-target-buffer (user-error "No target file"))
  (let ((text (line-edit--current-line-text)))
	(with-current-buffer line-edit-target-buffer
	  (save-excursion
		(goto-char (point-min))
		(message "forward to %s" (1- line))
		(forward-line (1- line))
		(insert text)
		(insert "\n")))))

;; 在当前行后插入文本，并将光标移动到下一行，即插入的行
(defun line-edit-insert-after ()
  (interactive)
  (line-edit--insert-text (1+ line-edit-line-num))
  (setq line-edit-line-num (1+ line-edit-line-num)))

;; 在当前行前插入文本，光标保持在当前行，但是line-num会加一
(defun line-edit-insert-before ()
  (interactive)
  (line-edit--insert-text line-edit-line-num)
  (setq line-edit-line-num (1+ line-edit-line-num)))


(defun line-edit-show-info ()
  (interactive)
  (if line-edit-target-buffer
      (message "Current line-edit info: buffer=%s, target buffer=%s, line=%d"
               (buffer-name) line-edit-target-buffer line-edit-line-num)
    (message "Line-edit not active")))

(defun line-edit-jump-to-target ()
  (interactive)
  (unless line-edit-target-buffer (user-error "No target file"))
  (let ((buf line-edit-target-buffer)
		(line line-edit-line-num))
	(switch-to-buffer line-edit-target-buffer)
	(goto-char (point-min))
	(forward-line (1- line))
	(message "Jumped to target buffer %s line %d" buf line)))


(defvar line-edit-command-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "f") 'line-edit-open-file)
	(define-key map (kbd "n") 'line-edit-next-line)
	(define-key map (kbd "p") 'line-edit-previous-line)
	;; (define-key map (kbd "G") 'line-edit-goto-line)
	(define-key map (kbd "e") 'line-edit-toggle-edit)
	(define-key map (kbd "r") 'line-edit-refresh)
	(define-key map (kbd "q") 'line-edit-quit)
	(define-key map (kbd "i") 'line-edit-show-info)
	(define-key map (kbd "j") 'line-edit-jump-to-target)
	(define-key map (kbd "c") 'line-edit-commit-line)
	(define-key map (kbd "o") 'line-edit-insert-after)
	(define-key map (kbd "O") 'line-edit-insert-before)
	map)
  "Keymap for line-edit commands.")

;; Minor mode keymap still exists, can be empty or include the same commands
(defvar line-edit-minor-mode-map (make-sparse-keymap))

(define-minor-mode line-edit-minor-mode
  "Line-edit: Inline edit a line from another file."
  :lighter " LE"
  :keymap line-edit-minor-mode-map)

(provide 'line-edit)
;;; line-edit.el ends here
