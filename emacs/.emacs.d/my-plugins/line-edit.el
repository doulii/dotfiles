;;; line-edit.el --- Minor-mode: Inline edit a single line from another file -*- lexical-binding: t; -*-

;; Description:
;;  - Copies a line from a target file into the current buffer for editing
;;  - Supports navigating lines, editing, and committing back to the target buffer
;;  - Provides commands to show current info and jump to the target line

(defvar-local line-edit-target-file nil)
(defvar-local line-edit-target-buffer nil)
(defvar-local line-edit-line-num 1)
(defvar-local line-edit-editing nil)
(defvar-local line-edit-current-pos nil)

(defun line-edit--ensure-target-buffer (file)
  (or (find-buffer-visiting file)
      (find-file-noselect file)))

(defun line-edit--read-line (buf line)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (if (eobp) "" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

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

(defun line-edit-refresh ()
  (interactive)
  (unless line-edit-target-file (user-error "Target file not set"))
  (let* ((buf (line-edit--ensure-target-buffer line-edit-target-file))
         (content (line-edit--read-line buf line-edit-line-num)))
    (setq line-edit-current-pos (point))
    (delete-region (line-beginning-position) (line-end-position))
    (insert content)
    (setq line-edit-editing nil)))

(defun line-edit--change-line (d)
  (when line-edit-editing (line-edit-commit-line))
  (setq line-edit-line-num (max 1 (+ line-edit-line-num d)))
  (line-edit-refresh))

(defun line-edit-next-line (&optional arg)
  "Move to next line in target file. With ARG, move ARG lines."
  (interactive "p") ;; read prefix argument, default 1
  (line-edit--change-line (or arg 1)))

(defun line-edit-previous-line (&optional arg)
  "Move to previous line in target file. With ARG, move ARG lines."
  (interactive "p")
  (line-edit--change-line (- (or arg 1))))

(defun line-edit-toggle-edit ()
  (interactive)
  (setq line-edit-editing (not line-edit-editing))
  (if line-edit-editing
      (message "Edit mode enabled: modify the line and press C to commit")
    (message "Edit mode disabled")))

(defun line-edit-commit-line ()
  (interactive)
  (unless line-edit-editing (user-error "Not in edit mode"))
  (let ((text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (buf (line-edit--ensure-target-buffer line-edit-target-file)))
    (line-edit--replace-line buf line-edit-line-num text)
    (message "Line committed to target buffer (not saved yet)"))
  (setq line-edit-editing nil))

(defun line-edit-quit ()
  (interactive)
  (setq line-edit-target-file nil)
  (setq line-edit-target-buffer nil)
  (setq line-edit-line-num 1)
  (setq line-edit-current-pos nil)
  (message "Line-edit cleared"))

;; ---------------- Minor mode ----------------
(defvar line-edit-command-map
  (let ((map (make-sparse-keymap)))
	;; Bind commands using (kbd ...) for consistency
	(define-key map (kbd "o") 'line-edit-open-file)
	(define-key map (kbd "n") 'line-edit-next-line)
	(define-key map (kbd "p") 'line-edit-previous-line)
	(define-key map (kbd "e") 'line-edit-toggle-edit)
	(define-key map (kbd "c") 'line-edit-commit-line)
	(define-key map (kbd "r") 'line-edit-refresh)
	(define-key map (kbd "q") 'line-edit-quit)
	(define-key map (kbd "i") 'line-edit-show-info)
	(define-key map (kbd "j") 'line-edit-jump-to-target)
	map)
  "Keymap for line-edit commands.")

;; Minor mode keymap still exists, can be empty or include the same commands
(defvar line-edit-minor-mode-map (make-sparse-keymap))

(define-minor-mode line-edit-minor-mode
  "Line-edit: Inline edit a line from another file."
  :lighter " LE"
  :keymap line-edit-minor-mode-map)

(defun line-edit-open-file (file line)
  (interactive
   (list
    (read-file-name "File: ")
    (read-number "Line: " 1)))
  (setq line-edit-target-file (expand-file-name file))
  (setq line-edit-line-num line)
  (setq line-edit-target-buffer (line-edit--ensure-target-buffer file))
  (line-edit-refresh))

;; ---------------- Additional commands ----------------
(defun line-edit-show-info ()
  "Show current line-edit info: buffer / file / line." 
  (interactive)
  (if line-edit-target-file
      (message "Current line-edit info: buffer=%s, file=%s, line=%d"
               (buffer-name) line-edit-target-file line-edit-line-num)
    (message "Line-edit not active")))

(defun line-edit-jump-to-target ()
  "Jump to the target file's corresponding line."
  (interactive)
  (unless line-edit-target-file (user-error "Line-edit not active"))
  (let ((buf (line-edit--ensure-target-buffer line-edit-target-file)))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (forward-line (1- line-edit-line-num))
    (message "Jumped to target file %s line %d" line-edit-target-file line-edit-line-num)))

(provide 'line-edit)
;;; line-edit.el ends here
