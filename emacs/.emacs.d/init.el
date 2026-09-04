;;; -*- lexical-binding: t -*-

(load (locate-user-emacs-file "local.el") t)

(defmacro doulii/get-config (name default)
  (let ((sym (intern (concat "doulii/local/" (symbol-name name)))))
    `(if (boundp ',sym)
         ,sym
       ,default)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
;; (menu-bar-mode t)
;; (setq visible-bell t)

;; 窗口无边框
;; (add-to-list 'default-frame-alist '(undecorated . t))


;; Themes
;; https://emacsthemes.com/
;;
;; (load-theme 'tango-dark)
;; (load-theme 'wombat)
;; (load-theme 'doom-solarized-dark)
;; (load-theme 'doom-tomorrow-night)
;; (load-theme 'doom-monokai-classic) ;; 背景偏黄
;; (load-theme 'doom-monokai-octagon) ;; 太蓝
;; (load-theme 'doom-monokai-pro) ;; 太黄
;; (load-theme 'doom-monokai-ristretto) ;; 比pro更黄
;; (load-theme 'doom-monokai-machine t) ;; 还行 有点蓝
;; (load-theme 'doom-monokai-spectrum) ;; 还行 偏暗

(setq-default tab-width 4)

(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; 某些mode下禁用line numbers
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                neotree-mode-hook
                shell-mode-hook
                eshel-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; 中文line break
(setq word-wrap-by-category t)

(setq doulii/font-size (doulii/get-config font-size 150))
(setq doulii/font-family (doulii/get-config font-family "FiraCode Nerd Font"))

(set-face-attribute 'default nil :font doulii/font-family :height doulii/font-size)
;; (add-to-list 'default-frame-alist '(font . "Yahei Consolas Hybrid-12"))
;; (add-to-list 'default-frame-alist '(font . "WenQuanYi Zen Hei"))
;; (add-to-list 'default-frame-alist '(font . "WenQuanYi Zen Hei Mono-16"))
;; (add-to-list 'default-frame-alist '(font . "YaHei Consolas Hybrid-16"))
;; 中文（汉字）
;; (dolist (charset '(han cjk-misc bopomofo))
;;   (set-fontset-font
;;    t charset
;;    (font-spec :family "崇羲篆體" :size 25)))


;; enable by `M-x variable-pitch-mode`
(set-face-attribute 'fixed-pitch nil :family doulii/font-family :height doulii/font-size)
(set-face-attribute 'variable-pitch nil :family doulii/font-family :height doulii/font-size :weight 'regular)

(set-locale-environment "zh_CN.UTF-8")

(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(let ((custom-file-path (expand-file-name "custom.el" user-emacs-directory)))
          ;; (make-directory custom-file-path t)
          (setq custom-file custom-file-path)
          (load custom-file))
  ;; (load-file custom-file)

;; (let ((backup-dir-path (expand-file-name "backups/" user-emacs-directory)))
;;   (make-directory backup-dir-path t)
;;   (setq backup-directory-alist '(("." . ,backup-dir-path))))
;; (setq make-backup-files t)

;; (let ((auto-save-dir-path (expand-file-name "auto-saves/" user-emacs-directory)))
;;   (setq auto-save-file-name-transforms `((".*" ,auto-save-dir-path t)))
;;   (make-directory auto-save-dir-path t))

;; (make-directory "~/.emacs.d/backups/" t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))

;; (make-directory "~/.emacs.d/auto-saves/" t)
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t)))

;; MELPA community packages
;; Initialize package sources
(require 'package)

;;			 ("melpa-stable" . "https://stable.melpa.org/packages/")
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
                         ;; ("nongnu" . "https://elpa.nongnu.org/nongnu/") ;; for eat, no need now
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; display command line history
;; M-x global-command-log-mode
;; M-x clm/toggle-command-log-buffer
(use-package command-log-mode)

(use-package sqlite3)

;; 首次安装需要运行
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; 首次安装需要运行
;; M-x nerd-icons-install-fonts
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; https://github.com/doomemacs/themes/tree/screenshots
(use-package doom-themes
  :init (load-theme (doulii/get-config theme 'doom-one-light) t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Enable Vertico.
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))


;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )


(use-package embark
  :bind
  (:map minibuffer-local-map
   ("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult)

(use-package which-key
  ;; :init (which-key-mode)
  ;; :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; (use-package helpful
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package general)
(general-create-definer leader-key :prefix "C-c")

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
(leader-key "ts" '(hydra-text-scale/body :which-key "scale text"))

(defhydra hydra-dap-debug (:timeout 4)
  "dap debug"
  ("c" dap-continue "continue" :exit t)
  ("n" dap-next "next")
  ("i" dap-step-in "step in")
  ("o" dap-step-out "step out")
  ("q" nil "quit" :exit t))
(leader-key "dd" '(hydra-dap-debug/body :which-key "dap debug"))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))

;; (use-package evil-leader
;;   :config
;;     (global-evil-leader-mode) ; enable global-evil-leader-mode before evil-mode
;;     (evil-leader/set-leader ";")
;;    (evil-leader/set-key
;;        "e" 'treemacs ; 太浪费，不常用，但占用了短快捷键
;;        "q" 'quit-window
;;        "k" 'kill-buffer
;;        "b" 'counsel-ibuffer
;;        "dd" 'dap-debug-last
;;        "dr" 'dap-debug-restart
;;        "dq" 'dap-disconnect
;;        "db" 'dap-breakpoint-toggle
;;        "dc" 'dap-continue
;;        "dn" 'dap-next
;;        "di" 'dap-step-in
;;        "do" 'dap-step-out
;;        "SPC" 'ace-jump-word-mode
;;        "jb" 'ace-jump-mode-pop-mark
;;        "jc" 'ace-jump-char-mode
;;        "jl" 'ace-jump-line-mode
;;        "jw" 'ace-jump-word-mode
;;        ";" 'evil-repeat-find-char))
(defun doulii/set-evil-key (s f)
  (evil-define-key 'normal 'global (kbd (concat "<leader>" s)) f))

(defun doulii/set-evil-keymap (s map)
  (map-keymap
   (lambda (event binding)
     (when (commandp binding)
       ;; event 可能是 number 或 symbol，要转换成字符串
       (let ((key-str (single-key-description event)))
         (doulii/set-evil-key (concat s key-str) binding))))
   map))

;; origami toggle使用origami-forward-toggle-node
(defun doulii/evil-fold-origami-forward (mode-actions)
  (if (eq (caar mode-actions) 'origami-mode)
      (cons
       (car mode-actions)
       (plist-put (cdr mode-actions)
                  :toggle (lambda () (origami-forward-toggle-node (current-buffer) (point)))))
    mode-actions))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))))

;; fix dap-debug 可能修改 treemacs--in-this-buffer 的问题
(defun doulii/toggle-treemacs ()
  (interactive)
  (setq-default treemacs--in-this-buffer nil)
  (treemacs))

(use-package evil
  :init
  (setq evil-undo-system 'undo-tree)
  ;; (setq evil-want-integration t) ;; default is true
  (setq evil-want-keybinding nil)
  ;;  :after (evil-leader)
  :config
  (setq evil-fold-list (mapcar #'doulii/evil-fold-origami-forward evil-fold-list))
  (evil-mode 1)
  ;; (evil-set-leader '(normal motion) ";")
  (evil-set-leader 'normal ";")
  ;; (doulii/set-evil-key "ee" 'treemacs)
  (doulii/set-evil-key "ee" 'doulii/toggle-treemacs)
  (doulii/set-evil-key "ewe" 'treemacs-edit-workspaces)
  (doulii/set-evil-key "es" 'treemacs-switch-workspace)
  (doulii/set-evil-key "en" 'neotree-toggle)
  (doulii/set-evil-key "q" 'quit-window)
  (doulii/set-evil-key "x" 'delete-window)
  (doulii/set-evil-key "k" 'kill-buffer)
  (doulii/set-evil-key "b" 'counsel-ibuffer)
  (doulii/set-evil-key "ss" 'save-buffer)
  (doulii/set-evil-key "sr" 'consult-ripgrep)
  (doulii/set-evil-key "sl" 'consult-line)
  (doulii/set-evil-key "gi" 'consult-imenu)
  (doulii/set-evil-key "go" 'consult-outline)
  (doulii/set-evil-key "dd" 'dap-debug-last)
  (doulii/set-evil-key "dr" 'dap-debug-restart)
  (doulii/set-evil-key "dq" 'dap-disconnect)
  (doulii/set-evil-key "db" 'dap-breakpoint-toggle)
  (doulii/set-evil-key "dc" 'dap-continue)
  (doulii/set-evil-key "dn" 'dap-next)
  (doulii/set-evil-key "di" 'dap-step-in)
  (doulii/set-evil-key "do" 'dap-step-out)
  (doulii/set-evil-key "SPC" 'ace-jump-word-mode)
  (doulii/set-evil-key "jb" 'ace-jump-mode-pop-mark)
  (doulii/set-evil-key "jc" 'ace-jump-char-mode)
  (doulii/set-evil-key "jl" 'ace-jump-line-mode)
  (doulii/set-evil-key "jw" 'ace-jump-word-mode)
  (doulii/set-evil-key "mb" 'magit-blame)
  ;; (doulii/set-evil-key "pg" 'go-playground)
  (doulii/set-evil-key "p" 'persp-key-map)
  ;; (doulii/set-evil-keymap "c" claude-code-command-map)
  (doulii/set-evil-key "rb" 'revert-buffer)
  ;; (doulii/set-evil-keymap "l" line-edit-command-map) ;; 需要package ready以后才能读取变量
  (doulii/set-evil-key ";" 'evil-repeat-find-char))


;; (use-package evil-collection
;;   :after evil
;;   :config (evil-collection-init))

;; vim style C-g
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; use ~gcc~ to toggle comment
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package unicad)

;; (use-package persp-mode
;;   :init
;;   (setq persp-keymap-prefix nil)
;;   ;; :custom
;;   ;; (persp-keymap-prefix nil)
;;   :config
;;   (with-eval-after-load "persp-mode-autoloads"
;;     (setq wg-morph-on nil) ;; switch off animation
;;     (setq persp-autokill-buffer-on-remove 'kill-weak)
;;     (add-hook 'window-setup-hook #'(lambda () (persp-mode 1)))))

(use-package winner
  :hook (after-init . winner-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x pgtk))
  :config
  (dolist (var '("LANG" "LC_CTYPE" "LC_ALL"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
  ;; (setq exec-path-from-shell-arguments nil)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(defun doulii/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))
;;  (setq evil-auto-indent nil))
;;  (auto-fill-mode 0)

(use-package org
  :hook (org-mode . doulii/org-mode-setup)
  :config
  (setq org-edit-src-content-indentation 0)
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-todo-keywords
		'((sequence "TODO(t)" "DOING(i)" "PENDING(p)" "|" "DONE(d!)" "REJECTED(r)")
		  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; TODO
  ;; Custom agenda view
  ;; https://github.com/daviwil/emacs-from-scratch/blob/5e1f99448e32852277e2d274ce2057d55b8c7aaf/init.el#L300
  ;; Capture templates
  (setq org-capture-templates
		`(("t" "Tasks / Projects")
		  ("tt" "Task" entry (file+olp "~/Nextcloud/OrgMode/Tasks.org" "Inbox")
		   "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))

  ;; (setq org-agenda-files '("~/Nextcloud/OrgMode/wiki/editors/emacs/emacs-from-scratch.org"))
  ;; (setq org-agenda-files '("~/Nextcloud/OrgMode/"))
  (setq org-agenda-files (directory-files-recursively "~/Nextcloud/OrgMode/" "\\.org$"))
  (setq org-directory "~/Nextcloud/OrgMode/")

  ;; LaTeX preview scale
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))

  ;; org mode heading font size
  (dolist (face '((org-level-1 . 1.2)
				  (org-level-2 . 1.1)
				  (org-level-3 . 1.05)
				  (org-level-4 . 1.0)
				  (org-level-5 . 1.0)
				  (org-level-6 . 1.0)
				  (org-level-7 . 1.0)
				  (org-level-8 . 1.0)))
	;;  (message "%s" (cdr face)))
	;;  (set-face-attribute (car face) nil :font "YaHei Consolas Hybrid" :weight 'regular :height (cdr face)))
	(set-face-attribute (car face) nil :family doulii/font-family :weight 'regular :height (cdr face)))

  ;; column view font size
  (set-face-attribute 'org-column nil :height 150)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;; org mode 居中显示
(defun doulii/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
		visual-fill-column-center-text t)
  (visual-fill-column-mode))
(use-package visual-fill-column
  :defer t
  :hook (org-mode . doulii/org-mode-visual-fill))

(setq org-babel-python-command "python3")
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; (go . t)
     (scheme . t)
	 (python . t)))
  (setq org-confirm-babel-evaluate nil))

(with-eval-after-load 'org
  (require 'org-tempo)
  (dolist (tpl '(("sh"   . "src bash")
                 ("el"   . "src emacs-lisp")
                 ("go"   . "src go")
                 ("scm"  . "src scheme")
                 ("sql"  . "src sql")
                 ("py"   . "src python")
                 ("ini"  . "src ini")
                 ("conf" . "src conf")
                 ("json" . "src json")
                 ("toml" . "src toml")
                 ("yaml" . "src yaml")))
    (add-to-list 'org-structure-template-alist tpl)))

;; org mode (Refer: org mode guide)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; 自动展开加粗斜体等marker
(use-package org-appear
  :after org
  :config (setq org-appear-autolinks t)
  :hook (org-mode . org-appear-mode))
;; (use-package org-expose-emphasis-markers
;;   :after org
;; 	:hook (org-mode . org-expose-emphasis-markers-mode))

(use-package org-download
  :config
  (setq org-download-heading-lvl nil))



(defun doulii/org-babel-tangle-config ()
  ;;  (when (string-equal (file-name-directory buffer-file-name)
  ;;                      (expand-file-name user-emacs-directory))
  (when (string-equal (file-name-nondirectory
                       (directory-file-name
                        (file-name-directory buffer-file-name)))
                      ".emacs.d")
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'doulii/org-babel-tangle-config)))

;; 获取上次发版的版本号
(defun doulii/application-publish-notification/get-last-version ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "/-/compare/[a-z0-9\\.]*\\.\\.\\.\\\([a-z0-9\\.]*\\\)\\\W" nil t 1)
    (match-string-no-properties 1)))

;; 生成发版时间，最早为四分钟以后，取整到5分钟的整数倍
(defun doulii/application-publish-notification/publish-time ()
  (let ((time (decode-time (time-add (current-time) 240)))
        (r 5))
    (format-time-string "%Y/%-m/%-d - %H:%M"
                        (org-encode-time
                         (apply #'list
                                0 (* r (ceiling (nth 1 time) r))
                                (nthcdr 2 time))))))

;; run vterm with C-c p x v
(use-package vterm)

;; eat
;; need non-gnu elpa
;; 不好用，退格键不能删除
;; (use-package eat)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '(("~/Projects" . 1))))
  (setq projectile-switch-project-action #'projectile-find-file)
  (setq projectile-enable-caching t)
  (setq projectile-enable-cmake-presets t)
  :config
  ;; add cmake sub project
  ;; https://github.com/bbatsov/projectile/issues/1130#issuecomment-1123237339
  ;; (dolist (e '("package.json" "meson.build" "CMakeLists.txt" ))
  ;;       (add-to-list 'projectile-project-root-files-bottom-up e))
  (add-to-list 'projectile-ignored-projects "/opt/homebrew/")
  (add-to-list 'projectile-ignored-projects "~/")
  (add-to-list 'projectile-globally-ignored-files "#*#"))

;; (setq projectile-project-search-path '(("~/Projects" . 2))))
;; (setq projectile-project-search-path '(("~/Projects" . 1)
;;                                        ("~/Projects/github" . 1))))
;; (setq projectile-switch-project-action #'projectile-dired)
;;  (setq projectile-switch-project-action 'neotree-projectile-action))

;; ivy/counsel不再使用，切换到vertico
;; (use-package counsel-projectile
;;   :after projectile
;;   :config
;;   (projectile-known-projects) ;; counsel-projectile启动时不能正确list project，先临时fix，等修复 https://github.com/ericdanan/counsel-projectile/issues/189
;;   (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
;; (use-package evil-magit
;;   :after magit)

(setq auth-sources '("~/.authinfo"))
;; https://magit.vc/manual/ghub/Getting-Started.html
;; https://magit.vc/manual/forge
;; TODO: clone github/gitlab repository
(use-package forge
  :after magit
  :config
  (add-to-list 'forge-alist '("git.bilibili.co" "git.bilibili.co/api/v4" "git.bilibili.co" forge-gitlab-repository)))

(use-package diff-hl
  :after magit
  :config
  (global-diff-hl-mode)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package yasnippet
  :config
  (yas-reload-all)
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :after yasnippet)

;; :config
;; (add-to-list 'company-backends '(company-capf company-yasnippet))
;; (defun doulii/tab-complete ()
;;   (interactive)
;;   (cond
;;    ;; 1. snippet 展开 / 跳字段
;;    ((and (bound-and-true-p yas-minor-mode)
;;          (or (yas-expand)
;;              (yas-next-field))))
;;    ;; 2. company 补全
;;    ((company-manual-begin))
;;    ;; 3. fallback
;;    (t
;;     (indent-for-tab-command))))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  (company-backends '((company-capf :with company-yasnippet)))
  (company-transformers '(company-sort-by-occurrence))
  )

;; :bind (:map company-active-map
;;        ("<tab>" . company-complete-selection))
;;       (:map lsp-mode-map
;;        ("<tab>" . company-indent-or-complete-common))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package treemacs
  :defer t
  :config
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  ;; 解决 perspectives 调用 treemacs hook 参数不匹配问题
  ;; 待treemacs修复
  (when (fboundp 'treemacs--remove-treemacs-window-in-new-frames)
    (remove-hook 'persp-activated-functions #'treemacs--remove-treemacs-window-in-new-frames)
    (add-hook 'persp-activated-functions
              (lambda (location-type _frame-or-window _persp)
                (when (eq location-type 'frame)
                  (treemacs--remove-treemacs-window-in-new-frames location-type))))))
(use-package treemacs-evil :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))
;; :hook (projectile-after-switch-project-hook . treemacs-display-current-project-exclusively))

(use-package treemacs-icons-dired :hook (dired-mode . treemacs-icons-dired-enable-once))
(use-package treemacs-magit :after (treemacs magit))

;; (use-package neotree)
;; (global-set-key (kbd "C-c f e") 'neotree-toggle)

(defun doulii/neotree-follow ()
  "Auto update neotree to follow current file."
  (when (neo-global--window-exists-p)
    (neotree-find buffer-file-name)))
;; (when (and (neo-global--window-exists-p)
;;            buffer-file-name)
;;   (neotree-find buffer-file-name)))

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons))
  (setq neo-autorefresh t)
  (setq neo-smart-open t))
;; (setq projectile-switch-project-action #'neotree-projectile-action))
;; :hook
;; (buffer-list-update . doulii/neotree-follow))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(defun doulii/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (lsp-enable-which-key-integration))

(defun doulii/lsp-completion-hook ()
  (when lsp-completion-mode
    (set (make-local-variable 'company-backends)
         (remq 'company-capf company-backends))))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-completion-provider :none)
  :config
  ;; ignore golang stdlib
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]libexec[/\\\\]")
  :hook ((go-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (lua-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (dart-mode . lsp-deferred)
         (meson-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (vue-mode . lsp-deferred)
         (lsp-mode . doulii/lsp-mode-setup)))
;; (lsp-completion-mode . doulii/lsp-completion-hook)

;; (scheme-mode . lsp-deferred)
;; (lsp-mode . lsp-enable-which-key-integration)))
;;  :config (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :config (setq lsp-ui-imenu-auto-refresh t)
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; (use-package origami)
(use-package lsp-origami
  :hook (lsp-after-open lsp-origami-try-enable))

(use-package dap-mode)
;; :config (dap-auto-configure-mode)
;;  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))

(use-package flycheck
  :ensure t
  :hook
  (after-init #'global-flycheck-mode))
;; :config
;; (add-hook 'after-init-hook #'global-flycheck-mode))

(setq-default c-basic-offset 4)

(require 'dap-cpptools)

;; (use-package clang-format) ;; replaced by lsp/clangd
;; (use-package cmake-mode)

(use-package meson-mode)

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local comment-start "// ")
            (setq-local comment-end "")))

(use-package auto-virtualenvwrapper)
;; :hook
;;  (python-base-mode auto-virtualenvwrapper-activate)
;;  (window-configuration-change auto-virtualenvwrapper-activate)
;;  (focus-in auto-virtualenvwrapper-activate))

(use-package pet
  :after (auto-virtualenvwrapper)
  :config
  (add-hook 'python-base-mode-hook
            (lambda ()
              (auto-virtualenvwrapper-activate) ; activate before pet-mode
              (pet-mode))
            -10))
;; (add-hook 'python-base-mode-hook 'pet-mode -10))
;; :hook (python-base-mode . pet-mode)) ; depth -10

(require 'dap-python)
(setq dap-python-debugger 'debugpy)

(use-package go-mode)
;; (add-hook 'go-mode-hook 'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(defun doulii/buf-generate ()
  "run buf generate for proto"
  (interactive)
  (shell-command "buf generate"))
(use-package protobuf-mode
  :config (setq c-basic-offset 2)
  :bind (("C-c b" . 'doulii/buf-generate)))
;;(global-set-key (kbd "C-c b") 'doulii/buf-generate)

;; (use-package dap-dlv-go)
(require 'dap-dlv-go)

(use-package go-playground)

(use-package lua-mode)

(use-package typescript-mode
  :hook
  (typescript-mode . (lambda ()
                       (setq indent-tabs-mode nil
                             tab-width 2
                             typescript-indent-level 2))))

(require 'dap-chrome)
;; run dap-chrome-setup

(use-package vue-mode)
;; :config
;; (setq js-indent-level 2)        ;; JS 缩进
;; (setq css-indent-offset 2)     ;; CSS 缩进
;; (setq web-mode-markup-indent-offset 2) ;; HTML 缩进
;; (setq web-mode-code-indent-offset 2)   ;; JS in HTML 缩进
;; (setq web-mode-css-indent-offset 2))   ;; CSS in HTML 缩进
;; :hook
;; (mmm-mode . (lambda () (set-face-background 'mmm-default-submode-face "#fafafa"))))
;; (mmm-mode-hook . (lambda () (set-face-background 'mmm-default-submode-face nil))))

(use-package prettier
  :config
  ;; (setq prettier-enabled-parsers (angular babel babel-flow babel-ts css elm espree flow graphql html java json json5 json-stringify less lua markdown mdx meriyah php postgresql pug python ruby scss sh solidity svelte swift toml typescript vue xml yaml))
  (setq prettier-enabled-parsers '(angular babel babel-flow babel-ts css elm espree flow graphql html java json-stringify less mdx meriyah php postgresql pug ruby scss sh solidity svelte swift typescript vue xml))
  (global-prettier-mode))

(use-package dart-mode)
(use-package lsp-dart
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)))

(when (file-directory-p "~/quicklisp")
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))
;; Replace "sbcl" with the path to your implementation

(use-package paredit
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (scheme-mode . enable-paredit-mode))
(use-package enhanced-evil-paredit
  :hook
  (paredit-mode . enhanced-evil-paredit-mode))

(use-package geiser-guile
  :config
  (setq process-environment
        (append '("LANG=en_US.UTF-8" "LC_ALL=en_US.UTF-8")
                process-environment)))
;; (setq geiser-guile-binary "guile-3.0")

;; lsp
(use-package lsp-scheme
  :hook
  (scheme-mode . lsp-scheme)
  :config
  (setq lsp-scheme-implementation "guile"))

;; (require 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp)
(use-package lsp-java
  :hook (java-mode . lsp))

(use-package yaml-mode)

(use-package sqlite-mode
  :config
  (defun doulii/sqlite-view-file-magically ()
    "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it."
    (require 'sqlite-mode)
    (let ((file-name buffer-file-name))
      (kill-current-buffer)
      (sqlite-mode-open-file file-name)))

  (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . doulii/sqlite-view-file-magically)))

(use-package auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package dockerfile-mode
  :ensure t)

;; (use-package ebuild-mode)
(when (require 'ebuild-mode nil 'noerror)
  ;; language-server
  ;; https://termux-language-server.readthedocs.io/en/latest/resources/configure.html
  ;; TODO: 未验证
  (make-lsp-client
   :new-connection (lsp-stdio-connection
					`(,(executable-find "termux-language-server")))
   :activation-fn (lsp-activate-on "build.sh" "*.subpackage.sh" "PKGBUILD"
                                   "*.install" "makepkg.conf" "*.ebuild" "*.eclass" "color.map" "make.conf")
   :server-id "termux"))

;; dependency
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))
(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

(defun my-claude-display-right (buffer)
  "Display Claude buffer in right side window."
  (display-buffer buffer '((display-buffer-in-side-window)
                           (side . right)
                           (window-width . 75))))
(add-to-list 'display-buffer-alist
             '("^\\*claude"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 75)))
;; install claude-code.el
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (setq claude-code-terminal-backend 'vterm)
  ;; (setq claude-code-display-window-fn #'my-claude-display-right)

  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (doulii/set-evil-keymap "c" claude-code-command-map)
  ;; (claude-code-mode)
  ;; :bind-keymap ("<leader> c" . claude-code-command-map)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  ;; :bind
  ;; (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  )

(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-output-type 'png)
  )

;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;; (use-package eaf
;;   :init ((setq eaf-python-command "~/.emacs.d/site-lisp/emacs-application-framework/venv/bin/python3"))
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework")
;; :custom
;; (eaf-browser-continue-where-left-off t)
;; (eaf-browser-enable-adblocker t)
;; (browse-url-browser-function 'eaf-open-browser)
;; :config
;; (defalias 'browse-web #'eaf-open-browser)
;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;; (eaf-bind-key nil "M-q" eaf-browser-keybinding))
;; unbind, see more in the Wiki

;; (require 'eaf-demo)
;; (require 'eaf-music-player)
;; (require 'eaf-2048)
;; (require 'eaf-terminal)
;; (require 'eaf-image-viewer)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-file-browser)
;; (require 'eaf-mindmap)
;; (require 'eaf-video-player)
;; (require 'eaf-org-previewer)
;; (require 'eaf-netease-cloud-music)
;; (require 'eaf-system-monitor)
;; (require 'eaf-pyqterminal)
;; (require 'eaf-markmap)

(use-package lorem-ipsum)

(add-to-list 'load-path
             (concat user-emacs-directory "my-plugins")
             t)
(require 'my-plugin)
;; :load-path (concat user-emacs-directory "my-plugins")

;; C-c l 通常用于lsp
(use-package line-edit
  :load-path load-path
  :config
  (doulii/set-evil-keymap "l" line-edit-command-map))
;; :bind-keymap ("C-c l" . line-edit-command-map)

;; (evil-define-state line-edit
;;   "My custom evil state."
;;   :tag " <L> "
;;   :message "-- MY STATE --"
;;   :enable (normal)          ;; 继承 normal state 的行为
;;   :keymap (make-sparse-keymap))
;; (evil-define-key 'line-edit evil-line-edit-state-map
;;   (kbd "j") #'line-edit-next-line
;;   (kbd "k") #'line-edit-previous-line)

;; (use-package my-plugin
;;   :load-path (concat user-emacs-directory "my-plugins/my-plugin.el"))
