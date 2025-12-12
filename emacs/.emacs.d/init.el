(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
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
(global-display-line-numbers-mode t)

;; 某些mode下禁用line numbers
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                shell-mode-hook
                eshel-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; 中文line break
(setq word-wrap-by-category t)

;; (set-face-attribute 'default nil :font "Fira Code Retina" :height 280)
;; (set-face-attribute 'default nil :font "WenQuanYi Zen Hei Mono" :height 160)
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 150)
;; (set-face-attribute 'default nil :font "YaHei Consolas Hybrid" :height 150)
;; (add-to-list 'default-frame-alist '(font . "Yahei Consolas Hybrid-12"))
;; (add-to-list 'default-frame-alist '(font . "WenQuanYi Zen Hei"))
;; (add-to-list 'default-frame-alist '(font . "WenQuanYi Zen Hei Mono-16"))
;; (add-to-list 'default-frame-alist '(font . "YaHei Consolas Hybrid-16"))

;; enable by `M-x variable-pitch-mode`
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height 150)
(set-face-attribute 'variable-pitch nil :font "YaHei Consolas Hybrid" :height 150 :weight 'regular)

(set-locale-environment "zh_CN.UTF-8")

(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

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
  :init (load-theme 'doom-one t))
  ;; :init (load-theme 'doom-one-light t))
  ;; :init (load-theme 'doom-monokai-machine t))
  ;; :init (load-theme 'doom-tomorrow-day t))
  ;; :init (load-theme 'doom-opera-light t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ivy buffer completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . iv-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package counsel
  ;; :bind (("M-x" . counsel-M-x)
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq counsel-find-file-ignore-regexp "~$|\\.cache/"))

;; this is a test

(use-package which-key
  ;; :init (which-key-mode)
  ;; :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
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
(defun aq/set-evil-key (s f)
  (evil-define-key 'normal 'global (kbd (concat "<leader>" s)) f))

(defun aq/set-evil-keymap (s map)
  (map-keymap
   (lambda (event binding)
     (when (commandp binding)
       ;; event 可能是 number 或 symbol，要转换成字符串
       (let ((key-str (single-key-description event)))
         (aq/set-evil-key (concat s key-str) binding))))
   map))

;; origami toggle使用origami-forward-toggle-node
(defun aq/evil-fold-origami-forward (mode-actions)
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
(defun aq/toggle-treemacs ()
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
  (setq evil-fold-list (mapcar #'aq/evil-fold-origami-forward evil-fold-list))
  (evil-mode 1)
  ;; (evil-set-leader '(normal motion) ";")
  (evil-set-leader 'normal ";")
  ;; (aq/set-evil-key "ee" 'treemacs)
  (aq/set-evil-key "ee" 'aq/toggle-treemacs)
  (aq/set-evil-key "ewe" 'treemacs-edit-workspaces)
  (aq/set-evil-key "es" 'treemacs-switch-workspace)
  (aq/set-evil-key "en" 'neotree-toggle)
  (aq/set-evil-key "q" 'quit-window)
  (aq/set-evil-key "x" 'delete-window)
  (aq/set-evil-key "k" 'kill-buffer)
  (aq/set-evil-key "b" 'counsel-ibuffer)
  (aq/set-evil-key "s" 'save-buffer)
  (aq/set-evil-key "dd" 'dap-debug-last)
  (aq/set-evil-key "dr" 'dap-debug-restart)
  (aq/set-evil-key "dq" 'dap-disconnect)
  (aq/set-evil-key "db" 'dap-breakpoint-toggle)
  (aq/set-evil-key "dc" 'dap-continue)
  (aq/set-evil-key "dn" 'dap-next)
  (aq/set-evil-key "di" 'dap-step-in)
  (aq/set-evil-key "do" 'dap-step-out)
  (aq/set-evil-key "SPC" 'ace-jump-word-mode)
  (aq/set-evil-key "jb" 'ace-jump-mode-pop-mark)
  (aq/set-evil-key "jc" 'ace-jump-char-mode)
  (aq/set-evil-key "jl" 'ace-jump-line-mode)
  (aq/set-evil-key "jw" 'ace-jump-word-mode)
  ;; (aq/set-evil-key "pg" 'go-playground)
  (aq/set-evil-key "p" 'persp-key-map)
  ;; (aq/set-evil-key "c" claude-code-command-map)
  (aq/set-evil-key "rb" 'revert-buffer)
  ;; (aq/set-evil-keymap "l" line-edit-command-map) ;; 需要package ready以后才能读取变量
  (aq/set-evil-key ";" 'evil-repeat-find-char))


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

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (dolist (var '("LANG" "LC_CTYPE" "LC_ALL"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(defun aq/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1))
    ;;  (setq evil-auto-indent nil))
    ;;  (auto-fill-mode 0)

(use-package org
	:hook (org-mode . aq/org-mode-setup)
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
	(set-face-attribute (car face) nil :font "FiraCode Nerd Font" :weight 'regular :height (cdr face)))

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
(defun aq/org-mode-visual-fill ()
	(setq visual-fill-column-width 120
	visual-fill-column-center-text t)
	(visual-fill-column-mode))
(use-package visual-fill-column
	:defer t
	:hook (org-mode . aq/org-mode-visual-fill))

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
	(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
	(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
	(add-to-list 'org-structure-template-alist '("go" . "src go"))
	(add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
	(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
	(add-to-list 'org-structure-template-alist '("py" . "src python")))

;; org mode (Refer: org mode guide)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; 自动展开加粗斜体等marker
(use-package org-appear
  :after org
  :config (setq org-appear-autolinks t)
	:hook (org-mode . org-appear-mode))

(use-package org-download
  :config
  (setq org-download-heading-lvl nil))



(defun aq/org-babel-tangle-config ()
  ;;  (when (string-equal (file-name-directory buffer-file-name)
  ;;                      (expand-file-name user-emacs-directory))
  (when (string-equal (file-name-nondirectory
                       (directory-file-name
                        (file-name-directory buffer-file-name)))
                      ".emacs.d")
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'aq/org-babel-tangle-config)))

;; 获取上次发版的版本号
(defun aq/application-publish-notification/get-last-version ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "/-/compare/[a-z0-9\\.]*\\.\\.\\.\\\([a-z0-9\\.]*\\\)\\\W" nil t 1)
    (match-string-no-properties 1)))

;; 生成发版时间，最早为两分钟以后，取整到5分钟的整数倍
(defun aq/application-publish-notification/publish-time ()
  (let ((time (decode-time (time-add (current-time) 120)))
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
    ;; (setq projectile-project-search-path '(("~/Projects" . 2))))
    (setq projectile-project-search-path '(("~/Projects" . 1)
                                           ("~/Projects/github" . 1))))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t)
  :config
  ;; add cmake sub project
  ;; https://github.com/bbatsov/projectile/issues/1130#issuecomment-1123237339
  (setq projectile-project-root-files-bottom-up
        (cons "meson.build"
              (cons "CMakeLists.txt" projectile-project-root-files-bottom-up)))
  (add-to-list 'projectile-ignored-projects "/opt/homebrew/")
  (add-to-list 'projectile-ignored-projects "~/"))

;;  (setq projectile-switch-project-action 'neotree-projectile-action))
(use-package counsel-projectile
  :config
  (projectile-known-projects) ;; counsel-projectile启动时不能正确list project，先临时fix，等修复 https://github.com/ericdanan/counsel-projectile/issues/189
  (counsel-projectile-mode))

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
  :config (yas-global-mode 1))
(use-package yasnippet-snippets
  :after yasnippet)

;; :config
  ;; (add-to-list 'company-backends '(company-capf company-yasnippet))
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
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

(defun aq/neotree-follow ()
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
;; (buffer-list-update . aq/neotree-follow))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(defun aq/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (lsp-enable-which-key-integration))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
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
         (lsp-mode . aq/lsp-mode-setup)))
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

(defun aq/buf-generate ()
  "run buf generate for proto"
  (interactive)
  (shell-command "buf generate"))
(use-package protobuf-mode
  :config (setq c-basic-offset 2)
  :bind (("C-c b" . 'aq/buf-generate)))
;;(global-set-key (kbd "C-c b") 'aq/buf-generate)

;; (use-package dap-dlv-go)
(require 'dap-dlv-go)

(use-package go-playground)

(use-package lua-mode)

(use-package typescript-mode)

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

(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

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
  (defun aq/sqlite-view-file-magically ()
    "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it."
    (require 'sqlite-mode)
    (let ((file-name buffer-file-name))
      (kill-current-buffer)
      (sqlite-mode-open-file file-name)))

  (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . aq/sqlite-view-file-magically)))

(use-package auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

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

  (aq/set-evil-key "c" 'claude-code-command-map)

  ;; (claude-code-mode)
  ;; :bind-keymap ("<leader> c" . claude-code-command-map)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  ;; :bind
  ;; (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
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

;; C-x C-e to execute sexp
;; C-h f describe-function

;; move customize to seperate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path
             (concat user-emacs-directory "my-plugins")
             t)
(require 'my-plugin)
  ;; :load-path (concat user-emacs-directory "my-plugins")

;; C-c l 通常用于lsp
(use-package line-edit
  :load-path load-path
  :config
  (aq/set-evil-keymap "l" line-edit-command-map))
  ;; :bind-keymap ("C-c l" . line-edit-command-map)

;; (use-package my-plugin
;;   :load-path (concat user-emacs-directory "my-plugins/my-plugin.el"))
