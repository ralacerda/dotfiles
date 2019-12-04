;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("gnu"
. "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa"
. "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org"
. "http://orgmode.org/elpa/") t)
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that
;; this is a fresh installation! So we'll want to update the package
;; repository and install use-package before loading the literate
;; configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Always install packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;--------------------------------------------------------------------
;; My functions
;;--------------------------------------------------------------------

(defun my-scroll-down ()
  (interactive)
  (scroll-up-command 5))

(defun my-scroll-up ()
  (interactive)
  (scroll-down-command 5))

(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(defun list-dirs-recursively (dir &optional include-symlinks)
    "Return list of all subdirectories recursively. Returns absolute paths.
Optionally call recursively on symlinks."
    (let ((result nil)
          (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
      (dolist (file (file-name-all-completions "" dir))
        (when (and (directory-name-p file) (not (member file '("./" "../"))))
          (setq result (nconc result (list (expand-file-name file dir))))
          (let* ((leaf (substring file 0 (1- (length file))))
                 (full-file (expand-file-name leaf dir)))
            ;; Don't follow symlinks to other directories.
            (unless (and (file-symlink-p full-file) (not include-symlinks))
              (setq result
                    (nconc result (list-dirs-recursively full-file)))))
          ))
      result))

(defun load-init ()
  "This will load the init.el"
    (interactive)
  (find-file "~/.emacs.d/init.el"))

(add-hook 'overwrite-mode-hook
          (lambda ()
            (setq cursor-type (if overwrite-mode 'hollow 'box))))

(defun hrs/rename-file (new-name)
  (interactive "FNew name: ")
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (when (buffer-modified-p)
             (save-buffer))
          (rename-file filename new-name t)
          (kill-buffer (current-buffer))
          (find-file new-name)
          (message "Renamed '%s' -> '%s'" filename new-name))
      (message "Buffer '%s' isn't backed by a file!" (buffer-name)))))

(defun hrs/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

;;--------------------------------------------------------------------
;; Sane Defaults
;;--------------------------------------------------------------------

;; Default behaviour is to ask when opening symlinks
(setq vc-follow-symlinks t)

;; When scrolling down, keep point in the same position
(setq scroll-preserve-screen-position 'always)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(unless (file-readable-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; If the file has a shebang, mark is as executable
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)


(setq-default create-lockfiles nil)

(setq-default enable-recursive-minibuffers t)

;; Display column number in mode line.
(setq-default column-number-mode t)

(setq sentence-end-double-space nil)

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does
                           not exist. Create it?" dir)))
                  (make-directory dir t))))))

(global-auto-revert-mode t)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(global-font-lock-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq require-final-newline t)

(setq vc-make-backup-files t)

(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)

(setq locale-coding-system 'utf-8) 
(set-terminal-coding-system 'utf-8) 
(set-keyboard-coding-system 'utf-8) 
(set-selection-coding-system 'utf-8) 
(prefer-coding-system 'utf-8)

(setq save-interprogram-paste-before-kill t)

(setq backup-directory-alist `(("." . "~/.emacs-saves")) ; Backups folder
      backup-by-copying t   ; Set backup by copying (safer)
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 10
      version-control t)    ; Backup even if there is version control

(show-paren-mode 1)
(setq show-paren-delay 0.0)

(setq inhibit-eol-conversion t)

;; When I end my Isearch, I want to back to what I typed
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(defun my-goto-match-beginning ()
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
    "Go to beginning of match."
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))

(put 'narrow-to-region 'disabled nil)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Start the server
;; (server-start)

(save-place-mode 1)
;;--------------------------------------------------------------------
;; Visual Stuff
;;--------------------------------------------------------------------

(set-face-attribute 'default nil :height 130)

(setq display-line-numbers-width 3)
(setq display-line-numbers-width-start t)

;; When emacs starts, go fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set the title to the buffer name
(setq frame-title-format '((:eval (buffer-name))))

;; Highlight the current line
(global-hl-line-mode)

;; If the mark is active, and you start typing, delete region
(delete-selection-mode t)

;; Visually show the mark
(transient-mark-mode t)

;; When starting a programming mode, show line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; This is confusing
(setq split-width-threshold 80)
(setq split-height-threshold nil)

;; Hide scrollabar, tool bar, menu bar
(scroll-bar-mode  0)
(tool-bar-mode  0)
(menu-bar-mode   0)

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1))

(setq mouse-wheel-follow-mouse 't)

(setq scroll-margin 5)
(setq scroll-conservatively 10000)

;;--------------------------------------------------------------------
;; Packages
;;--------------------------------------------------------------------

(use-package mwim)

(use-package color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-eighties))

(use-package rainbow-delimiters
:hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package dashboard
  :config
  (setq dashboard-items '((recents  . 15)
                        (bookmarks . 5)
                        (projects . 5)))
  (dashboard-setup-startup-hook))

(use-package ivy 
  :config  
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 20)
  (setq ivy-fixed-height-minibuffer nil)
  (ivy-mode 1)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package iy-go-to-char
  :bind
  (("C-t" . 'iy-go-up-to-char)
  ("C-S-t" . 'iy-go-up-to-char-backward)))

(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

(use-package counsel
  :bind
  ("M-x" . 'counsel-M-x)
  :config
  (setq ivy-initial-inputs-alist nil)
  (use-package smex)
  (counsel-mode 1)
  (let ((done (where-is-internal #'ivy-done     ivy-minibuffer-map t))
        (alt  (where-is-internal #'ivy-alt-done ivy-minibuffer-map t)))
    (define-key counsel-find-file-map done #'ivy-alt-done)
    (define-key counsel-find-file-map alt  #'ivy-done)))

(use-package avy
  :bind ("C-+" . avy-goto-word-1)
  :bind ("C-*" . avy-goto-char)
  :bind ("C-q" . avy-goto-word-1)

  :config
  (setq avy-all-windows nil))

;;---------------------------------------------------
;; Programing modes
;;---------------------------------------------------

(use-package arduino-mode)

;;--------------------------------------------------------------------
;; Org Stuff
;;--------------------------------------------------------------------

(load "~/emacs.d/org.el")

;;--------------------------------------------------------------------
;; More packages
;;--------------------------------------------------------------------

(use-package highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces))
    
(use-package flycheck)

(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region))

(use-package markdown-mode
  :defer t
  )

(use-package restart-emacs
:bind ("C-c e r" . 'restart-emacs))

(use-package which-key
  :defer t
  :config
  (which-key-mode))

(use-package magit
  :defer t
  :bind
  ("C-x g" . magit-status))

(use-package yasnippet
  :config
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode))

(use-package git-gutter
  :defer t
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:update-interval 1))

;;--------------------------------------------------------------------
;; Keybinds
;;--------------------------------------------------------------------

(global-unset-key "\C-z")
(global-set-key (kbd "C-z")  'dabbrev-expand)
(global-set-key (kbd "C-S-z")  'hippie-expand)

(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(global-set-key (kbd "M-Z") 'zap-up-to-char)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-S-o") (lambda () (interactive) (switch-to-buffer
nil)))

(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)

(global-set-key (kbd "\C-xk") 'hrs/kill-current-buffer)

(global-set-key (kbd "C-v") 'my-scroll-down)
(global-set-key (kbd "M-v") 'my-scroll-up)

(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)

(use-package general)

;; General commands for files
(general-define-key
:prefix "C-c f"
 "d" 'load-init :which-key "Emacs init.el"
 "o" 'browse-file-directory :which-key "Open in dolphin"
 "c" 'load-config :which-key "Open configuration.org"
 "i" 'open-index-file :which-key "Open my Index TODO List"
 "b" 'open-inbox-file :which-key "Open inbox file"
 "w" 'open-work-file :which-key "Open my Work TODO List" 
 "a" 'org-agenda :which-key "Open my Agenda")

(general-def org-mode-map
  "C-c C-x C-s" 'hrs/mark-done-and-archive :which-key "Mark as
  done and archive")

;; Org capture
(general-define-key
 :prefix "C-c"
 "l" 'org-store-link
 "c" 'org-capture)
