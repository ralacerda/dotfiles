;;; .doom.d/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here

;;-----------------------------------
;;  My functions
;------------------------------------

(defun my-scroll-down ()
  (interactive)
  (scroll-up-command 5))

(defun my-scroll-up ()
  (interactive)
  (scroll-down-command 5))

(global-set-key (kbd "C-v") 'my-scroll-down)
(global-set-key (kbd "M-v") 'my-scroll-up)
(global-set-key (kbd "C-x b") 'switch-to-buffer)

(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(add-hook 'overwrite-mode-hook
          (lambda ()
            (setq cursor-type (if overwrite-mode 'hollow 'box))))

(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(defun my-goto-match-beginning ()
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
    "Go to beginning of match."
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))

;;----------------------------
;; My Sane Defaults
;;----------------------------

(setq scroll-preserve-screen-position 'always)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(setq-default tab-width 4)

(setq backup-directory-alist `(("." . "~/.emacs-saves")) ; Backups folder
      backup-by-copying t   ; Set backup by copying (safer)
      kept-new-versions 10
      kept-old-versions 10
      version-control t)    ; Backup even if there is version control

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1))
(setq scroll-conservatively 101)

(setq scroll-margin 5)

(setq doom-theme 'doom-spacegrey)

(use-package! dashboard
  :config (dashboard-setup-startup-hook))
