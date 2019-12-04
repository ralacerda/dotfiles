(use-package org
  :ensure org-plus-contrib
  :config
  (require 'org-tempo)
  (require 'ox-latex))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted) 

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-startup-indented t)

(setq initial-major-mode 'org-mode)

(use-package org-bullets
  :config (setq org-bullets-bullet-list '("▶" "◉" "●" "○"))
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-ellipsis " ⤵")

(add-to-list 'org-structure-template-alist
             '("el" . "src emacs-lisp"))

(setq org-src-window-setup 'current-window)

(setq org-src-fontify-natively t)

(setq org-directory "~/Dropbox/Journal")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-tags-column 40)

(setq org-cycle-separator-lines 0)

(setq org-inbox-file (org-file-path "inbox.org"))
(setq org-index-file (org-file-path "index.org"))
(setq org-someday-file (org-file-path "someday.org"))

(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(setq org-agenda-files (list
                        (org-file-path "inbox.org")
                        (org-file-path "index.org")
                        (org-file-path "tickler.org")))

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(setq org-agenda-span 14)
(setq org-agenda-start-on-weekday nil)

(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 ;; (agenda . " %i %?-12t% s")
                                 (todo . " %i %-12:c")
                                 (tags . " %i ")
                                 (search . " %i ")))

;; Visual lines on agenda mode are kind of annoying
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))

(defun hrs/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(setq org-log-redeadline 'time)

(setq org-log-reschedule 'time)

(setq org-log-done 'time)

(setq org-capture-templates
         '(("t" "Todo"
           entry
           (file org-inbox-file)
           "* TODO %?\n")
           ("e" "Event"
           entry
           (file org-inbox-file)
           "* %? \n %^T \n")))

(defun open-inbox-file ()
  "Open my inbox file"
  (interactive)
  (find-file org-inbox-file)
  (end-of-buffer))

(defun open-index-file ()
  "Open the master index file."
  (interactive)
  (find-file org-index-file)
  (end-of-buffer))

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)
        (org-someday-file :maxlevel . 3)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "DELAYED" "DONE")))

(setq org-agenda-custom-commands
      '(("o" tags-todo "Work")
        ("l" "Agenda and lab related tasks"
         ((agenda)
          (tags-todo "LAB")))
        ("w" "Only tasks relate to my professor advisor"
         tags-todo "ADVISOR"
         ((org-agenda-skip-function '(org-agenda-skip-entry-if
                                     'todo '("WAITING")) )))))
