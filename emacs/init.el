(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
                                        ;
;This is only needed once, near the top of the file
(eval-when-compile
;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "~/.config/emacs/lisp/")
  (require 'use-package))


;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" "~/src/config/emacs"))
(require 'init-packages)
(require 'custom-functions)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-all-windows nil)
 '(backup-directory-alist '((".*" . "~/emacs-backup")))
 '(c-basic-offset 4)
 '(c-default-style "stroustrup")
 '(cursor-type t)
 '(custom-enabled-themes '(modus-operandi))
 '(dired-dwim-target 'dired-dwim-target-next)
 '(display-line-numbers t)
 '(evil-default-state 'insert)
 '(gmm-tool-bar-style 'gnome t)
 '(gud-pdb-command-name "/C2/conda/envs/aux/bin/python3 -m pdb")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-move-visual nil)
 '(mode-require-final-newline t)
 '(org-agenda-files '("~/org/"))
 '(org-agenda-log-mode-items '(closed clock state))
 '(org-agenda-start-with-log-mode 'only)
 '(org-capture-templates
   '(("n" "Note" entry (file "~/org/notes.org") "* %?\12%i\12%a"
      :time-prompt t)
     ("t" "Task" entry (file "~/org/tasks.org") "* TODO %?\12%i\12%a"
      :empty-lines 1 :clock-in t :time-prompt t)))
 '(org-default-notes-file "~/org/notes.org")
 '(org-goto-auto-isearch nil)
 '(org-stuck-projects '("+LEVEL=2/-DONE" ("NEXT") nil ""))
 '(org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "DELEGATED(l)" "VERIFY(v)" "|"
               "CANCELLED(c)" "DONE(d)")))
 '(package-selected-packages
   '(editorconfig god-mode evil lsp-ui clipetty--dcs-end clipetty company
                  lsp-treemacs flycheck which-key vertico use-package
                  orderless markdown-mode magit avy))
 '(python-fill-docstring-style 'pep-257)
 '(python-indent-def-block-scale 4)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-guess-indent-offset-verbose nil)
 '(require-final-newline t)
 '(show-paren-style 'parenthesis)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Adwaita Mono" :foundry "UKWN" :slant normal :weight regular :height 120 :width normal)))))
