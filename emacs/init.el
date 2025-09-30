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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
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
 '(custom-enabled-themes '(tango ##))
 '(dired-dwim-target 'dired-dwim-target-next)
 '(display-line-numbers t)
 '(gmm-tool-bar-style 'gnome t)
 '(gud-pdb-command-name "/C2/conda/envs/aux/bin/python3 -m pdb")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-move-visual nil)
 '(mode-require-final-newline t)
 '(org-agenda-files '("~/org/notes.org"))
 '(org-agenda-start-with-log-mode 'only)
 '(org-capture-templates
   '(("n" "Notes Entry" entry (file+headline "" "Notes") "" :time-prompt
      t)
     ("t" "Tasks Entry" checkitem
      (file+headline "~/org/tasks.org" "Tasks") "" :clock-in t
      :time-prompt t)))
 '(org-default-notes-file "~/org/notes.org")
 '(org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "INTR(i)" "PROG(p)" "DONE(d)")))
 '(package-selected-packages
   '(avy clang-format clipetty clipetty--dcs-end company compat
         editorconfig evil flycheck gptel lsp-treemacs lsp-ui magit
         markdown-mode orderless rust-mode use-package vertico
         which-key))
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
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 90 :width normal)))))
