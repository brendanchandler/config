; This is only needed once, near the top of the file
(eval-when-compile
;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "~/.config/emacs/lisp/")
  (require 'use-package))


(define-prefix-command 'filesystem-map)
(define-key global-map (kbd "C-c C-f") filesystem-map)

(define-prefix-command 'search-map)
(define-key global-map (kbd "C-c C-j") search-map)

(define-prefix-command 'o-map)
(define-key global-map (kbd "C-o") o-map)

(use-package emacs
  :ensure t
  :bind (("C-c f t" . (lambda () (interactive)
			(find-file "/ssh:bchandler@tesseract:/home/phoebus/BCHANDLER/")))
	 ("C-c f w" . (lambda () (interactive)
			(find-file "/ssh:bchandler@weed:/home/bchandler/")))
	 ("C-c f h" . (lambda () (interactive)
			(find-file "/ssh:bchandler@voltctl:/home/helios/BCHANDLER/")))
	 ("C-c f i" . (lambda () (interactive)
			(find-file user-init-file)))
	 ("C-c c f r" . (lambda () (interactive)
			  (load-file user-init-file)))
	 ("C-o e" . hippie-expand)
	 ("M-i" . imenu)
	 ("M-h" . windmove-left)
	 ("M-l" . windmove-right)
	 ("M-j" . windmove-down)
	 ("M-k" . windmove-up)
	 ("C-x C-o" . tab-next)
	 ("C-c m" . (lambda () (interactive)
		      (occur "Section:")))
	 ("M-e" . forward-to-word)
	 ("C-x C-b" . ibuffer)
	 ("M-n" . move-line-down)
	 ("M-p" . move-line-up)
	 ("C-c w" . bc/vertical-windows)
	 )
  :config
  (context-menu-mode)
  (windmove-default-keybindings)
  (global-unset-key (kbd "C-z"))
  
  (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
  (defun my-goto-match-beginning ()
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))

  (defadvice isearch-exit (after my-goto-match-beginning activate)
    "Go to beginning of match."
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))
  )


;; (use-package clipetty--dcs-end
;;   :ensure t
;;   :config
;;   (clipetty-mode))

(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(use-package editorconfig
  :ensure t)

;; Copilot configurations
(use-package copilot
  :load-path (lambda () (expand-file-name "copilot.el" user-emacs-directory))
  :diminish
  :bind
  (("C-<return>" . 'copilot-accept-completion))
  :config
  (global-copilot-mode)
  (add-to-list 'copilot-indentation-alist '(prog-mode 4))
  (add-to-list 'copilot-indentation-alist '(cc-mode 4))
  (add-to-list 'copilot-indentation-alist '(org-mode 4))
  (add-to-list 'copilot-indentation-alist '(text-mode 4))
  (add-to-list 'copilot-indentation-alist '(closure-mode 4))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 4))
  (add-hook 'prog-mode-hook 'copilot-mode))


(use-package avy
  :ensure t
  :bind ("M-o" . avy-goto-char))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))
(use-package which-key
  :ensure t
  :init
  (which-key-mode t))

(use-package magit
  :ensure t)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))
  
(use-package cc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'ibuffer-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-mode 1))

(defun bc-next-buffer ()
  (interactive)
  "Open ido or switch buffers"
  (if (eq major-mode 'minibuffer-inactive-mode)
      (ido-next-match)
    (ido-switch-buffer)))

(defun bc-prev-buffer ()
  (interactive)
  "Switch to previous buffer"
  (if (eq major-mode 'minibuffer-inactive-mode)
      (ido-prev-match)
    (ido-switch-buffer)))

(defun bc-open-emacs-init ()
  "Open EMACS init.el."
  (interactive)
  (find-file "~/.config/emacs/init.el"))
(defun bc-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun bc-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun bc-decrement-number-decimal (&optional arg)
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -1)))


(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode)))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (cc-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))


;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; (use-package eglot-mode
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (c++-mode . eglot)
;; 	 (c-mode . eglot)
;; 	 (python-mode . eglot)))

;;(add-hook 'c-mode-hook 'eglot)
;;(add-hook 'c++-mode-hook 'eglot)
;;(add-hook 'python-mode-hook 'eglot)

(defun bc/term-toggle-mode ()
  "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(defun bc/vertical-windows ()
  "Sets up 3 vertical, balanced windows"
  (interactive)
  (progn (delete-other-windows)
       (split-window-right)
       (split-window-right)
       (balance-windows)))

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
 '(display-line-numbers 'relative)
 '(evil-default-state 'insert)
 '(gmm-tool-bar-style 'gnome t)
 '(gud-pdb-command-name "/C2/conda/envs/aux/bin/python3 -m pdb")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-move-visual nil)
 '(mode-require-final-newline t)
 '(org-agenda-files '("~/org/notes.org"))
 '(org-agenda-start-with-log-mode 'only)
 '(org-capture-templates
   '(("n" "Notes Entry" entry
      (file+headline "" "Notes")
      "" :time-prompt t)
     ("t" "Tasks Entry" checkitem
      (file+headline "~/org/tasks.org" "Tasks")
      "" :clock-in t :time-prompt t)))
 '(org-default-notes-file "~/org/notes.org")
 '(org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "INTR(i)" "PROG(p)" "DONE(d)")))
 '(package-selected-packages
   '(editorconfig god-mode evil lsp-ui clipetty--dcs-end clipetty company lsp-treemacs flycheck which-key vertico use-package orderless markdown-mode magit avy))
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
(put 'erase-buffer 'disabled nil)

  (defun bc/insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;; Treat underscores as part of a word, not a word separator.
;; Lets evil * and other movement keys work as expected
(modify-syntax-entry ?_ "w")

(put 'upcase-region 'disabled nil)

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))
