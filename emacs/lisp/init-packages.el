;;; init-packages.el --- elisp for loading and configuring Emacs plugins
;;
;; Author: Brendan Chandler <bchandler@anl.gov>
;; Maintainer: Brendan Chandler <bchandler@anl.gov>
;; Version: 0.1
;; Keywords: convenience, tools
;;
;;; Commentary:
;;
;; This file is an init file fragment for loading and configuring Emacs plugins.
;;
;;; Code:

;; Keymaps
(define-prefix-command 'filesystem-map)
(define-key global-map (kbd "C-c C-f") filesystem-map)

(define-prefix-command 'search-map)
(define-key global-map (kbd "C-c C-j") search-map)

(define-prefix-command 'o-map)
(define-key global-map (kbd "C-o") o-map)

;; Window Movement Keys
(define-minor-mode bren-mode
  "Minor mode for window movement keybindings."
  :lighter " Bren"
  :init-value t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-h") 'windmove-left)
            (define-key map (kbd "M-l") 'windmove-right)
            (define-key map (kbd "M-j") 'windmove-down)
            (define-key map (kbd "M-k") 'windmove-up)
            map))
(bren-mode 1)

;; Common Keybinding Prefixes:
;; C-c f :: file related operations
;; C-c s :: searching
(use-package emacs
  :ensure nil
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
     ("C-j" . duplicate-dwim)
	 ("C-x C-o" . tab-next)
	 ("M-e" . forward-to-word)
	 ("C-x C-b" . ibuffer)
     ("C-c f r" . recentf)
	 ("M-n" . move-line-down)
	 ("M-p" . move-line-up)
	 )
  :custom
  (column-number-mode t)
  (auto-save-default t)
  ;; Various custom options taken from emacs solo
  (completion-ignore-case t)
  (completions-detailed t)
  (delete-by-moving-to-trash t)
  (delete-pair-blink-delay 0)
  (delete-selection-mode t)
  (enable-recursive minibuffers t)
  (find-ls-option '("-exec ls -ldh {} +" . "-ldh"))  ; find-dired results with human readable sizes
  (global-goto-address-mode t)
  (help-window-select t)
  (inhibit-startup-message t)
  (create-lockfiles nil)
  (create-backup-files nil)
  (recentf-max-saved-items 300) ; default is 20
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  (tramp-copy-size-limit (* 2 1024 1024)) ;; 2MB
  (tramp-use-scp-direct-remote-copying t)
  (tramp-verbose 2)
  (tab-always-indent 'complete)
  (tab-width 4)
  (grep-command "rg -nS --no-heading ") ; TODO: make it dinamic check if ripgrep is available before setting it and if it costs too much of the init time
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".jj" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist" "O.{arch}"))
  :config
  (context-menu-mode)
  (global-unset-key (kbd "C-z"))
  (defun my-goto-match-beginning ()
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))
  (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

  ;; Don't indent opening brace in inline functions
  (defun my-c++-mode-hook ()
    (c-set-offset 'inline-open 0))

  (add-hook 'c++-mode-hook 'my-c++-mode-hook)
  )
  ;; We want auto-save, but no #file# cluterring, so everything goes under our config cache/
  (make-directory (expand-file-name "cache/auto-saves/" user-emacs-directory) t)
  (setq auto-save-list-file-prefix (expand-file-name "cache/auto-saves/sessions/" user-emacs-directory)
        auto-save-file-name-transforms `((".*" ,(expand-file-name "cache/auto-saves/" user-emacs-directory) t)))


(use-package tramp
  :config
  (setq tramp-connection-reuse nil)
  (customize-set-variable
   'tramp-ssh-controlmaster-options
   (concat
    "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
    "-o ControlMaster=auto -o ControlPersist=yes")))


(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style "Mozilla")
  (global-set-key (kbd "C-c i") 'clang-format-region)
  (global-set-key (kbd "C-c u") 'clang-format-buffer)
  (add-hook 'c-common-mode 'clang-format-buffer-on-save))

;; (use-package clipetty--dcs-end
;;   :ensure t
;;   :config
;;   (clipetty-mode))

;; (use-package s
;;   :ensure t)

;; (use-package dash
;;   :ensure t)

;; (use-package editorconfig
;;   :ensure t)

;; OPTIONAL configuration

 ;; Conditionally load site specific configuration from external file
(let ((site-config-file "~/.config/emacs/lisp/site-config.el"))
  (when (file-exists-p site-config-file)
      (load site-config-file)))

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
  :hook (prog-mode . hs-minor-mode)
  :ensure t)

;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-disable-insert-state-bindings nil)
;;   (setq evil-default-state 'emacs)
;;   :config
;;   (evil-set-initial-state 'term-mode 'emacs)
;;   (evil-set-initial-state 'shell-mode 'emacs)
;;   (evil-set-initial-state 'dired-mode 'emacs)
;;   (evil-set-initial-state 'magit-mode 'emacs)
;;   (evil-set-initial-state 'ibuffer-mode 'emacs)
;;   (evil-set-initial-state 'compilation-mode 'emacs)
;;   (evil-set-initial-state 'help-mode 'emacs)
;;   (evil-set-initial-state 'magit-status-mode 'emacs)
;;   (evil-set-initial-state 'magit-diff-mode 'emacs)
;;   (evil-set-initial-state 'magit-log-mode 'emacs)
;;   (evil-set-initial-state 'org-mode 'emacs)
;;   (evil-set-initial-state 'info-mode 'emacs)
;;   (evil-set-initial-state 'eshell-mode 'emacs)
;;   (evil-mode 1)
;;   (add-hook 'prog-mode-hook
;;             (lambda ()
;;               (evil-local-mode 1)
;;               (evil-normal-state))))

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

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))

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

(use-package rust-mode
  :ensure t
  :hook (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  :config
  (setq rust-format-on-save t))

(provide 'init-packages)
