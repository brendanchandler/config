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
(provide 'init-packages)

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
  )

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

(use-package gptel
  :config
  (setq gptel-model "gpto1"
        gptel-backend
        (gptel-make-openai "Argo"
          :host "argo-bridge.cels.anl.gov"  ; your custom host
          :endpoint "/chat/completions"   ; standard OpenAI endpoint
          :stream t
          :key ""                ; your API key
          :models '("gpto1"))))

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
