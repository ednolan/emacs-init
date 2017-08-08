;; col numbers
(setq column-number-mode t)

;; line numbers
(global-linum-mode t)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default tab-stop-list (number-sequence 2 200 2))

;; meta-arrow to move between buffers
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; cursor
(setq-default cursor-type 'bar)

;; annoying startup messages
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message "eddie")
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; delete selection mode
(delete-selection-mode 1)

;; minibuffer autocomplete
;; broken for some reason?
;; (icomplete-mode 1)

;; remove vertical border between buffers
(set-face-attribute 'vertical-border
                    nil
                    :foreground "gray")

;; interpret files as utf-8 encoded by default
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; custom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'mac-classic t)

;; major mode hooks
;; delete trailing whitespace
;; turn off electric indent
;; C-j for newline-and-indent
;; configure tabination

;; all
(defun setup-common ()
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (electric-indent-local-mode -1)
  (local-set-key (kbd "C-j") #'newline-and-indent))
;; C
(defun setup-c-mode ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (set (make-local-variable 'c-basic-offset) 4))
(add-hook 'c-mode-hook 'setup-common)
(add-hook 'c-mode-hook 'setup-c-mode)
;; C++
(defun setup-c++-mode ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop))
(add-hook 'c++-mode-hook 'setup-common)
(add-hook 'c++-mode-hook 'setup-c++-mode)
;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'setup-common)
;; Golang
(defun setup-go-mode ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'tab-width) 8))
(add-hook 'go-mode-hook 'setup-common)
(add-hook 'go-mode-hook 'setup-go-mode)
;; HTML
(defun setup-html-mode ()
  (set (make-local-variable 'sgml-basic-offset) 4))
(add-hook 'html-mode-hook 'setup-common)
(add-hook 'html-mode-hook 'setup-html-mode)
;; JavaScript
(defun setup-js-mode ()
  (set (make-local-variable 'js-indent-level) 4))
(add-hook 'js-mode-hook 'setup-common)
(add-hook 'js-mode-hook 'setup-js-mode)
;; LaTeX
(add-hook 'latex-mode-hook 'setup-common)
;; OCaml
(add-hook 'tuareg-mode-hook 'setup-common)
;; Rust
(add-hook 'rust-mode-hook 'setup-common)

;; package management
;; melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))
;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; file backups
(use-package backup-each-save
  :config
  (setq backup-inhibited t)
  (setq auto-save-default nil)
  (setq make-backup-files nil)
  (add-hook 'after-save-hook 'backup-each-save)
)

;; cleaner mode lines
(use-package diminish)

;; helper for mode remappings
(use-package bind-key)

;; programming languages

;; C++
;; CMake
; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

(autoload 'cmake-mode "~/.emacs.d/cmake-mode/cmake-mode.el" t)

;; Go
(use-package go-mode
  :ensure t
  :defer t
  :init
  (require 'go-mode)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
)

;; LaTeX
(use-package tex-mode
  :ensure auctex
  :defer t
  :init
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t)
)
;; set XeTeX mode in TeX/LaTeX
(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-command-default "XeLaTeX")
            (setq TeX-save-query nil)))

;; OCaml
;; tuareg
(load "/home/eddie/.opam/4.03.0/share/emacs/site-lisp/tuareg-site-file")
;; merlin
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))
;; use ocp-indent
(require 'ocp-indent)

;; Rust
(use-package rust-mode
  :ensure t
  :defer t
  :init
  (require 'rust-mode)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
)

;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("68b7d8301bf8121abb8a92bbe7c247fbc3e64a0adfdda534daefd18f18c44a55" default)))
 '(fringe-mode 0 nil (fringe))
 '(linum-format (quote dynamic))
 '(package-selected-packages
   (quote
    (autex-latexmk use-package rust-mode go-mode company-irony backup-each-save auctex-latexmk)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "PfEd" :slant normal :weight normal :height 109 :width normal)))))
