;; font size
(set-face-attribute 'default (selected-frame) :height 80)

;; col numbers
(setq column-number-mode t)

;; tabs
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)
(setq js-indent-level 2)
(setq-default tab-width 2)
(setq-default tab-stop-list (number-sequence 2 200 2))
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

;; file backups
(add-to-list 'load-path "~/.emacs.d/elpa/backup-each-save-20130704.732/")
(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

;; major mode hooks
;; delete trailing whitespace; turn off electric indent
;; C
(add-hook 'c-mode-hook
                (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'c-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'c-mode-hook
          (lambda () (local-set-key (kbd "C-j") #'newline-and-indent)))
;; C++
(add-hook 'c++-mode-hook
                (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'c++-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "C-j") #'newline-and-indent)))
;; OCaml
(add-hook 'tuareg-mode-hook
                (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'tuareg-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'tuareg-mode-hook
          (lambda () (local-set-key (kbd "C-j") #'newline-and-indent)))
;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
                (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'emacs-lisp-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (local-set-key (kbd "C-j") #'newline-and-indent)))

;; annoying messages
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message "eddie")
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; cursor
(setq-default cursor-type 'bar)

;; line numbers
(global-linum-mode t)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

;; delete selection mode
(delete-selection-mode 1)

;; minibuffer autocomplete
(icomplete-mode 99)

;; remove vertical border between buffers
(set-face-attribute 'vertical-border
                    nil
                    :foreground "gray")

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

;; cleaner mode lines
(require 'diminish)

;; helper for mode remappings
(require 'bind-key)

;; programming languages

;; both ocaml and c++
;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              nil
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	company-backends                '((company-irony company-gtags))
	)
  :bind ("C-;" . company-complete-common)
  )

;; ocaml
;; -- common-lisp compatibility if not added earlier in your .emacs
(require 'cl)

;; -- Tuareg mode -----------------------------------------
;; Add Tuareg to your search path
(load "/home/eddie/.opam/4.03.0/share/emacs/site-lisp/tuareg-site-file")
(require 'tuareg)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode))
          auto-mode-alist))

;; -- opam and utop setup --------------------------------
;; Setup environment variables using opam
(dolist
   (var (car (read-from-string
           (shell-command-to-string "opam config env --sexp"))))
 (setenv (car var) (cadr var)))
;; Update the emacs path
(setq exec-path (split-string (getenv "PATH") path-separator))
;; Update the emacs load path
(push (concat (getenv "OCAML_TOPLEVEL_PATH")
          "/../../share/emacs/site-lisp") load-path)
;; merlin
     ;; Add opam emacs directory to the load-path
     (setq opam-share (substring (shell-command-to-string "opam config var
   share 2> /dev/null") 0 -1))
     (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
     ;; Load merlin-mode
     (require 'merlin)
     ;; Start merlin on ocaml files
     (add-hook 'tuareg-mode-hook 'merlin-mode t)
     (add-hook 'caml-mode-hook 'merlin-mode t)
     ;; Enable auto-complete
     (setq merlin-use-auto-complete-mode 'easy)
     ;; Use opam switch to lookup ocamlmerlin binary
     (setq merlin-command 'opam)
; Make company aware of merlin
(with-eval-after-load 'company
 (add-to-list 'company-backends 'merlin-company-backend))

; bind merlin-locate to C-c ;
(add-hook 'merlin-mode-hook (lambda () (local-set-key (kbd "C-c ;") 'merlin-locate)))

;; C++

;; == irony-mode ==
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe-mode 0 nil (fringe))
 '(linum-format (quote dynamic))
 '(tool-bar-mode nil))
