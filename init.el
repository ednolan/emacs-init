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
(icomplete-mode 99)

;; remove vertical border between buffers
(set-face-attribute 'vertical-border
                    nil
                    :foreground "gray")

;; interpret files as utf-8 encoded by default
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; smaller frame size on my laptop
(if (window-system)
    (when (string= system-name "ed-centos7-mbp")
      (set-face-attribute 'default (selected-frame) :height 80)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (set-face-attribute 'default (selected-frame) :height 80)))))

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
  (set (make-local-variable 'c-basic-offset) 2))
(add-hook 'c-mode-hook 'setup-common)
(add-hook 'c-mode-hook 'setup-c-mode)
;; C++
(defun setup-c++-mode ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop))
(add-hook 'c++-mode-hook 'setup-common)
(add-hook 'c++-mode-hook 'setup-c++-mode)
;; OCaml
(add-hook 'tuareg-mode-hook 'setup-common)
;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'setup-common)
;; JavaScript
(defun setup-js-mode ()
  (set (make-local-variable 'js-indent-level) 2))
(add-hook 'js-mode-hook 'setup-common)
(add-hook 'js-mode-hook 'setup-js-mode)
;; HTML
(defun setup-html-mode ()
  (set (make-local-variable 'sgml-basic-offset) 2))
(add-hook 'js-mode-hook 'setup-common)
(add-hook 'js-mode-hook 'setup-html-mode)

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
;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var
   share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; Tuareg
(require 'tuareg)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode))
              auto-mode-alist))
;; Merlin
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)
;; Make company aware of merlin
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))
;; bind merlin-locate to C-c ;
(add-hook 'merlin-mode-hook (lambda () (local-set-key (kbd "C-c ;") 'merlin-locate)))
;; use ocp-indent
(require 'ocp-indent)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
