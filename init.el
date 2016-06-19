;; col numbers
(setq column-number-mode t)

;; tabs
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 2 200 2))
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

;; file backups
(add-to-list 'load-path "~/.emacs.d/elpa/backup-each-save-20130704.732/")
(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

;; trailing whitespace
;; C
(add-hook 'c-mode-hook
                (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
;; C++
(add-hook 'c++-mode-hook
                (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
;; OCaml
(add-hook 'tuareg-mode-hook
                (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
                (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

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

;; don't auto-indent new lines
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; minibuffer autocomplete
(icomplete-mode 99)

;; i forget what this does
(set-face-attribute 'vertical-border
                    nil
                    :foreground "gray")

;; melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; programming languages
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



;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe-mode 0 nil (fringe))
 '(linum-format (quote dynamic))
 '(tool-bar-mode nil))
