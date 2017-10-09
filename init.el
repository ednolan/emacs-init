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
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 200 4))

;; meta-arrow to move between buffers
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; cursor
(setq-default cursor-type 'bar)

;; toolbar off
(tool-bar-mode -1)

;; annoying startup messages
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message "edward")
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; delete selection mode
(delete-selection-mode 1)

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

;; scratch buffer to text mode
(setq initial-major-mode 'text-mode)

;; save Customize settings in separate .el file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

;; revert all buffers function
;; credit to Chris Stuart https://www.emacswiki.org/emacs/RevertBuffer
(defun revert-all-buffers ()
  "Iterate through the list of buffers and revert them, e.g. after a
    new branch has been checked out."
  (interactive)
  (when (yes-or-no-p
         "Are you sure - any changes in open buffers will be lost! ")
    (let ((frm1 (selected-frame)))
      (make-frame)
      (let ((frm2 (next-frame frm1)))
        (select-frame frm2)
        (make-frame-invisible)
        (dolist (x (buffer-list))
          (let ((test-buffer (buffer-name x)))
            (when (not (string-match "\*" test-buffer))
              (when (not (file-exists-p (buffer-file-name x)))
                (select-frame frm1)
                (when (yes-or-no-p
                       (concat "File no longer exists ("
                               (buffer-name x)
                               "). Close buffer? "))
                  (kill-buffer (buffer-name x)))
                (select-frame frm2))
              (when (file-exists-p (buffer-file-name x))
                (switch-to-buffer (buffer-name x))
                (revert-buffer t t t)))))
        (select-frame frm1)
        (delete-frame frm2)))))

;; style config
(defconst mana-cpp-style
  '((c-hanging-braces-alist . ((brace-list-open)
                               (brace-entry-open)
                               (statement-cont)
                               (substatement-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (namespace-open after)
                               (defun-open (before after))
                               (defun-close (before after))
                               (class-open after)
                               (class-close before)
                               (inline-open (before after))
                               (inline-close (before after))
                               (func-decl-cont after)
                               (member-init-intro before)
                               (member-init-cont)
                               (inher-intro)
                               (inher-cont)
                               (block-open)
                               (block-close (before after))))
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-basic-offset . 4)
    (c-offsets-alist . ((innamespace . 0)
                        (access-label . /)
                        (topmost-intro . 0)
                        (arglist-intro . ++)
                        (arglist-cont-nonempty . c-lineup-arglist)
                        (comment-intro . 0))))
  "MANA Tech LLC Style")

;; major mode hooks
;; delete trailing whitespace
;; configure tabination

;; all
(defun setup-common ()
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (local-set-key (kbd "C-c e") 'mark-whole-buffer)
  )
;; C++
(defun setup-c++-mode ()
  (local-set-key [C-tab] 'tab-to-tab-stop)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (set (make-local-variable 'c-max-one-liner-length) 80)
  (c-add-style "mana" mana-cpp-style)
  (c-set-style "mana")
  (defvar my-cpp-other-file-alist
    '(("\\.cpp\\'" (".h")) ("\\.h\\'" (".cpp"))))
  (setq-default ff-other-file-alist 'my-cpp-other-file-alist)
  )
(add-hook 'c++-mode-hook 'setup-common)
(add-hook 'c++-mode-hook 'setup-c++-mode)
;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'setup-common)
;; HTML
(defun setup-html-mode ()
  (set (make-local-variable 'sgml-basic-offset) 4))
(add-hook 'html-mode-hook 'setup-common)
(add-hook 'html-mode-hook 'setup-html-mode)
;; Smerge
(defun setup-smerge-mode ()
  (local-set-key (kbd "C-c s p") 'smerge-prev)
  (local-set-key (kbd "C-c s n") 'smerge-next)
  (local-set-key (kbd "C-c s o") 'smerge-keep-mine)
  (local-set-key (kbd "C-c s t") 'smerge-keep-other)
  )
(add-hook 'smerge-mode-hook 'setup-common)
(add-hook 'smerge-mode-hook 'setup-smerge-mode)

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
(setq use-package-always-ensure t)

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

;; company
(use-package company
  :bind (("C-." company-complete))
  :init
  (add-hook 'c++-mode-hook 'company-mode)
  :config
  (add-to-list 'company-backends 'company-irony)
  (setq company-async-timeout 30)
  )

;; irony
(use-package irony
  :defer t
  :init
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c++-mode-hook 'irony-mode)
  )

(use-package company-irony
  :defer t
  )

;; flycheck
(use-package flycheck
  :defer t
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  )

(use-package flycheck-irony
  :defer t
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
  )

;; ggtags
(use-package ggtags
  :init
  (add-hook 'c++-mode-hook 'ggtags-mode)
  )

;; keybinding ref

;; company
;; Search through completions (C-s) (C-r)
;; Complete one of the first 10 candidates (M-(digit))
;; Initiate completion manually (C-.)
;; See source of selected candidate (C-w)

;; flycheck
;; Next error (M-g n)
;; Previous error (M-g p)

;; ggtags
;; Find tag (M-.)
;; Find reference (M-])
;; Show definition (C-c M-?)
;; Find file (C-c M-f)
