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

;; major mode hooks
;; delete trailing whitespace
;; configure tabination

;; all
(defun setup-common ()
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (local-set-key (kbd "C-c e") 'mark-whole-buffer))
;; C++
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
(defun setup-c++-mode ()
  (local-set-key [C-tab] 'tab-to-tab-stop)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "<f6>") 'cmake-ide-compile)
  (local-set-key (kbd "C-.") 'company-complete-common)
  (set (make-local-variable 'c-max-one-liner-length) 80)
  (c-add-style "mana" mana-cpp-style)
  (c-set-style "mana")
  (c-toggle-auto-newline)
  (defvar my-cpp-other-file-alist
    '(("\\.cpp\\'" (".h")) ("\\.h\\'" (".cpp"))))
  (setq-default ff-other-file-alist 'my-cpp-other-file-alist)
  )
(add-hook 'c++-mode-hook 'setup-common)
(add-hook 'c++-mode-hook 'setup-c++-mode)
;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'setup-common)
;; Smerge
(defun setup-smerge-mode ()
  (local-set-key (kbd "C-c {") 'smerge-prev)
  (local-set-key (kbd "C-c }") 'smerge-next)
  (local-set-key (kbd "C-c m") 'smerge-keep-mine)
  (local-set-key (kbd "C-c u") 'smerge-keep-other))
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

;; C++
;; cmake-ide dependencies:
;; rtags, flycheck, auto-complete-clang, company-clang, irony
(use-package rtags
  :ensure t
  :defer t
  :init
  (setq rtags-completions-enabled t)
  (rtags-enable-standard-keybindings)
  (setq rtags-path "~/.emacs.d/rtags/bin"))
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode))
(use-package auto-complete-clang
  :ensure t
  :defer t)
(use-package irony
  :ensure t
  :defer t)
;  :init
;  (add-hook 'c++-mode-hook 'irony-mode))
(use-package company-irony
  :ensure t
  :defer t)
(use-package company
  :ensure t
  :defer t
  :config
  (add-to-list 'company-backends 'company-irony)
  :init
  (add-hook 'c++-mode-hook 'company-mode))
;; cmake-ide
(use-package cmake-ide
  :ensure t
  :defer t
  :init
  (cmake-ide-setup)
  (setq cmake-ide-flags-c++ (append '("-std=c++11"))))

;; Set rtags to enable completions and use the standard keybindings.
;; A list of the keybindings can be found at:
;; http://syamajala.github.io/c-ide.html

;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(linum-format (quote dynamic))
 '(package-selected-packages (quote (use-package)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
