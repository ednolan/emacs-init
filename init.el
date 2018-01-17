;; keybinding ref

;; macros
;; Insert newline at word before 79 chars (C-c n)

;; smerge
;; Prev conflict (C-c m p)
;; Next conflict (C-c m n)
;; Keep ours (C-c m o)
;; Keep theirs (C-c m t)

;; flycheck
;; Next error (M-g n)
;; Previous error (M-g p)

;; rtags
;; Find symbol at point (C-c r .)
;; Find all references at point (C-c r /)
;; Reparse file (C-c r e)
;; Print class hierarchy (C-c r h)
;; Find functions called by this function (C-c r A)

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

;; ;; toolbar off
;; (tool-bar-mode -1)

;; annoying startup messages
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message "edward")
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; delete selection mode
(delete-selection-mode 1)

;; mouse support in terminal mode
(xterm-mouse-mode t)

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

;; .h files are c++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; C-c e to mark-whole-buffer
(global-set-key (kbd "C-c e") 'mark-whole-buffer)

;; make windows split horizontally
(setq split-height-threshold 1)

;; Nicer C-x C-b
(global-set-key (kbd "C-x C-b") 'bs-show)

;; C-` to set mark
(global-set-key (kbd "C-`") 'set-mark-command)

;; mac os maps <insert> to <help> ???
(global-set-key (kbd "<help>") 'overwrite-mode)

;; F5 to revert-buffer without confirmation
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)

;; my macros

;; newline at token before column 80
(fset 'insert-newline-for-paragraph
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217831 tab 55 57 backspace backspace 56 48 return C-left return] 0 "%d")) arg)))

(global-set-key (kbd "C-c n") 'insert-newline-for-paragraph)

;; don't prompt that file changed on disk based solely on timestamp
;; credit to Stack Overflow user doublep
;; https://stackoverflow.com/a/29556894
;; Ignore modification-time-only changes in files, i.e. ones that
;; don't really change the contents.  This happens often with
;; switching between different VC buffers.
(defun update-buffer-modtime-if-byte-identical ()
  (let* ((size      (buffer-size))
         (byte-size (position-bytes size))
         (filename  buffer-file-name))
    (when (and byte-size (<= size 1000000))
      (let* ((attributes (file-attributes filename))
             (file-size  (nth 7 attributes)))
        (when (and file-size
                   (= file-size byte-size)
                   (string= (buffer-substring-no-properties 1 (1+ size))
                            (with-temp-buffer
                              (insert-file-contents filename)
                              (buffer-string))))
          (set-visited-file-modtime (nth 5 attributes))
          t)))))

(defun verify-visited-file-modtime--ignore-byte-identical (original &optional buffer)
  (or (funcall original buffer)
      (with-current-buffer buffer
        (update-buffer-modtime-if-byte-identical))))
(advice-add 'verify-visited-file-modtime :around #'verify-visited-file-modtime--ignore-byte-identical)

(defun ask-user-about-supersession-threat--ignore-byte-identical (original &rest arguments)
  (unless (update-buffer-modtime-if-byte-identical)
    (apply original arguments)))
(advice-add 'ask-user-about-supersession-threat :around #'ask-user-about-supersession-threat--ignore-byte-identical)

;; style config
(defconst mana-cpp-style
  '((c-basic-offset . 4)
    (c-offsets-alist . ((innamespace . 0)
                        (access-label . /)
                        (topmost-intro . 0)
                        (arglist-intro . ++)
                        (arglist-cont-nonempty . c-lineup-arglist)
                        (comment-intro . 0)
                        (member-init-intro . 0)
                        (case-label . *)
                        (statement-case-intro . *)
                        (inline-open . 0)
                        (substatement-open . 0))))
  "MANA Tech LLC Style")

;; major mode hooks
;; delete trailing whitespace
;; configure tabination

;; all
(defun setup-common ()
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  )
;; C++
(defun setup-c++-mode ()
  (local-set-key [C-tab] 'tab-to-tab-stop)
  (defun insert-four-spaces ()
    (interactive)
    (insert "    "))
  (local-set-key (kbd "C-S-<iso-lefttab>") 'insert-four-spaces)
  (defun ff-find-other-file-ignore-headers ()
    (interactive)
    (ff-find-other-file nil t))
  (local-set-key (kbd "C-c o") 'ff-find-other-file-ignore-headers)
  (c-add-style "mana" mana-cpp-style)
  (c-set-style "mana")
  (defvar my-cpp-other-file-alist
    '(("\\.cpp\\'" (".h")) ("\\.h\\'" (".cpp"))))
  (setq-default ff-other-file-alist 'my-cpp-other-file-alist)
  ;; rtags
  (rtags-start-process-maybe)
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
;; Python
(defun setup-python-mode ()
  (setq tab-width 4))
(add-hook 'python-mode-hook 'setup-common)
(add-hook 'python-mode-hook 'setup-python-mode)
;; reStructuredText
;; We don't want to strip trailing whitespace here
;; (add-hook 'rst-mode-hook 'setup-common)
;; Smerge
(defun setup-smerge-mode ()
  (local-set-key (kbd "C-c m p") 'smerge-prev)
  (local-set-key (kbd "C-c m n") 'smerge-next)
  (local-set-key (kbd "C-c m o") 'smerge-keep-mine)
  (local-set-key (kbd "C-c m t") 'smerge-keep-other)
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

;; thin gray line at 79 cols
(use-package fill-column-indicator
  :init
  (setq-default fill-column 79)
  (add-hook 'prog-mode-hook (lambda ()
                              (fci-mode 1)
                              ))
  :config
  (setq fci-handle-truncate-lines nil)
  )

;; cleaner mode lines
(use-package diminish)

;; helper for mode remappings
(use-package bind-key)

;; helm
(use-package helm
  :init
  (add-hook 'c++-mode-hook 'helm-mode)
  (progn
    (use-package helm-projectile)
    )
  )

;; projectile
(use-package projectile
  :init
  (add-hook 'c++-mode-hook 'projectile-mode)
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  )

;; company
(use-package company
  :bind (("C-." . company-complete))
  :init
  (add-hook 'c++-mode-hook 'company-mode)
  :config
  (add-to-list 'company-backends 'company-rtags)
  (add-to-list 'company-backends 'company-irony)
  (setq company-async-timeout 30)
  (setq company-idle-delay nil)
  )

;; flycheck
(use-package flycheck
  :defer t
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  )

;; programming languages

;; C++

;; irony
(use-package irony
  :defer t
  :init
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c++-mode-hook 'irony-mode)
  )

;; rtags
(use-package rtags
  :config
  (setq rtags-path "/u/edward/emacsstuff/rtags/bin")
  (setq rtags-completions-enabled t)
  (rtags-enable-standard-keybindings)
  )

;; company-irony
(use-package company-irony
  :defer t
  :config
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends 'company-irony)
  )

;; company-rtags
(use-package company-rtags
  :defer t)

;; flycheck-irony
(use-package flycheck-irony
  :defer t
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
  )

;; flycheck-rtags
(use-package flycheck-rtags
  :defer t
  :config
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil)
  )

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :init (setq markdown-command "pandoc --from commonmark --to html5 -s")
  )

;; Python
(use-package py-yapf
  :init
  (add-hook 'python-mode-hook 'py-yapf-enable-on-save)
  )
