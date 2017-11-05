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
(setq inhibit-startup-echo-area-message "eddie")
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

;; .h files are c++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; C-c e to mark-whole-buffer
(global-set-key (kbd "C-c e") 'mark-whole-buffer)

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
                        (inline-open . 0))))
  "MANA Tech LLC Style")

;; major mode hooks
;; delete trailing whitespace
;; configure tabination

;; all
(defun setup-common ()
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (local-set-key (kbd "C-c e") 'mark-whole-buffer)
  )
;; C
(defun setup-c-mode ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (set (make-local-variable 'c-basic-offset) 4))
(add-hook 'c-mode-hook 'setup-common)
(add-hook 'c-mode-hook 'setup-c-mode)
;; C++
(defun setup-c++-mode ()
  (local-set-key [C-tab] 'tab-to-tab-stop)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
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
;; Markdown
(add-hook 'markdown-mode-hook 'setup-common)
;; OCaml
(add-hook 'tuareg-mode-hook 'setup-common)
;; Rust
(add-hook 'rust-mode-hook 'setup-common)
;; Smerge
(defun setup-smerge-mode ()
  (local-set-key (kbd "C-c s p") 'smerge-prev)
  (local-set-key (kbd "C-c s n") 'smerge-next)
  (local-set-key (kbd "C-c s o") 'smerge-keep-mine)
  (local-set-key (kbd "C-c s t") 'smerge-keep-other))
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
(setq use-package-always-ensure t)

;; cleaner mode lines
(use-package diminish)

;; helper for mode remappings
(use-package bind-key)

;; programming languages

;; multiple
;; company
(use-package company
  :bind (("C-." . company-complete))
  :init
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'tuareg-mode-hook 'company-mode)
  :config
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'merlin-company-backend)
  (setq company-async-timeout 30)
  )

;; C++
;; CMake
; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

(autoload 'cmake-mode "~/.emacs.d/cmake-mode/cmake-mode.el" t)

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

;; Go
(use-package go-mode
  :defer t
  :init
  (require 'go-mode)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
)

;; JavaScript
(load-file "~/.emacs.d/flow-for-emacs/flow.el")

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

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :init (setq markdown-command "pandoc --from commonmark -t html5 -s")
  )

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
  :defer t
  :init
  (require 'rust-mode)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
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
;; Abort search (M-,)
;; Find reference (M-])
;; Show definition (C-c M-?)
;; Find file (C-c M-f)
;; Next match (M-n)
;; Previous match (M-p)
;; Next file (M-})
;; Previous file (M-{)
;; File where navigation session started (M-=)
;; First match (M-<)
;; Last match (M->)
;; Exit navigation mode (RET)

;; smerge
;; Prev conflict (C-c s p)
;; Next conflict (C-c s n)
;; Keep ours (C-c s o)
;; Keep theirs (C-c s t)

;; markdown
;; Compile (C-c C-c m)
;; Preview (C-c C-c p)
;; Export (C-c C-c e)
;; Live preview (C-c C-c l)
