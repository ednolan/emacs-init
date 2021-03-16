(defun macroutil-end-and-save-macro ()
  (kmacro-end-macro nil)
  (let* ((macro-name-prefix "macroutil-")
         (macro-name-suffix
          (read-string "Name for new macroutil macro: "))
         (macro-symbol (intern (concat macro-name-prefix macro-name-suffix))))
    (message (concat macro-name-prefix macro-name-suffix))
    (fset macro-symbol last-kbd-macro)
    (when (y-or-n-p "Store function in macro file? ")
      (find-file (expand-file-name "~/.emacs.d/macroutil-macros.el"))
      (insert-kbd-macro macro-symbol)
      (save-buffer)
      (quit-window))))

(defun macroutil-exec-saved-macro ()
  (message "Hello world"))

(defun macroutil-end-and-save-macro-or-exec-saved-macro (arg)
  (interactive "P")
  (if defining-kbd-macro
      (macroutil-end-and-save-macro)
    (macroutil-exec-saved-macro arg)))

;;(global-set-key (kbd "<f8>") 'macroutil-end-and-save-macro-or-exec-saved-macro)
