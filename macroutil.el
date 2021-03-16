(defun macroutil--register-symbol-by-number (number)
  (cond
   ((equal number 0) 'macroutil-register-0)
   ((equal number 1) 'macroutil-register-1)
   ((equal number 2) 'macroutil-register-2)
   ((equal number 3) 'macroutil-register-3)
   ((equal number 4) 'macroutil-register-4)
   ((equal number 5) 'macroutil-register-5)
   ((equal number 6) 'macroutil-register-6)
   ((equal number 7) 'macroutil-register-7)
   ((equal number 8) 'macroutil-register-8)
   ((equal number 9) 'macroutil-register-9)))

(defun macroutil--store-named-macro (macro-symbol)
  (find-file (expand-file-name "~/.emacs.d/macroutil-macros.el"))
  (insert-kbd-macro macro-symbol)
  (save-buffer)
  (quit-window))

(defun macroutil--name-and-store-macro (macro)
  (let* ((macro-name-prefix "macroutil-macro-")
         (macro-name-suffix
          (read-string "Name for new macroutil macro: "))
         (macro-symbol (intern (concat macro-name-prefix macro-name-suffix))))
    (message (concat macro-name-prefix macro-name-suffix))
    (fset macro-symbol macro)
    (when (y-or-n-p "Store function in macro file? ")
      (macroutil--store-named-macro macro-symbol))))

(defun macroutil--prompt-for-register ()
  (macroutil--register-symbol-by-number
   (string-to-number (read-string "Macro register number: "))))

(defun macroutil--prompt-for-macro ()
  (completing-read "Macro: " obarray
                   (lambda (func)
                     (string-prefix-p "macroutil-macro-" (symbol-name func)))
                   t nil 'extended-command-history))

(defun macroutil--process-macro (macro)
  (when (y-or-n-p "Store macro in register? ")
    (fset (macroutil--prompt-for-register) macro))
  (when (y-or-n-p "Give macro a name? ")
    (macroutil--name-and-store-macro macro)))

(defun macroutil--exec-named-macro (count)
  (execute-kbd-macro (intern (macroutil--prompt-for-macro)) count))

(defun macroutil-process-last-macro ()
  (interactive)
  (macroutil--process-macro last-kbd-macro))

(defun macroutil--end-and-process-macro ()
  (kmacro-end-macro nil)
  (macroutil-process-last-macro))

(defun macroutil-store-named-macro ()
  (interactive)
  (macroutil--store-named-macro (intern (macroutil--prompt-for-macro))))

(defun macroutil-name-and-store-register-macro (arg)
  (interactive "P")
  (macroutil--name-and-store-macro
   (symbol-function (macroutil--register-symbol-by-number arg))))

(defun macroutil-assign-named-macro-to-register (arg)
  (interactive "P")
  (let ((macro (intern (macroutil--prompt-for-macro))))
    (fset (macroutil--register-symbol-by-number arg) macro)))

(defun macroutil-end-and-process-macro-or-exec-named-macro (count)
  (interactive "P")
  (if defining-kbd-macro
      (macroutil--end-and-process-macro)
    (macroutil--exec-named-macro count)))
