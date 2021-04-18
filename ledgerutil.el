(load-file (expand-file-name "~/.emacs.d/fuzzy-match.el"))

(defgroup ledgerutil nil
  "Customization group for ledgerutil"
  :group 'convenience)

(defcustom ledgerutil-category-alist nil
  "Mapping from transaction description to category for ledgerutil"
  :group 'ledgerutil
  :type '(alist :key-type string :value-type string))

(defun region-contents-string ()
  (if (region-active-p)
      (buffer-substring (region-beginning) (region-end))))

(defun ledgerutil-transaction-description ()
  (deactivate-mark)
  (let ((orig-lineno (line-number-at-pos)))
    (forward-whitespace 1)
    (if (eq orig-lineno (line-number-at-pos))
        (progn
          (push-mark nil t t)
          (move-end-of-line ())
          (let ((transaction-description (region-contents-string)))
            (pop-mark)
            (move-beginning-of-line ())
            transaction-description))
      (progn
        (previous-line)
        nil))))

(defun ledgerutil-transaction-category ()
  (deactivate-mark)
  (next-line)
  (next-line)
  (right-char 4)
  (push-mark nil t t)
  (move-end-of-line ())
  (let ((transaction-category (region-contents-string)))
    (pop-mark)
    (move-beginning-of-line ())
    (previous-line)
    (previous-line)
    transaction-category))

(defun ledgerutil-category-replace (new-category)
  (next-line)
  (move-end-of-line ())
  (right-char 5)
  (push-mark nil t t)
  (move-end-of-line ())
  (delete-backward-char 1)
  (insert new-category)
  (pop-mark)
  (move-beginning-of-line ())
  (previous-line)
  (previous-line))

(defun ledgerutil-line-starts-with-date ()
  (right-char 4)
  (let ((is-date (eq ?/ (char-after))))
    (move-beginning-of-line ())
    is-date))

(defun ledgerutil-move-to-next-transaction ()
  (move-beginning-of-line ())
  (next-line)
  (while (not (ledgerutil-line-starts-with-date))
    (next-line)))

(defun ledgerutil-suggested-category (category-in)
  (let* ((category-alist-keys (map-keys ledgerutil-category-alist))
         (suggested-description (car (FM-all-fuzzy-matches category-in category-alist-keys))))
    (if suggested-description
        (cdr (assoc suggested-description ledgerutil-category-alist)))))

(defun ledgerutil-check-transaction ()
  "Suggest a category for the transaction at point. Repeat with the next transaction"
  (interactive)
  (let ((transaction-description (ledgerutil-transaction-description)))
    (if transaction-description
         (let ((suggested-category (ledgerutil-suggested-category transaction-description)))
           (if suggested-category
               (if (y-or-n-p (concat "Use transaction category: " suggested-category))
                   (ledgerutil-category-replace suggested-category)))))
    (ledgerutil-move-to-next-transaction)
    (ledger-highlight-xact-under-point)
    (ledgerutil-check-transaction)))

(defun ledgerutil-save-alist ()
  (customize-save-variable 'ledgerutil-category-alist ledgerutil-category-alist))

(defun ledgerutil-clear-alist ()
  "Clear the mapping from ledger transaction descriptions to categories"
  (interactive)
  (setq ledgerutil-category-alist ())
  (ledgerutil-save-alist))

(defun ledgerutil-insert-transaction ()
  "Add the transaction at point to the mapping. Repeat with the next transaction"
  (interactive)
  (let ((transaction-description (ledgerutil-transaction-description)))
    (if transaction-description
         (let ((transaction-category (ledgerutil-transaction-category)))
           (if (y-or-n-p (concat "Insert transaction: "
                                 transaction-description "->"
                                 transaction-category))
               (progn
                 (add-to-list 'ledgerutil-category-alist (cons transaction-description transaction-category))
                 (ledgerutil-save-alist)))))
    (ledgerutil-move-to-next-transaction)
    (ledger-highlight-xact-under-point)
    (ledgerutil-insert-transaction)))

(global-set-key (kbd "C-c l c") 'ledgerutil-check-transaction)
(global-set-key (kbd "C-c l i") 'ledgerutil-insert-transaction)
