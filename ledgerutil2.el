;; d -> Donations
;; g -> Grocery
;; h -> Health Care
;; i -> Insurance
;; l -> Lifestyle
;; r -> Restaurants
;; R -> Rent
;; s -> Shopping
;; t -> Transportation
;; T -> Travel
;; u -> Utilities
;; x -> Transfer
;; X -> Taxes

(define-minor-mode ledgerutil2-categorization-mode
  "ledgerutil2 categorization mode"
  :lighter " ledgerutil2-categorization"
  :keymap (define-keymap
            "d" (lambda () (interactive) (ledgerutil2-categorize "Donations"))
            "g" (lambda () (interactive) (ledgerutil2-categorize "Grocery"))
            "h" (lambda () (interactive) (ledgerutil2-categorize "Health Care"))
            "i" (lambda () (interactive) (ledgerutil2-categorize "Insurance"))
            "l" (lambda () (interactive) (ledgerutil2-categorize "Lifestyle"))
            "r" (lambda () (interactive) (ledgerutil2-categorize "Restaurants"))
            "R" (lambda () (interactive) (ledgerutil2-categorize "Rent"))
            "s" (lambda () (interactive) (ledgerutil2-categorize "Shopping"))
            "t" (lambda () (interactive) (ledgerutil2-categorize "Transportation"))
            "T" (lambda () (interactive) (ledgerutil2-categorize "Travel"))
            "u" (lambda () (interactive) (ledgerutil2-categorize "Utilities"))
            "x" (lambda () (interactive) (ledgerutil2-categorize "Transfer"))
            "X" (lambda () (interactive) (ledgerutil2-categorize "Taxes"))))

(defun ledgerutil2-categorize (category)
  (interactive "sEnter expense category: ")
  (kill-whole-line)
  (insert "    ")
  (insert "Expenses:")
  (insert category)
  (newline)
  (dotimes (i 4)
    (next-line)
    (scroll-up 1))
  (scroll-up 1))
