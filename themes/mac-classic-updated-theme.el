(deftheme mac-classic-updated
  "Inspired by TextMate's Mac Classic theme. Updated for truecolor")

;; Not a bad idea to define a palette...
(let (
      (color-1 "#0000CD")
      (color-2 "#C5060B")
      (color-3 "#3596A4")
      (color-4 "#0000FF")
      (color-5 "#585CF6")
      (color-6 "#318495")
      (color-7 "#0000A2")
      (color-8 "#036A07")
      (color-9 "#26B31A")
      (color-10 "#0066FF")
      (color-11 "#FFD0D0")
      (color-12 "#B90690"))

  ;; Set faces
  (custom-theme-set-faces
   'mac-classic-updated
   '(default ((t (:background "#ffffff" :foreground "#000000"))))
   ;; Font lock faces
   '(font-lock-builtin-face            ((t (:foreground ,color-1 :bold t))))
   '(font-lock-constant-face           ((t (:foreground ,color-2 :bold t))))
   '(font-lock-preprocessor-face       ((t (:foreground ,color-3))))
   '(font-lock-keyword-face            ((t (:foreground ,color-4 :bold t))))
   '(font-lock-type-face               ((t (:foreground ,color-5 :bold t))))
   '(font-lock-variable-name-face      ((t (:foreground ,color-6))))
   '(font-lock-function-name-face      ((t (:foreground ,color-7 :bold t))))
   '(font-lock-string-face             ((t (:foreground ,color-8))))
   '(ruby-string-variable-face         ((t (:foreground ,color-9))))
   '(font-lock-comment-face            ((t (:foreground ,color-10 :italic t))))
   '(font-lock-comment-delimiter-face  ((t (:foreground ,color-10))))
   '(whitespace-trailing               ((t (:background ,color-11))))
   '(font-lock-doc-face                ((t (:italic t :slant oblique :foreground ,color-12))))
   '(font-lock-doc-string-face         ((t (:foreground ,color-12)))))
  )

(provide-theme 'mac-classic-updated)
