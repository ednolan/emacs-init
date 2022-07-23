(defun cpp2util--is-header-p (filepath)
  (and (string-match-p "/include/" filepath)
       (string-equal "hpp" (file-name-extension filepath))))

(defun cpp2util--is-inl-p (filepath)
  (and (string-match-p "/include/" filepath)
       (string-equal "inl" (file-name-extension filepath))))

(defun cpp2util--is-src-p (filepath)
  (string-match-p "/src/" filepath))

(defun cpp2util--is-test-p (filepath)
  (string-match-p "/test/" filepath))

(defun cpp2util--header-to-inl (filepath)
  (file-name-with-extension filepath ".inl"))

(defun cpputil-chop-initial-directory (filepath)
  (string-match "^/.*?\\(/.*\\)" filepath)
  (match-string 1 filepath))

(defun cpp2util--inl-to-src (filepath)
  (string-match "\\(.*\\)\\include\\(.*\\)" filepath)
  (concat
   (match-string 1 filepath)
   "src"
   (cpputil-chop-initial-directory
    (file-name-with-extension (match-string 2 filepath) ".cpp"))))

(defun cpp2util--src-to-test (filepath)
  (string-replace "src" "test" filepath))

(defun cpp2util--test-project-name (filepath)
  (string-match "^.*/\\(.*?\\)/test" filepath)
  (match-string 1 filepath))

(defun cpp2util--test-to-header (filepath)
  (string-match "\\(.*\\)\\test\\(.*\\)" filepath)
  (concat
   (match-string 1 filepath)
   "include/"
   (cpp2util--test-project-name filepath)
   (progn
     (string-match "\\(.*\\)\\test\\(.*\\)" filepath)
     (file-name-with-extension (match-string 2 filepath) ".hpp")
     )))

(defun cpp2util--next-file (filepath)
  (cond ((cpp2util--is-header-p filepath) (cpp2util--header-to-inl filepath))
        ((cpp2util--is-inl-p filepath) (cpp2util--inl-to-src filepath))
        ((cpp2util--is-src-p filepath) (cpp2util--src-to-test filepath))
        ((cpp2util--is-test-p filepath) (cpp2util--test-to-header filepath))))

(defun cpp2util--open-or-next (filepath)
  (if (file-exists-p filepath)
      (progn
        (find-file filepath)
        nil)
    (cpp2util--next-file filepath)))

(defun cpp2util-find-file ()
  (interactive)
  (setq filepath (cpp2util--next-file buffer-file-name))
  (while (setq filepath (cpp2util--open-or-next filepath))))
