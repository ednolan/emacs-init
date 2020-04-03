(defun bp-hpp-include-path ()
  (concat (substring
           (substring buffer-file-name (length (expand-file-name "~/bellport/src/")) nil)
           0 -3)
          "hpp"))

(defun bp-header-comment-path ()
  (substring buffer-file-name (length (expand-file-name "~/bellport/src/bp/")) nil))

(defun bp-namespace-name ()
  (let ((srcdir (substring default-directory (length (expand-file-name "~/bellport/src/")) -1)))
    (replace-regexp-in-string "/" "::" srcdir)))

(defun bp-header-comment ()
  (let* ((current-year (shell-command-to-string "echo -n $(date +%Y)"))
         (comment-path (bp-header-comment-path))
         (comment-dash-padding-length (- 79
                                         (length "//--- ")
                                         (length comment-path)
                                         (length " ")
                                         (length "*- C++ -*-==//")))
         (comment-dash-padding (make-string comment-dash-padding-length ?-)))
    (concat "//--- " comment-path " " comment-dash-padding "*- C++ -*-==//\n"
            "//\n"
            "//               Bellport Low Latency Trading Infrastructure.\n"
            "//\n"
            "// Copyright MayStreet " current-year " - all rights reserved\n"
            "//===---------------------------------------------------------------------===//\n")))

(defun bp-insert-boilerplate-hpp ()
  "Insert boilerplate for a header file in Bellport"
  (interactive)
  (insert (bp-header-comment))
  (insert "\n")
  (insert "#pragma once\n")
  (insert "\n")
  (insert "namespace " (bp-namespace-name) " {\n")
  (insert "\n")
  (insert "} // namespace " (bp-namespace-name) "\n"))

(defun bp-insert-boilerplate-cpp ()
  "Insert boilerplate for an implementation file in Bellport"
  (interactive)
  (insert (bp-header-comment))
  (insert "\n")
  (insert "#include <" (bp-hpp-include-path) ">\n")
  (insert "\n")
  (insert "namespace " (bp-namespace-name) " {\n")
  (insert "\n")
  (insert "} // namespace " (bp-namespace-name) "\n"))
