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

(defun bp-header-comment (comment-path)
  (let* ((current-year (shell-command-to-string "echo -n $(date +%Y)"))
         (comment-width (if (> (length comment-path) 54) 89 79))
         (comment-dash-padding-length (- comment-width
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
            "//===" (make-string (- comment-width 10) ?-) "===//\n")))

(defun bp-test-corresponding-header (include-path)
  (replace-regexp-in-string
   "/test/"
   "/"
   (concat (substring include-path 0 (- 0 (length "_test.hpp"))) ".hpp")))

(defun bp-test-namespace-name (namespace-name)
  (concat (replace-regexp-in-string "::test" "" namespace-name) "::test"))

(defun bp-insert-boilerplate-hpp ()
  "Insert boilerplate for a header file in Bellport"
  (interactive)
  (insert (bp-header-comment (bp-header-comment-path)))
  (insert "\n")
  (insert "#pragma once\n")
  (insert "\n")
  (insert "namespace " (bp-namespace-name) " {\n")
  (insert "\n")
  (insert "} // namespace " (bp-namespace-name) "\n"))

(defun bp-insert-boilerplate-test ()
  "Insert boilerplate for a test file in Bellport"
  (interactive)
  (insert (bp-header-comment (bp-header-comment-path)))
  (insert "\n")
  (insert "#pragma once\n")
  (insert "#include <" (bp-test-corresponding-header (bp-hpp-include-path)) ">\n")
  (insert "#include <bp/test/bp_test.hpp>\n")
  (insert "\n")
  (insert "namespace " (bp-test-namespace-name (bp-namespace-name)) " {\n")
  (insert "\n")
  (insert "TEST(TodoProjectName, TodoComponentName) {\n")
  (insert "  #warning empty test\n")
  (insert "}\n")
  (insert "\n")
  (insert "} // namespace " (bp-test-namespace-name (bp-namespace-name)) "\n")
  )

(defun bp-insert-boilerplate-cpp ()
  "Insert boilerplate for an implementation file in Bellport"
  (interactive)
  (insert (bp-header-comment (bp-header-comment-path)))
  (insert "\n")
  (insert "#include <" (bp-hpp-include-path) ">\n")
  (insert "\n")
  (insert "namespace " (bp-namespace-name) " {\n")
  (insert "\n")
  (insert "} // namespace " (bp-namespace-name) "\n"))

(defun bp-insert-boilerplate ()
  "Insert boilerplate for a source file in Bellport"
  (interactive)
  (let ((ext (file-name-extension buffer-file-name)))
    (if (string-equal ext "cpp")
        (bp-insert-boilerplate-cpp)
      (if (string-suffix-p "_test.hpp" buffer-file-name)
          (bp-insert-boilerplate-test)
          (bp-insert-boilerplate-hpp)))))
