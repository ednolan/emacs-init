(defun bp-hpp-include-path ()
  (concat (substring
           (substring buffer-file-name (length (expand-file-name "~/bellport/src/")) nil)
           0 -3)
          "hpp"))

(defun bp-namespace-name ()
  (let ((srcdir (substring default-directory (length (expand-file-name "~/bellport/src/")) -1)))
    (replace-regexp-in-string "/" "::" srcdir)))

(defun bp-header-comment ()
  (let* ((current-year (shell-command-to-string "echo -n $(date +%Y)")))
    (concat "//===-------------------------------------------------------*- C++ -*-===//\n"
            "//\n"
            "//               Bellport Low Latency Trading Infrastructure.\n"
            "//\n"
            "// Copyright MayStreet " current-year " - all rights reserved\n"
            "//===-----------------------------------------------------------------===//\n")))

(defun bp-test-corresponding-header (include-path)
  (replace-regexp-in-string
   "/test/"
   "/"
   (concat (substring include-path 0 (- 0 (length "_test.hpp"))) ".hpp")))

(defun bp-header-corresponding-test (include-path)
  (file-truename
   (replace-regexp-in-string
    (file-name-directory include-path)
    (concat (file-name-directory include-path) "/../test/" (file-name-as-directory (file-name-base (directory-file-name (file-name-directory include-path)))))
    (replace-regexp-in-string ".hpp" "_test.hpp" include-path))))

(defun bp-test-namespace-name (namespace-name)
  (concat (replace-regexp-in-string "::test" "" namespace-name) "::test"))

(defun bp-insert-boilerplate-inl ()
  "Insert boilerplate for an inline definitions file in Bellport"
  (interactive)
  (insert (bp-header-comment))
  (insert "\n")
  (insert "static_assert(__cplusplus > 202000);\n")
  (insert "\n")
  (insert "namespace " (bp-namespace-name) " {\n")
  (insert "\n")
  (insert "} // namespace " (bp-namespace-name) "\n"))

(defun bp-insert-boilerplate-hpp ()
  "Insert boilerplate for a header file in Bellport"
  (interactive)
  (insert (bp-header-comment))
  (insert "\n")
  (insert "static_assert(__cplusplus > 202000);\n")
  (insert "\n")
  (insert "#pragma once\n")
  (insert "\n")
  (insert "namespace " (bp-namespace-name) " {\n")
  (insert "\n")
  (insert "} // namespace " (bp-namespace-name) "\n"))

(defun bp-insert-boilerplate-test ()
  "Insert boilerplate for a test file in Bellport"
  (interactive)
  (insert (bp-header-comment))
  (insert "\n")
  (insert "static_assert(__cplusplus > 202000);\n")
  (insert "\n")
  (insert "#pragma once\n")
  (insert "#include <" (bp-test-corresponding-header (bp-hpp-include-path)) ">\n")
  (insert "#include <bp/test/bp_test.hpp>\n")
  (insert "\n")
  (insert "namespace " (bp-test-namespace-name (bp-namespace-name)) " {\n")
  (insert "\n")
  (insert "#warning class lacks unit tests\n")
  (insert "\n")
  (insert "} // namespace " (bp-test-namespace-name (bp-namespace-name)) "\n")
  )

(defun bp-insert-boilerplate-cpp ()
  "Insert boilerplate for an implementation file in Bellport"
  (interactive)
  (insert (bp-header-comment))
  (insert "\n")
  (insert "static_assert(__cplusplus > 202000);\n")
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
        (if (string-suffix-p ".hpp" buffer-file-name)
            (bp-insert-boilerplate-hpp)
          (bp-insert-boilerplate-inl))))))

(defun bp--try-find-file-or-next (extension basename)
  (if (file-exists-p (concat basename "." extension))
      (find-file (concat basename "." extension))
    (bp--find-next-file extension basename)))

(defun bp--try-find-file-or-fail (path)
  (if (file-exists-p path)
      (progn
        (find-file path)
        path)
    nil))

(defun bp--find-next-file (extension basename)
  (cond ((string-equal extension "hpp") (bp--try-find-file-or-next "inl" basename))
        ((string-equal extension "inl") (bp--try-find-file-or-next "cpp" basename))
        ((string-equal extension "cpp")
         (if (not (bp--try-find-file-or-fail (bp-header-corresponding-test (concat basename ".hpp"))))
             (bp--try-find-file-or-next "hpp" basename)))))

(defun bp-find-other-file ()
  (interactive)
  (if (string-suffix-p "_test.hpp" buffer-file-name)
      (find-file (bp-test-corresponding-header buffer-file-name))
    (bp--find-next-file (file-name-extension buffer-file-name)
                        (file-name-sans-extension buffer-file-name))))
