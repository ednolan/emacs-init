(defgroup cclsutil nil
  "Customization group for cclsutil"
  :group 'convenience)

(defcustom cclsutil-registered-directories nil
  "List of project directories whose C++ source files activate CCLS and LSP"
  :group 'cclsutil
  :type '(repeat string))

(defun cclsutil-file-name-list-prefixes-file-namep (file-name file-name-list)
  (if (not file-name-list)
      nil
    (if (string-prefix-p (car file-name-list) file-name)
        t
      (cclsutil-file-name-list-prefixes-file-namep file-name (cdr file-name-list)))))

(defun cclsutil-file-name-in-registered-directoryp (file-name)
  (cclsutil-file-name-list-prefixes-file-namep file-name cclsutil-registered-directories))

(defun cclsutil-buffer-in-registered-directoryp ()
  (cclsutil-file-name-in-registered-directoryp buffer-file-name))
