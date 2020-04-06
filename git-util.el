(defun helm-git-grep-this-project ()
  "Perform a helm git grep with only the paths that differ from master"
  (interactive)
  (let* ((modified-files (split-string (shell-command-to-string
                          (concat "cd " default-directory " ; git diff --name-status master | cut -f2"))))
         (helm-git-grep-pathspecs modified-files))
    (helm-git-grep)))
