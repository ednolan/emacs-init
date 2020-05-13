(defun git-util-this-project-paths ()
  (split-string (shell-command-to-string
                 (concat "cd " default-directory " ; git diff --name-status master | cut -f2 | xargs -n1 -i{} echo $(git rev-parse --show-toplevel)/{}"))))

(defun git-util-this-repo-paths ()
  (split-string (shell-command-to-string
                 (concat "cd " default-directory " ; git ls-tree --full-name --full-tree -r --name-only HEAD | xargs -n1 -i{} echo $(git rev-parse --show-toplevel)/{}"))))

(defun git-util-open-this-repo ()
  "Open all paths in this git repository"
  (interactive)
  (mapc 'find-file-noselect (git-util-this-repo-paths)))

(defun git-util-open-this-project ()
  "Open all paths that differ from master in this git repository"
  (interactive)
  (mapc 'find-file-noselect (git-util-this-project-paths)))

(defun git-util-helm-git-grep-this-project ()
  "Perform a helm git grep with only the paths that differ from master"
  (interactive)
  (let ((helm-git-grep-pathspecs (git-util-this-project-paths)))
    (helm-git-grep)))
