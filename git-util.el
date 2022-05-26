(defun git-util-this-project-paths ()
  (split-string (shell-command-to-string
                 (concat "cd " default-directory " ; git diff --name-status master | cut -f2 | xargs -n1 -i{} echo $(git rev-parse --show-toplevel)/{}"))))

(defun git-util-this-repo-paths ()
  (split-string (shell-command-to-string
                 (concat "cd " default-directory " ; git ls-tree --full-name --full-tree -r --name-only HEAD | xargs -n1 -i{} echo $(git rev-parse --show-toplevel)/{}"))))

(defun text-files-in-directory (directory)
  (split-string (shell-command-to-string
                 (concat "source ~/.bash_functions ; while read -r line; do isbinary \"$line\" ; [[ \"$?\" -ne 0 ]] && echo \"$line\" ; done < <(find " directory " -type f)"))))

(defun git-util-open-this-repo ()
  "Open all paths in this git repository"
  (interactive)
  (mapc 'find-file-noselect (git-util-this-repo-paths)))

(defun git-util-open-this-project ()
  "Open all paths that differ from master in this git repository"
  (interactive)
  (mapc 'find-file-noselect (git-util-this-project-paths)))

(defgroup git-util-custom-group nil
  "Customize group for git-util."
  :group 'convenience)

(defcustom git-util-custom-project-directory ""
  "Project directory that git-util-helm-git-grep-custom-project-directory will search"
  :group 'git-util-custom-group
  :type '(string))

(defun git-util-open-custom-project-directory ()
  "Open all paths in this git repository"
  (interactive)
  (mapc 'find-file-noselect
        (text-files-in-directory git-util-custom-project-directory)))
