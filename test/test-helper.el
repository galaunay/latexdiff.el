
(setq latexdiff-auto-display-pdf nil)

(let ((latexdiff-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path latexdiff-dir))
(require 'emacs-latexdiff)

(defun latexdiff-testcase-dir ()
  (expand-file-name "./test/testcase"))

(defun latexdiff-testcase-git-dir ()
  (expand-file-name "./test/testcase-git"))

(defun latexdiff-testcase-file1 ()
  (let ((dir (latexdiff-testcase-dir)))
    (concat (file-name-as-directory dir) "article.tex")))

(defun latexdiff-testcase-file2 ()
  (let ((dir (latexdiff-testcase-dir)))
    (concat (file-name-as-directory dir) "article2.tex")))

(defun latexdiff-testcase-pdf1 ()
  (let ((dir (latexdiff-testcase-dir)))
    (concat (file-name-as-directory dir) "article.pdf")))

(defun latexdiff-testcase-dummy-pdf ()
  (let ((dir (latexdiff-testcase-dir)))
    (concat (file-name-as-directory dir) "dummy.pdf")))
