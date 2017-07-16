(defun latexdiff-testcase-dir ()
  (expand-file-name "./testcase"))

(defun latexdiff-testcase-git-dir ()
  (expand-file-name "./testcase-git"))

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
