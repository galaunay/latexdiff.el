
(setq latexdiff-auto-display-pdf nil)

(let ((latexdiff-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path latexdiff-dir))
(require 'emacs-latexdiff)

(defun latexdiff-testcase-dir ()
  (expand-file-name "./test/testcase"))

(defun latexdiff-testcase-rev1 ()
"bfbe4f5")

(defun latexdiff-testcase-rev2 ()
"8672704")

(defun latexdiff-testcase-commits-info ()
  '(("bfbe4f5" "28 minutes ago" "galaunay" "perroquets" "") ("28ddecf" "28 minutes ago" "galaunay" "Lapin" "") ("8672704" "27 minutes ago" "galaunay" "removed section1" " (HEAD -> master)")))

(defun latexdiff-testcase-commits-description ()
'(#("galaunay  39 minutes ago   (HEAD -> master) removed section1" 0 9 (face latexdiff-author-face) 10 25 (face latexdiff-date-face) 26 43 (face latexdiff-ref-labels-face) 44 60 (face latexdiff-message-face)) #("galaunay  39 minutes ago   Lapin" 0 9 (face latexdiff-author-face) 10 25 (face latexdiff-date-face) 27 32 (face latexdiff-message-face)) #("galaunay  40 minutes ago   perroquets" 0 9 (face latexdiff-author-face) 10 25 (face latexdiff-date-face) 27 37 (face latexdiff-message-face))))

(defun latexdiff-testcase-commits-hash ()
  '("8672704" "28ddecf" "bfbe4f5"))

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
