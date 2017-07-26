
;;; Code:



(let ((latexdiff-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path latexdiff-dir))

(require 'latexdiff)

(setq latexdiff-auto-display-pdf nil)

(defun latexdiff-testcase-dir ()
  "Return the test cases path."
  (expand-file-name "./test/testcase"))

(defun latexdiff-testcase-rev1 ()
  "Return the git hash for the first test revision."
"bfbe4f5")

(defun latexdiff-testcase-rev2 ()
  "Return the git hash for the second test revision."
"8672704")

(defun latexdiff-testcase-commits-info ()
  "Return expected result for `latexdiff--get-commits-infos`."
'(("bfbe4f5" "3 days ago" "galaunay" "perroquets" "") ("28ddecf" "3 days ago" "galaunay" "Lapin" "") ("8672704" "3 days ago" "galaunay" "removed section1" "") ("11f6343" "6 minutes ago" "galaunay" "Add pdf and second article" " (HEAD -> master, origin/master, origin/HEAD)")))

(defun latexdiff-testcase-commits-description ()
  "Return expected result for `latexdiff--get-commits-description`."
  '(#("galaunay  5 minutes ago   (HEAD -> master, origin/master, origin/HEAD) Add pdf and second article" 0 9 (face latexdiff-author-face) 10 24 (face latexdiff-date-face) 25 70 (face latexdiff-ref-labels-face) 71 97 (face latexdiff-message-face)) #("galaunay  3 days ago      removed section1" 0 9 (face latexdiff-author-face) 10 24 (face latexdiff-date-face) 26 42 (face latexdiff-message-face)) #("galaunay  3 days ago      Lapin" 0 9 (face latexdiff-author-face) 10 24 (face latexdiff-date-face) 26 31 (face latexdiff-message-face)) #("galaunay  3 days ago      perroquets" 0 9 (face latexdiff-author-face) 10 24 (face latexdiff-date-face) 26 36 (face latexdiff-message-face))))

(defun latexdiff-testcase-commits-hash ()
  "Return expected result for `latexdiff--get-commits-hashes`."
  '("11f6343" "8672704" "28ddecf" "bfbe4f5"))

(defun latexdiff-testcase-git-dir ()
  "Return the directory containing git testcase."
  (expand-file-name "./test/testcase-git"))

(defun latexdiff-testcase-file1 ()
  "Return the path to the first test file."
  (let ((dir (latexdiff-testcase-dir)))
    (concat (file-name-as-directory dir) "article.tex")))

(defun latexdiff-testcase-file2 ()
  "Return the path to the second test file."
  (let ((dir (latexdiff-testcase-dir)))
    (concat (file-name-as-directory dir) "article2.tex")))

(defun latexdiff-testcase-pdf1 ()
  "Return the path to the pdf produced by the first test file."
  (let ((dir (latexdiff-testcase-dir)))
    (concat (file-name-as-directory dir) "article.pdf")))

(defun latexdiff-testcase-dummy-pdf ()
  "Return the path to an empty pdf."
  (let ((dir (latexdiff-testcase-dir)))
    (concat (file-name-as-directory dir) "dummy.pdf")))
