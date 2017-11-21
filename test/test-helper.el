
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

  '(("bfbe4f5" "4 months ago" "galaunay" "perroquets" "") ("28ddecf" "4 months ago" "galaunay" "Lapin" "") ("8672704" "4 months ago" "galaunay" "removed section1" "") ("11f6343" "4 months ago" "galaunay" "Add pdf and second article" "") ("4a5fed7" "3 months ago" "galaunay" "Update .gitignore" " (HEAD, origin/master, origin/HEAD, master)")))

(defun latexdiff-testcase-commits-description ()
  "Return expected result for `latexdiff--get-commits-description`."
  '(#("galaunay  3 months ago   (HEAD, origin/master, origin/HEAD, master) Update .gitignore" 0 9 (face latexdiff-author-face) 10 23 (face latexdiff-date-face) 24 67 (face latexdiff-ref-labels-face) 68 85 (face latexdiff-message-face)) #("galaunay  4 months ago   Add pdf and second article" 0 9 (face latexdiff-author-face) 10 23 (face latexdiff-date-face) 25 51 (face latexdiff-message-face)) #("galaunay  4 months ago   removed section1" 0 9 (face latexdiff-author-face) 10 23 (face latexdiff-date-face) 25 41 (face latexdiff-message-face)) #("galaunay  4 months ago   Lapin" 0 9 (face latexdiff-author-face) 10 23 (face latexdiff-date-face) 25 30 (face latexdiff-message-face)) #("galaunay  4 months ago   perroquets" 0 9 (face latexdiff-author-face) 10 23 (face latexdiff-date-face) 25 35 (face latexdiff-message-face))))

(defun latexdiff-testcase-commits-hash ()
  "Return expected result for `latexdiff--get-commits-hashes`."
  '("4a5fed7" "11f6343" "8672704" "28ddecf" "bfbe4f5"))

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
