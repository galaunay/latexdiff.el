
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

(defun latexdiff-testcase-rev-broken ()
  "Return the git hash for the broken test revision."
"79c7b3d")

(defun latexdiff-testcase-commits-info ()
  "Return expected result for `latexdiff--get-commits-infos`."
'(("bfbe4f5" "4 months ago" "galaunay" "perroquets" "") ("28ddecf" "4 months ago" "galaunay" "Lapin" "") ("8672704" "4 months ago" "galaunay" "removed section1" "") ("11f6343" "4 months ago" "galaunay" "Add pdf and second article" "") ("4a5fed7" "4 months ago" "galaunay" "Update .gitignore" " (origin/master, origin/HEAD)") ("79c7b3d" "16 minutes ago" "galaunay" "Broke the pdf file" "") ("164e618" "2 minutes ago" "galaunay" "Fix the file" " (HEAD -> master)")))

(defun latexdiff-testcase-commits-description ()
  "Return expected result for `latexdiff--get-commits-description`."
  '(#("galaunay  3 minutes ago    (HEAD -> master) Fix the file" 0 9 (face latexdiff-author-face) 10 25 (face latexdiff-date-face) 26 43 (face latexdiff-ref-labels-face) 44 56 (face latexdiff-message-face)) #("galaunay  17 minutes ago   Broke the pdf file" 0 9 (face latexdiff-author-face) 10 25 (face latexdiff-date-face) 27 45 (face latexdiff-message-face)) #("galaunay  4 months ago     (origin/master, origin/HEAD) Update .gitignore" 0 9 (face latexdiff-author-face) 10 25 (face latexdiff-date-face) 26 55 (face latexdiff-ref-labels-face) 56 73 (face latexdiff-message-face)) #("galaunay  4 months ago     Add pdf and second article" 0 9 (face latexdiff-author-face) 10 25 (face latexdiff-date-face) 27 53 (face latexdiff-message-face)) #("galaunay  4 months ago     removed section1" 0 9 (face latexdiff-author-face) 10 25 (face latexdiff-date-face) 27 43 (face latexdiff-message-face)) #("galaunay  4 months ago     Lapin" 0 9 (face latexdiff-author-face) 10 25 (face latexdiff-date-face) 27 32 (face latexdiff-message-face)) #("galaunay  4 months ago     perroquets" 0 9 (face latexdiff-author-face) 10 25 (face latexdiff-date-face) 27 37 (face latexdiff-message-face))))

(defun latexdiff-testcase-commits-hash ()
  "Return expected result for `latexdiff--get-commits-hashes`."
  '("164e618" "79c7b3d" "4a5fed7" "11f6343" "8672704" "28ddecf" "bfbe4f5"))

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

(defun latexdiff-testcase-file-broken ()
  "Return the path to the broken test file."
  (let ((dir (latexdiff-testcase-dir)))
    (concat (file-name-as-directory dir) "article-broken.tex")))

(defun latexdiff-testcase-pdf1 ()
  "Return the path to the pdf produced by the first test file."
  (let ((dir (latexdiff-testcase-dir)))
    (concat (file-name-as-directory dir) "article.pdf")))

(defun latexdiff-testcase-dummy-pdf ()
  "Return the path to an empty pdf."
  (let ((dir (latexdiff-testcase-dir)))
    (concat (file-name-as-directory dir) "dummy.pdf")))
