;;; helm-latexdiff.el --- Latexdiff integration in Emacs

;; Copyright (C) 2016 Launay Gaby

;; Author: Launay Gaby <gaby.launay@gmail.com>
;; Maintainer: Launay Gaby <gaby.launay@gmail.com>
;; Version: 0.1.0
;; Keywords: latex, diff
;; URL: http://github.com/muahah/emacs-latexdiff

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; helm-latexdiff allow to interact with Latexdiff-vc
;; [https://github.com/ftilmann/latexdiff] for git repository
;; and using Helm.

;; To use helm-latexdiff, make sure that this file is in Emacs load-path
;; (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require helm-latexdiff
;; (require 'helm-latexdiff)
;;
;; helm-latexdiff do not define default keybinding, so add them
;;
;; (define-key latex-mode-map (kbd "C-c l d") 'helm-latexdiff)
;; or with Evil
;; (evil-leader/set-key-for-mode 'latex-mode "ld" 'helm-latexdiff)
;;
;; The main function to use is `helm-latexdiff' which show you the
;; commits of your current git repository and ask you to choose
;; the two commits to use latexdiff on

;;; todo
;; Make something more general than okular...
;; sometimes do not get the revision number

;;; Code:

(require 'helm)
(require 'vc-git)
(require 'subr-x)
(require 'tex)

(defcustom helm-latexdiff-args
  "--force --pdf"
  "Argument passed to 'latexdiff' (modify at your own risk).

You may want to add '--flatten' if you have project with
multiple files."
  :type 'string
  :group 'helm-latexdiff)

(defcustom helm-latexdiff-vc-args
  "--force --pdf"
  "Argument passed to 'latexdiff-vc' (modify at your own risk).

You may want to add '--flatten' if you have project with
multiple files."
  :type 'string
  :group 'helm-latexdiff)

(defcustom helm-latexdiff-auto-display-pdf
  t
  "If set to `t`, generated diff pdf are automatically displayed after generation."
  :type 'boolean
  :group 'helm-latexdiff)

(defcustom helm-latexdiff-auto-clean-aux
  t
  "If set to `t`, automatically clean the auxilliary files (.aux, .log, ...)
after generating the diff pdf"
  :type 'boolean
  :group 'helm-latexdiff)

(defcustom helm-latexdiff-pdf-viewer
  "Emacs"
  "Command use to view PDF diffs.

If set to 'Emacs', open the PDF within Emacs."
  :type 'string
  :group 'helm-latexdiff)

(defface helm-latexdiff-date-face
  '((t (:inherit helm-prefarg)))
  "Face for the date"
  :group 'helm-latexdiff)

(defface helm-latexdiff-author-face
  '((t (:inherit helm-ff-file)))
  "Face for the author"
  :group 'helm-latexdiff)

(defface helm-latexdiff-message-face
  '((t (:inherit default :foreground "white")))
  "Face for the message"
  :group 'helm-latexdiff)

(defface helm-latexdiff-ref-labels-face
  '((t (:inherit helm-grep-match)))
  "Face for the ref-labels"
  :group 'helm-latexdiff)

(defgroup helm-latexdiff nil
  "latexdiff integration in Emacs."
  :prefix "helm-latexdiff-"
  :link `(url-link :tag "helm-latexdiff homepage" "https://github.com/muahah/emacs-latexdiff"))

(defun helm-latexdiff--check-if-installed ()
  "Check if latexdiff is installed."
  (with-temp-buffer
    (call-process "/bin/bash" nil t nil "-c"
		  "hash latexdiff-vc 2>/dev/null || echo 'NOT INSTALLED'")
    (goto-char (point-min))
    (if (re-search-forward "NOT INSTALLED" (point-max) t)
	(error "'latexdiff' is not installed, please install it"))))

(defun helm-latexdiff--check-if-pdf-produced (diff-file)
  "Check if DIFF-FILE has been produced."
  (let* ((diff-file (format "%s.pdf" diff-file))
	 (size (car (last (file-attributes diff-file) 5))))
    (not (or (eq size 0) (not (file-exists-p diff-file))))))

(defun helm-latexdiff-vc--latexdiff-sentinel (proc msg)
  "Sentinel for latexdiff executions.

PROC is the process to watch and MSG the message to
display when the process ends"
  (let ((diff-file (process-get proc 'diff-file))
	(FILE1 (process-get proc 'file1))
	(FILE2 (process-get proc 'file2)))
    (kill-buffer " *latexdiff*")
    ;; Clean if asked
    (when helm-latexdiff-auto-clean-aux
      (call-process "/bin/bash" nil 0 nil "-c"
		    (format "GLOBIGNORE='*.pdf' ; rm -r %s* ; GLOBIGNORE='' ;" diff-file)))
    ;; Check if pdf has been produced
    (if (not (helm-latexdiff--check-if-pdf-produced diff-file))
	(save-excursion
	  (find-file "latexdiff.log")
	  (rename-buffer "*latexdiff.log*"))
	(message "[%s.tex] PDF file has not been produced, check `*latexdiff.log*' buffer for more informations" FILE1)
      ;; Display the pdf if asked
        (when helm-latexdiff-auto-display-pdf
          (message "[%s.tex] Displaying PDF diff with %s" FILE1 FILE2)
          (if (string= helm-latexdiff-pdf-viewer "Emacs")
              (find-file (format "%s.pdf" diff-file))
            (call-process "/bin/bash" nil 0 nil "-c"
                          (format "%s %s.pdf" helm-latexdiff-pdf-viewer diff-file)))))))

(defun helm-latexdiff--latexdiff-sentinel (proc msg)
  "Sentinel for latexdiff executions.

PROC is the process to watch and MSG the message to
display when the process ends"
  (let ((diff-file (process-get proc 'diff-file))
	(file (process-get proc 'file))
	(REV1 (process-get proc 'rev1))
	(REV2 (process-get proc 'rev2)))
    (kill-buffer " *latexdiff*")
    ;; Clean if asked
    (when helm-latexdiff-auto-clean-aux
      (call-process "/bin/bash" nil 0 nil "-c"
		    (format "GLOBIGNORE='*.pdf' ; rm -r %s* ; rm -r %s-oldtmp* ; GLOBIGNORE='' ;" diff-file file)))
    ;; Check if pdf has been produced
    (if (not (helm-latexdiff--check-if-pdf-produced diff-file))
	(save-excursion
	  (find-file "latexdiff.log")
	  (rename-buffer "*latexdiff.log*"))
	(message "[%s.tex] PDF file has not been produced, check `*latexdiff.log*' buffer for more informations" file)
      ;; Display the pdf if asked
        (when helm-latexdiff-auto-display-pdf
          (message "[%s.tex] Displaying PDF diff between %s and %s" file REV1 REV2)
          (if (string= helm-latexdiff-pdf-viewer "Emacs")
              (find-file (format "%s.pdf" diff-file))
            (call-process "/bin/bash" nil 0 nil "-c"
                          (format "%s %s.pdf" helm-latexdiff-pdf-viewer diff-file)))))))

(defun helm-latexdiff--compile-diff (&optional FILE1 FILE2)
  "Use latexdiff to compile a pdf file of the difference between FILE1 and FILE2."
  (let ((diff-file (format "%s-diff" FILE2))
	(process nil))
    (helm-latexdiff--check-if-installed)
    (message "[%s.tex] Generating latex diff with %s" FILE1 FILE2)
    (setq process (start-process "latexdiff" " *latexdiff*"
				 "/bin/bash" "-c"
				 (format "rm -r latexdiff.log ; yes X | latexdiff-vc %s %s %s &> latexdiff.log ;" helm-latexdiff-args FILE1 FILE2)))
    (process-put process 'diff-file diff-file)
    (process-put process 'file1 FILE1)
    (process-put process 'file2 FILE2)
    (set-process-sentinel process 'helm-latexdiff-vc--latexdiff-sentinel)))

(defun helm-latexdiff-vc--compile-diff (&optional REV1 REV2)
  "Use latexdiff to compile a pdf file of the difference between REV1 and REV2."
  (let ((file (TeX-master-file nil nil t))
	(diff-file (format "%s-diff%s-%s" (TeX-master-file nil nil t) REV1 REV2))
	(process nil))
    (helm-latexdiff--check-if-installed)
    (message "[%s.tex] Generating latex diff between %s and %s" file REV1 REV2)
    (setq process (start-process "latexdiff" " *latexdiff*"
				 "/bin/bash" "-c"
				 (format "rm -r latexdiff.log ; yes X | latexdiff-vc %s -r %s -r %s %s.tex &> latexdiff.log ;" helm-latexdiff-vc-args REV1 REV2 file)))
    (process-put process 'diff-file diff-file)
    (process-put process 'file file)
    (process-put process 'rev1 REV1)
    (process-put process 'rev2 REV2)
    (set-process-sentinel process 'helm-latexdiff--latexdiff-sentinel)))

(defun helm-latexdiff-vc--compile-diff-with-current (REV)
  "Use latexdiff to compile a pdf file of the difference between the current state and REV."
  (let ((file (TeX-master-file nil nil t))
	(diff-file (format "%s-diff%s" (TeX-master-file nil nil t) REV))
	(process nil))
    (helm-latexdiff--check-if-installed)
    (message "[%s.tex] Generating latex diff with %s" file REV)
    (let* ((command (format "rm -r latexdiff.log ; yes X | latexdiff-vc %s -r %s %s.tex &> latexdiff.log ;" helm-latexdiff-vc-args REV file))
          (process (start-process "latexdiff" " *latexdiff*" "/bin/bash" "-c" command)))
      (process-put process 'diff-file diff-file)
      (process-put process 'file file)
      (process-put process 'rev1 "current")
      (process-put process 'rev2 REV)
      (set-process-sentinel process 'helm-latexdiff--latexdiff-sentinel))))

(defun helm-latexdiff--get-commits-infos ()
  "Return a list with all commits informations."
  ;; TODO : Implement other backends ?
  (let ((infos nil))
    (with-temp-buffer
      (vc-git-command t nil nil "log" "--format=%h---%cr---%cn---%s---%d" "--abbrev-commit" "--date=short")
      (goto-char (point-min))
      (while (re-search-forward "^.+$" nil t)
	(push (split-string (match-string 0) "---") infos)))
    infos))

(defun helm-latexdiff--get-commits-description ()
  "Return a list of commits description strings."
  (let ((descriptions ())
	(infos (helm-latexdiff--get-commits-infos))
	(tmp-desc nil)
	(lengths '((l1 . 0) (l2 . 0) (l3 . 0) (l4 . 0))))
    ;; Get lengths
    (dolist (tmp-desc infos)
      (pop tmp-desc)
      (when (> (length (nth 0 tmp-desc)) (cdr (assoc 'l1 lengths)))
	(add-to-list 'lengths `(l1 . ,(length (nth 0 tmp-desc)))))
      (when (> (length (nth 1 tmp-desc)) (cdr (assoc 'l2 lengths)))
	(add-to-list 'lengths `(l2 . ,(length (nth 1 tmp-desc)))))
      (when (> (length (nth 2 tmp-desc)) (cdr (assoc 'l3 lengths)))
	(add-to-list 'lengths `(l3 . ,(length (nth 2 tmp-desc)))))
      (when (> (length (nth 3 tmp-desc)) (cdr (assoc 'l4 lengths)))
	(add-to-list 'lengths `(l4 . ,(length (nth 3 tmp-desc))))))
    ;; Get infos
    (dolist (tmp-desc infos)
      (pop tmp-desc)
      (push (string-join
	     (list
	      (propertize (format
			   (format "%%-%ds "
				   (cdr (assoc 'l2 lengths)))
			   (nth 1 tmp-desc))
                          'face 'helm-latexdiff-author-face)
	      (propertize (format
			   (format "%%-%ds "
				   (cdr (assoc 'l1 lengths)))
			   (nth 0 tmp-desc))
                          'face 'helm-latexdiff-date-face)
	      (propertize (format "%s"
				  (nth 3 tmp-desc))
                          'face 'helm-latexdiff-ref-labels-face)
	      (propertize (format "%s"
				  (nth 2 tmp-desc))
                          'face 'helm-latexdiff-message-face))
	     " ")
	    descriptions))
    descriptions))

(defun helm-latexdiff--get-commits-hashes ()
  "Return the list of commits hashes."
  (let ((hashes ())
	(infos (helm-latexdiff--get-commits-infos))
	(tmp-desc nil))
    (dolist (tmp-desc infos)
      (push (pop tmp-desc) hashes))
    hashes))

(defun helm-latexdiff--update-commits ()
  "Return the alist of (HASH . COMMITS-DESCRIPTION)
to use with helm"
  (let ((descr (helm-latexdiff--get-commits-description))
	(hash (helm-latexdiff--get-commits-hashes))
	(list ()))
    (while (not (equal (length descr) 0))
      (setq list (cons (cons (pop descr) (pop hash)) list)))
    (reverse list)))

(defvar helm-source-latexdiff-choose-commit
  (helm-build-sync-source "Latexdiff choose commit"
    :candidates 'helm-latexdiff--update-commits
    :mode-line helm-read-file-name-mode-line-string
    :action '(("Choose this commit" . helm-latexdiff--compile-diff-with-current)))
  "Helm source for modified projectile projects.")

(defun helm-latexdiff-clean ()
  "Remove all file generated by latexdiff."
  (interactive)
  (let ((file (TeX-master-file nil nil t)))
    (call-process "/bin/bash" nil 0 nil "-c"
		  (format "rm -f %s-diff* ;
			   rm -f %s-oldtmp* ;
			   rm -f latexdiff.log"
			  file file))
    (message "[%s.tex] Removed all latexdiff generated files" file)))


(defun helm-latexdiff ()
  "Ask for two tex files and make the difference between them."
  (interactive)
  (let ((file1 (read-file-name "Base file: " nil nil t nil))
        (file2 (read-file-name "Base file: " nil nil t nil)))
    (helm-latexdiff--compile-diff file1 file2)
    )
  )

(defun helm-latexdiff-vc ()
  "Ask for a commit and make the difference with the current version."
  (interactive)
  (helm-latexdiff--check-if-installed)
  (helm :sources 'helm-source-latexdiff-choose-commit
	:buffer "*helm-latexdiff*"
	:nomark t
	:prompt "Choose a commit: "))

(defun latexdiff-vc ()
  "Ask for a commit and make the difference with the current version."
  (interactive)
  (helm-latexdiff--check-if-installed)
  (let* ((commits (helm-latexdiff--update-commits))
         (commit (completing-read "Choose a commit:" commits))
         (commit-hash (cdr (assoc commit commits))))
    (helm-latexdiff--compile-diff-with-current commit-hash)))

(defun helm-latexdiff-vc-range ()
  "Ask for two commits and make the difference between them."
  (interactive)
  (helm-latexdiff--check-if-installed)
  (let* ((commits (helm-latexdiff--update-commits))
	 (rev1 (helm-comp-read "Base commit: " commits))
	 (rev2 (helm-comp-read "Revised commit: " commits)))
    (helm-latexdiff--compile-diff rev1 rev2)))

(defun latexdiff-vc-range ()
  "Ask for two commits and make the difference between them."
  (interactive)
  (helm-latexdiff--check-if-installed)
  (let* ((commits (helm-latexdiff--update-commits))
         (commit1 (completing-read "Base commit:" commits))
         (commit1-hash (cdr (assoc commit1 commits)))
         (commit2 (completing-read "Revised commit:" commits))
         (commit2-hash (cdr (assoc commit2 commits))))
    (helm-latexdiff--compile-diff commit1-hash commit2-hash)))


(provide 'helm-latexdiff)
;;; helm-latexdiff.el ends here
