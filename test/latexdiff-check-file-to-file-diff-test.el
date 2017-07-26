(ert-deftest check-file-to-file-diff ()
  (let* ((file1 (latexdiff-testcase-file1))
         (file2 (latexdiff-testcase-file2))
         (filename1 (file-name-base  file1))
         (filename2 (file-name-base  file2)))
    (let ((diff-file (latexdiff--compile-diff file1 file2)))
      (while latexdiff-runningp (sleep-for 1))  ;; wait for the process to terminate
      (should (latexdiff--check-if-pdf-produced (format "%s.pdf" diff-file)))
      (with-current-buffer (find-file-noselect file1)
        (latexdiff-clean)))
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (should (search-backward (format "[%s] Generating latex diff with %s" filename1 filename2)
                               nil t)))))
