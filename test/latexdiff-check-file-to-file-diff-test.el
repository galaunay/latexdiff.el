(ert-deftest check-file-to-file-diff ()
  (let ((file1 (latexdiff-testcase-file1))
        (file2 (latexdiff-testcase-file2)))
    (let ((diff-file (latexdiff--compile-diff file1 file2)))
      (while latexdiff-runningp (sleep-for 1))  ;; wait for the process to terminate
      (should (latexdiff--check-if-pdf-produced (format "%s.pdf" diff-file))))
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (message "[%s] Displaying PDF diff with %s" file1 file2)
      (should (search-backward (format "[%s] Displaying PDF diff with %s" file1 file2)
                               nil t)))))
