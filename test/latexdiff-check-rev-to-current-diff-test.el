(ert-deftest check-rev-to-current-diff ()
  (let ((file1 (latexdiff-testcase-file1))
        (rev1 (latexdiff-testcase-rev1)))
    (with-current-buffer (find-file-noselect file1)
      (let ((diff-file (latexdiff-vc--compile-diff-with-current rev1)))
        (while latexdiff-runningp (sleep-for 1))  ;; wait for the process to terminate
        (should (latexdiff--check-if-pdf-produced (format "%s.pdf" diff-file)))
        (latexdiff-clean))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (should (search-backward (format "Generating latex diff with %s" rev1)
                                 nil t))))))
