(ert-deftest check-rev-to-rev-diff ()
  (let ((file1 (latexdiff-testcase-file1))
        (rev1 (latexdiff-testcase-rev1))
        (rev2 (latexdiff-testcase-rev2)))
    (with-current-buffer (find-file-noselect file1)
      (let ((diff-file (latexdiff-vc--compile-diff rev1 rev2)))
        (while latexdiff-runningp (sleep-for 1))  ;; wait for the process to terminate
        (should (latexdiff--check-if-pdf-produced (format "%s.pdf" diff-file))))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (should (search-backward (format "Generating latex diff between %s and %s" rev1 rev2) nil t))))))
