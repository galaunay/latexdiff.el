(ert-deftest check-rev-to-rev-diff ()
  ;; working example
  (let ((file1 (latexdiff-testcase-file1))
        (rev1 (latexdiff-testcase-rev1))
        (rev2 (latexdiff-testcase-rev2))
        (latexdiff-auto-display nil))
    (with-current-buffer (find-file-noselect file1)
      (let ((diff-dir (latexdiff-vc--compile-diff rev1 rev2)))
        (while latexdiff-runningp (sleep-for 1))  ;; wait for the process to terminate
        (should (latexdiff--check-if-file-produced (format "%s/%s.pdf" diff-dir (file-name-base file1))))
        (latexdiff-clean))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (should (search-backward (format "Generating latex diff between %s and %s" rev1 rev2) nil t)))))
  ;; non-working example
  (let ((file1 (latexdiff-testcase-file1))
        (rev1 (latexdiff-testcase-rev1))
        (rev2 (latexdiff-testcase-rev-broken))
        (latexdiff-auto-display nil))
    (with-current-buffer (find-file-noselect file1)
      (let ((diff-dir (latexdiff-vc--compile-diff rev1 rev2)))
        (while latexdiff-runningp (sleep-for 1))  ;; wait for the process to terminate
        (should (not (latexdiff--check-if-file-produced (format "%s/%s.pdf" diff-dir (file-name-base file1)))))
        (latexdiff-clean))
      (with-current-buffer "*Messages*"
        (goto-char (point-min))
        (should (search-forward
"[article] PDF file has not been produced" nil t))))))
