;;; ob-git-permalink-test.el --- Tests for ob-git-permalink

(require 'ob-git-permalink)
(require 'ert)

;;; Code:

;; Useful debug information
(message "Running tests on Emacs %s" emacs-version)

(ert-deftest git-permalink-parser-github-test ()
  "Test git-permalink-parser-github."
  (let ((result (git-permalink-parser-github "https://github.com/kijimaD/ob-git-permalink/blob/933beadc754b108d541ccaa5bb0f017c41ef107a/ob-git-permalink.el#L1")))
    (should (hash-table-p result))
    (should (gethash 'user result))
    (should (gethash 'repo result))
    (should (gethash 'githash result))
    (should (gethash 'path result))
    (should (gethash 'start result))
    (should (null (gethash 'end result))))

  (let ((result (git-permalink-parser-github "https://github.com/kijimaD/ob-git-permalink/blob/933beadc754b108d541ccaa5bb0f017c41ef107a/ob-git-permalink.el#L1-L10")))
    (should (hash-table-p result))
    (should (gethash 'user result))
    (should (gethash 'repo result))
    (should (gethash 'githash result))
    (should (gethash 'path result))
    (should (gethash 'start result))
    (should (gethash 'end result))))

(provide 'ob-git-permalink-test)

;;; ob-git-permalink-test.el ends here
