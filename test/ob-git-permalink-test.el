;;; ob-git-permalink-test.el --- Tests for ob-git-permalink

(require 'ob-git-permalink)
(require 'ert)

;;; Code:

;; Useful debug information
(message "Running tests on Emacs %s" emacs-version)

(ert-deftest git-permalink-parser-github-test ()
  "Test git-permalink-parser-github."
  (let* ((result (git-permalink-parser-github "https://github.com/kijimaD/ob-git-permalink/blob/933beadc754b108d541ccaa5bb0f017c41ef107a/ob-git-permalink.el#L1")))
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

(ert-deftest git-permalink-parser ()
  "Test git-permalink-parser."
  (let* ((result (git-permalink-parser "https://github.com/kijimaD/ob-git-permalink/blob/933beadc754b108d541ccaa5bb0f017c41ef107a/ob-git-permalink.el#L1")))
    (should (functionp result)))
  "raise error if not found parser"
  (should-error (git-permalink-parser "invalid URL")))

(ert-deftest git-permalink-build-link ()
  "Test git-permalink-build-link."
  (let* ((hash-table (make-hash-table)))
    (puthash 'user "kijimaD" hash-table)
    (puthash 'repo "ob-git-permalink" hash-table)
    (puthash 'githash "933beadc754b108d541ccaa5bb0f017c41ef107a" hash-table)
    (puthash 'path "ob-git-permalink.el" hash-table)
    (should (string= (git-permalink-build-link hash-table) "https://raw.githubusercontent.com/kijimaD/ob-git-permalink/933beadc754b108d541ccaa5bb0f017c41ef107a/ob-git-permalink.el"))))

(ert-deftest git-permalink-request ()
  "Test git-permalink-request."
  (should (string= (git-permalink-request
                    "https://raw.githubusercontent.com/kijimaD/ob-git-permalink/7aa402d78a714952ec7ad7cb98f2bf96ff24a3b1/.gitignore"
                    1
                    nil)
                   ".cask"))
  (should (string= (git-permalink-request
                    "https://raw.githubusercontent.com/kijimaD/ob-git-permalink/7aa402d78a714952ec7ad7cb98f2bf96ff24a3b1/.gitignore"
                    1
                    2)
                   ".cask\nob-git-permalink.elc")))

(ert-deftest git-permalink-get-code ()
  "Test git-permalink-get-code."
  (should (string= (git-permalink-get-code "https://github.com/kijimaD/ob-git-permalink/blob/7aa402d78a714952ec7ad7cb98f2bf96ff24a3b1/.gitignore#L1")
                   ".cask")))

(provide 'ob-git-permalink-test)

;;; ob-git-permalink-test.el ends here
