;;; ob-git-permalink.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2022 kijimaD

;; Author: kijimaD <norimaking777@gmail.com>
;; Version 0.1
;; Keywords:git link org-babel
;; Package-Requires: ((emacs "25.1")  (request "0.3.2"))
;; URL: https://github.com/kijimaD/ob-git-permalink

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides expanding code from git repository permalink

;;; Code:
(require 'ob)
(require 'ob-ref)

(add-to-list 'org-babel-tangle-lang-exts '("git-permalink"))

(defconst org-babel-header-args:git-permalink
  '((:url . :any)
    (:variables . :any)
    (:headers . :any))
  "Babel header arguments.")

(defun git-permalink-parser-github (url)
  "Parse GitHub URL and return result hash."
  (let* ((hash (make-hash-table)))
    (string-match "http[s]?://github.com/\\(.*?\\)/\\(.*?\\)/blob/\\(.*?\\)/\\(.*\\)#L\\(.*?\\)$" url)
    (puthash 'user (match-string 1 url) hash)
    (puthash 'repo (match-string 2 url) hash)
    (puthash 'githash (match-string 3 url) hash)
    (puthash 'path (match-string 4 url) hash)
    (puthash 'line (match-string 5 url) hash)
    hash))

(git-permalink-parser-github "https://github.com/kijimaD/create-link/blob/e765b1067ced891a90ba0478af7fe675cff9b713/.gitignore#L1")

(defun git-permalink-parser (url)
  "Return parser by URL."
  (cond ((string-match-p "^http[s]?://github.com" url) (git-permalink-parser-github url))))

;; (git-permalink-parser "https://github.com/kijimaD/create-link/blob/e765b1067ced891a90ba0478af7fe675cff9b713/.gitignore#L1")

(defun git-permalink-build-link (hash)
  "Build link with HASH."
  (when (not (hash-table-p hash))
    (error "Argument Error: HASH is not hash table"))
  (let* ((user (gethash 'user hash))
         (repo (gethash 'repo hash))
         (githash (gethash 'githash hash))
         (path (gethash 'path hash)))
    (format "https://raw.githubusercontent.com/%s/%s/%s/%s" user repo githash path)))

;; (git-permalink-build-link (git-permalink-parser "https://github.com/kijimaD/create-link/blob/e765b1067ced891a90ba0478af7fe675cff9b713/.gitignore#L1"))

(defun git-permalink-request (url line)
  "Get code from raw file URL and trim LINE."
  (let* ((buffer (url-retrieve-synchronously url))
         (contents (with-current-buffer buffer
                     ;; remove request header
                     (goto-char (point-min))
                     (re-search-forward "^$")
                     (delete-region (point) (point-min))
                     (kill-line)

                     ;; trim line
                     (forward-line (- line 1))
                     (kill-ring-save (line-beginning-position) (line-end-position))
                     (delete-region (point-min) (point-max))
                     (yank)

                     (buffer-string))))
    contents))

;; (insert (git-permalink-request "https://raw.githubusercontent.com/kijimaD/create-link/main/.gitignore"))

(defun git-permalink-get-code (url)
  "Get code by URL."
  (let* ((parser-hash)
         (request-url))
    (setq parser-hash (git-permalink-parser url))
    (setq request-url (git-permalink-build-link parser-hash))
    (git-permalink-request request-url
                           (string-to-number (gethash 'line parser-hash)))))

;;;###autoload
(defun org-babel-execute:git-permalink (body params)
  "Resolve BODY permalink with PARAMS and insert source code."
  (let* ((code (git-permalink-get-code body)))
    (with-temp-buffer
      (insert code)
      (buffer-substring (point-min) (point-max)))))

(provide 'ob-git-permalink)
;;; ob-git-permalink.el ends here
