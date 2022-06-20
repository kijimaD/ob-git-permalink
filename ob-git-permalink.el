;;; ob-git-permalink.el --- Easy code citation from repository hosting service

;; Copyright (C) 2022 kijima Daigo

;; Author: kijima Daigo <norimaking777@gmail.com>
;; Version 0.1.0
;; Keywords:git link org-babel
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/kijimaD/ob-git-permalink

;; This file is NOT part of GNU Emacs.

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

;; Org-Babel support for evaluating permalink and insert source code.

;;; Code:
(require 'ob)

(add-to-list 'org-babel-tangle-lang-exts '("git-permalink"))

(defconst org-babel-header-args:git-permalink
  '((:url . :any))
  "Babel git-permalink arguments.")

(defun git-permalink-parser-github (url)
  "Parse GitHub URL and return result hash."
  (let* ((hash (make-hash-table)))
    (string-match "http[s]?://github.com/\\(.*?\\)/\\(.*?\\)/blob/\\(.*?\\)/\\(.*\\)#L\\(.*?\\)\\(?:-L\\(.*?\\)\\)?$" url)
    (puthash 'user (match-string 1 url) hash)
    (puthash 'repo (match-string 2 url) hash)
    (puthash 'githash (match-string 3 url) hash)
    (puthash 'path (match-string 4 url) hash)
    (puthash 'start (string-to-number (match-string 5 url)) hash)
    (puthash 'end (if (match-string 6 url)
                      (string-to-number (match-string 6 url))
                    nil) hash)
    hash))

(git-permalink-parser-github "https://github.com/kijimaD/create-link/blob/e765b1067ced891a90ba0478af7fe675cff9b713/.gitignore#L1")
(git-permalink-parser-github "https://github.com/kijimaD/create-link/blob/e765b1067ced891a90ba0478af7fe675cff9b713/.gitignore#L1-L10")

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

(defun git-permalink-request (url start end)
  "Get code from raw file URL and trim between START and END."
  (let* ((lines)
         (current-line start)
         (buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      ;; remove request header
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (kill-line)

      ;; trim line
      (forward-line (- current-line 1))
      (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines)
      (forward-line 1)
      (when end
        (while (< (line-number-at-pos) (+ end 1))
          (progn
            (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines)
            (forward-line 1)))))
    (mapconcat (function (lambda (s) (format "%s" s)))
               (reverse lines)
               "\n")))

;; (insert (git-permalink-request "https://raw.githubusercontent.com/kijimaD/create-link/main/.gitignore" 2 20))

(defun git-permalink-get-code (url)
  "Get code by URL."
  (let* ((parser-hash (git-permalink-parser url))
         (request-url (git-permalink-build-link parser-hash))
         (start (gethash 'start parser-hash))
         (end (if (gethash 'start parser-hash)
                   (gethash 'end parser-hash)
                 start)))
    (git-permalink-request request-url start end)))

;;;###autoload
(defun org-babel-execute:git-permalink (body params)
  "Resolve BODY permalink and insert source code.
If PARAMS url is specified, the parameter is used."
  (let* ((params-url (cdr (assq :url params)))
         (url (if params-url
                  params-url
                body))
         (code (git-permalink-get-code url)))
    (with-temp-buffer
      (insert code)
      (buffer-substring (point-min) (point-max)))))

(provide 'ob-git-permalink)
;;; ob-git-permalink.el ends here
