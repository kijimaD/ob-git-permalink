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
  "git-permalink header arguments")

;; parse -> build -> request -> trim line -> display

;; url parse
;; github
;; - blob
;; - branch

;; domain user repo (blob + hash | branch) path line
;; change parser by domain
(defun git-permalink-parser (url)
  "Parse url and return parser"
  (cond ((string-match-p "^http[s]?://github.com" url) 'git-permalink-github-parser)))

(git-permalink-parser "https://github.com/kijimaD/create-link/blob/e765b1067ced891a90ba0478af7fe675cff9b713/.gitignore#L1")

;; TODO: into hash and return

;; https://raw.githubusercontent.com/kijimaD/create-link/main/.gitignore#L1
(defun git-permalink-parser-github-with-branch (url)
  "parse github url(branch)"
  (let* ((domain)
         (user)
         (repo)
         (path)
         (line))
    (string-match "http[s]?://raw.githubusercontent.com/\\(.*?\\)/\\(.*?\\)/.*?/\\(.*?\\)#L\\(.*?\\)$" url)
    (setq user (match-string 1 url))
    (setq repo (match-string 2 url))
    (setq path (match-string 3 url))
    (setq line (match-string 4 url))
    (concat " " user " " repo " " path " "line)))

(git-permalink-parser-github-with-branch "https://raw.githubusercontent.com/kijimaD/create-link/main/aa/.gitignore#L1")

;; https://github.com/kijimaD/create-link/blob/e765b1067ced891a90ba0478af7fe675cff9b713/.gitignore#L1
(defun git-permalink-parser-github-with-blob (url)
  "parse github url(blob)"
  (let* ((domain)
         (user)
         (repo)
         (hash)
         (path)
         (line))
    (string-match "http[s]?://github.com/\\(.*?\\)/\\(.*?\\)/blob/\\(.*?\\)/\\(.*\\)#L\\(.*?\\)$" url)
    (setq user (match-string 1 url))
    (setq repo (match-string 2 url))
    (setq hash (match-string 3 url))
    (setq path (match-string 4 url))
    (setq line (match-string 5 url))
    (concat " " user " " repo " " hash " " path " "line)))

(git-permalink-parser-github-with-blob "https://github.com/kijimaD/create-link/blob/e765b1067ced891a90ba0478af7fe675cff9b713/.gitignore#L1")

;; convert url from normal link to raw file url.
;; https://github.com/kijimaD/create-link/blob/e765b1067ced891a90ba0478af7fe675cff9b713/.gitignore#L1
;; ↓
;; https://raw.githubusercontent.com/kijimaD/create-link/e765b1067ced891a90ba0478af7fe675cff9b713/.gitignore

;; string interpolation
(defun git-permalink-build-link (alist)
  (let* ((link))
    ;; https://raw.githubusercontent.com/#{user}/#{repo}/#{hash}/#{path}
    (assoc 'type alist)
    (assoc 'user alist)
    (assoc 'repo alist)
    (assoc 'hash alist)
    (assoc 'path alist)))

;; request
;; request raw URL and get response body

(defun git-permalink-request (url)
  "Get code from raw file URL."
  (let* ((buffer (url-retrieve-synchronously url))
         (contents (with-current-buffer buffer
                     (goto-char (point-min))
                     (re-search-forward "^$")
                     (delete-region (point) (point-min))
                     (buffer-string)))
         (body))
    contents))

;; (insert (git-permalink-request "https://raw.githubusercontent.com/kijimaD/create-link/main/.gitignore"))

(defun git-permalink-get-code (url)
  "parse "
  (let* ((raw-url)
         (parser))
    (setq raw-url url)
    ;; get raw file
    (git-permalink-request raw-url)
    ;; cut line
    ))

;;;###autoload
(defun org-babel-execute:git-permalink (body params)
  (let* ((code (git-permalink-get-code body)))
    (with-temp-buffer
      (insert code)
      (buffer-substring (point-min) (point-max)))))

(provide 'ob-git-permalink)
;;; ob-git-permalink.el ends here
