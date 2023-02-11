[![MELPA](https://melpa.org/packages/ob-git-permalink-badge.svg)](https://melpa.org/#/ob-git-permalink)
[![Test](https://github.com/kijimaD/ob-git-permalink/actions/workflows/test.yml/badge.svg)](https://github.com/kijimaD/ob-git-permalink/actions/workflows/test.yml)

Org-Babel support for evaluating permalink and insert source code.

![Screenshot from 2023-02-05 02-05-58](https://user-images.githubusercontent.com/11595790/218249196-1a48c89f-6f65-43ed-850b-ee6c7b74d146.png)

👇Evaluate org-babel(C-c C-c)

![Screenshot 2023-02-11 17:45:12b](https://user-images.githubusercontent.com/11595790/218249203-52ba914b-8846-4d46-99bb-b7f7e14ab4f1.png)


## Installation

```
M-x package-install RET ob-git-permalink RET
```

## Usage

```
#+begin_src git-permalink
https://github.com/emacs-mirror/emacs/blob/a4dcc8b9a94466c792be3743760a4a45cf6e1e61/lisp/emacs-lisp/ring.el#L48-L52
#+end_src
```

↓ evaluate(C-c)

```
(defun ring-p (x)
  "Return t if X is a ring; nil otherwise."
  (and (consp x) (integerp (car x))
       (consp (cdr x)) (integerp (cadr x))
       (vectorp (cddr x))))
```

## How to get permalink?

- Emacs: [git-link](https://github.com/sshaw/git-link) is very useful.
- GitHub Web UI: [Creating a permanent link to a code snippet \- GitHub Docs](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/creating-a-permanent-link-to-a-code-snippet)

## Other notation

The results shown below are same.

```
#+caption: Emacs README
#+begin_src git-permalink :url https://github.com/emacs-mirror/emacs/blob/a4dcc8b9a94466c792be3743760a4a45cf6e1e61/README#L1-L2
#+end_src
```

```
#+RESULTS:
: Copyright (C) 2001-2022 Free Software Foundation, Inc.
: See the end of the file for license conditions.
```

## TODO

- support other hosting service(GitLab, Bitbucket...)
