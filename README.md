<h1> Emacs-latexdiff </h1>

# Introduction
latexdiff is a minor mode to interact with [Latexdiff]
(https://github.com/ftilmann/latexdiff) for git repository
and using Helm.

# Requirements

Emacs-latexdiff require Emacs-24.3 or later versions
and [Helm](https://github.com/emacs-helm/helm)



# Getting started

## Install latexdiff

Install `latexdiff` from your package manager or from
[the official website](https://github.com/ftilmann/latexdiff).
Emacs-latexdiff use `latexdiff-vc' so make sure it is available.

## Install Emacs-latexdiff from git
  1. Clone the `latexdiff` repository to some directory:

    ```bash
    $ git clone https://github.com/muahah/latexdiff.git /path/to/latexdiff/directory
    ```

  2. Add to `.emacs.el` (or equivalent):

    ```bash
    (add-to-list 'load-path "/path/to/latexdiff/directory")
    (require 'latexdiff)
    ```

## Install Emacs-latexdiff from MELPA

```elisp
(package-install 'emacs-latexdiff)
```

## Configuration

Emacs-latexdiff faces can be customized through the customization panel :
```elisp
(customize-group 'latexdiff)
```

Emacs-latexdiff do not define default keybinding, so you need to add
them :
```elisp
(define-key latexdiff-mode-map (kbd "C-c l d") 'helm-latexdiff)
```
or with Evil :
```elisp
(evil-leader/set-key-for-mode 'latexdiff-mode "ld" 'helm-latexdiff)
```

## Basic usage

Start latexdiff
`(latexdiff-mode t)' or M-x `latexdiff-mode'

Emacs-latexdiff is buffer local, so hook it up
(add-hook 'latex-mode-hook 'latexdiff-mode)
or
(add-hook 'LaTeX-mode-hook 'latexdiff-mode)
for use with Auctex

The main function to use is `helm-latexdiff` which show you the
commits of your current git repository and ask you to choose
the two commits to use latexdiff on