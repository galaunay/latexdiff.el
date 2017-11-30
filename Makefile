CASK ?= cask
EMACS ?= emacs

all: test

test:
	${CASK} install
	${CASK} exec ert-runner


.PHONY:	all test ecukes
