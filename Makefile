# Makefile for emacs-slack — install deps, byte-compile, reload, run tests.
#
# Variables:
#   EMACS       path to emacs (default: auto-detected)
#   ELPA_DIR   package cache for dependencies (default: ~/.cache/emacs-slack/elpa)
#
# Recipes:
#   make install     — fetch missing dependencies into ELPA_DIR
#   make compile     — byte-compile all .el files
#   make reload      — reload the package into a running Emacs (via emacsclient)
#   make test        — run the ERT suite (source-preferred)
#   make check       — install deps, compile, then test (what the hook runs)
#   make clean       — remove .elc files

EMACS     ?= $(shell command -v emacs 2>/dev/null)
ELPA_DIR  ?= $(XDG_CACHE_HOME:.=$(HOME)/.cache)/emacs-slack/elpa
EL_FILES  := $(wildcard *.el)
ELC_FILES := $(EL_FILES:.el=.elc)

EMACS_BATCH  = $(EMACS) -Q --batch \
  --eval "(progn \
    (require 'package) \
    (setq load-prefer-newer t \
          package-user-dir \"$(ELPA_DIR)\") \
    (package-initialize) \
    (add-to-list 'load-path default-directory))"

DEPS = websocket request circe alert emojify dash s ts

.PHONY: install compile reload test check clean

install:
	@mkdir -p "$(ELPA_DIR)"
	@echo "Installing dependencies into $(ELPA_DIR)..."
	@$(EMACS) -Q --batch \
	  --eval "(progn \
	    (require 'package) \
	    (setq package-user-dir \"$(ELPA_DIR)\" \
	          package-archives \
	          '((\"gnu\" . \"https://elpa.gnu.org/packages/\") \
	            (\"melpa\" . \"https://melpa.org/packages/\"))) \
	    (package-initialize) \
	    (package-refresh-contents) \
	    (dolist (pkg '(websocket request circe alert emojify dash s ts)) \
	      (unless (package-installed-p pkg) \
	        (message \"Installing %s...\" pkg) \
	        (package-install pkg))))"
	@echo "Dependencies installed."

compile:
	@$(EMACS_BATCH) -f batch-byte-compile $(EL_FILES)

reload:
	@emacsclient -e "(progn \
	  (let ((default-directory \"$(CURDIR)\")) \
	    (load-file \"slack.el\") \
	    (message \"emacs-slack reloaded\")))" 2>/dev/null \
	  || echo "No running Emacs server; skipping reload."

test:
	@$(EMACS_BATCH) -l ./test/run-test.el

check: install compile test

clean:
	rm -f $(ELC_FILES)
