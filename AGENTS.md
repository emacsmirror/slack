# AGENTS.md

GNU Emacs client for Slack. Plain Emacs Lisp, `lexical-binding: t`, built on
`eieio` for classes and `lui` for the buffer/log machinery.

## Layout

- `slack.el` — entry point, team registration, `slack-start`.
- `slack-room.el` — room model: message store (`messages` hash, `message-ids`
  sorted list) and the **message ranges** that track which slices of history are
  loaded. See the "Message ranges" comment block there for the interval model.
- `slack-message-buffer.el` — channel buffer: renders history per range with
  gap markers, fills gaps bidirectionally, and `slack-open-message` (jumping to
  a message from search/feed/permalink, including threads whose root is not
  loaded).
- `slack-conversations.el` — `conversations.history` / `.replies` wrappers.
  `conversations-history` callbacks receive `(messages next-cursor has-more)`;
  `has-more` is window-scoped (nil means that window is complete).
- `slack-thread-message-buffer.el` — thread buffer; renders the root's `replies`
  slot, not the room store, so the root must be told its replies via
  `slack-message-set-replies` after a fetch.
- `slack-export.el` — read-only flattened room snapshots with indented replies;
  missing replies are paginated into the room store before export.
- `slack-block.el` — Slack Block Kit rendering (large; see `split-block-plan.md`
  for a proposed split).
- `test/run-test.el` — ERT suite. Helpers: `slack-test-ts`, `slack-test-range`,
  `slack-test-setup`, `slack-test-with-registered-team`.
- `test/slack-export-test.el` — exporter-specific tests, loaded by
  `test/run-test.el`.

## Makefile

All build and test commands go through the Makefile:

```sh
make install   # fetch missing dependencies into ELPA_DIR
make compile   # byte-compile all .el files
make reload    # reload the package into a running Emacs (via emacsclient)
make test      # run the ERT suite (source-preferred)
make check     # install + compile + test (what the pre-commit hook runs)
make clean     # remove .elc files
```

Override the Emacs binary or dependency cache with `EMACS=/path/to/emacs` or
`ELPA_DIR=/path/to/elpa`.

The `test` recipe sets `load-prefer-newer` so source files win over stale
`.elc` artifacts. The suite should be fully green; if a test fails,
investigate it rather than assuming it is a known baseline failure.

The HEAD tree already emits a fixed set of byte-compile warnings (free
variable `metadata` in `slack-buffer-animate-image`, a few `Unused lexical
argument` / `Unknown slot` / `docstring` warnings in other files). A change
should add **no new** warnings, not eliminate the old ones.

## Conventions

- **List/sequence style:** the existing code is written with `cl-lib`, but the
  maintainer prefers `dash.el`. New and rewritten code should lean toward
  `dash.el` (`-map`, `-filter`, `-find`, `--find`, `-contains-p`, etc.) over the
  `cl-` equivalents. Do not mass-rewrite existing `cl-lib` calls; just use
  `dash.el` when you touch or add code.
- Timestamps are strings like `"1657626419.612969"` and sort correctly with
  `string<`; use that, not numeric conversion.
- Tests use fixed-width timestamps (`slack-test-ts`); do not shorten them to
  `"1"`, `"7"`, `"11"` — as strings `"11" < "7"` and the ordering under test
  would never occur in practice.
- Prefer plain-words comments for non-obvious data-model reasoning (the range
  code has a worked bag→islands→merge example); keep them when editing.
- Network paths are tested by stubbing `slack-conversations-history` /
  `slack-conversations-replies` / `browse-url` with `cl-letf`, not by hitting
  the API. See `slack-test-open-message-loads-missing-thread`.
- Keep modules roughly under 500 lines. If a file grows beyond that or becomes
  noticeably larger than a clean, cohesive Elisp module, propose a split or
  refactor before adding more unrelated functionality.
- Split tests by functionality into dedicated `test/*-test.el` files; load each
  file from `test/run-test.el` so the standard suite still covers everything.

## Pre-commit hook

The tracked hook is `githooks/pre-commit`; install it for a checkout with:

```sh
ln -sf ../../githooks/pre-commit .git/hooks/pre-commit
```

It runs `make test` and blocks commits on test failures. When Emacs, make, or
dependency setup is unavailable, it prints the reason and skips the gate.
Dependencies are installed via `make install` into
`~/.cache/emacs-slack/elpa` (or `$XDG_CACHE_HOME/emacs-slack/elpa`); override
that location with `ELPA_DIR`.
