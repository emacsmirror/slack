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

## Running the tests

The package ships compiled `.elc` files that are newer than the source after
edits. `require` loads the `.elc` by default, so **source changes can be
silently ignored**. Always run the suite with `load-prefer-newer` so the source
wins:

```sh
emacs -Q --batch \
      --eval '(progn (setq load-prefer-newer t) (package-initialize) (add-to-list (quote load-path) default-directory))' \
      -l ./test/run-test.el
```

Two tests are pre-existing failures unrelated to most work:
`slack-test-block-to-mrkdwn` and `slack-test-create-blocks-from-buffer`. A
clean run is "everything else passes".

## Byte-compiling

```sh
emacs -Q --batch \
      --eval '(progn (package-initialize) (add-to-list (quote load-path) default-directory))' \
      -f batch-byte-compile <file>.el
```

The HEAD tree already emits a fixed set of warnings (free variable `metadata`
in `slack-buffer-animate-image`, a few `Unused lexical argument` / `Unknown
slot` / `docstring` warnings in other files). A change should add **no new**
warnings, not eliminate the old ones.

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
