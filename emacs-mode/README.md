# CrabIR mode for Emacs

`crab-mode.el` provides syntax highlighting and navigation for **CrabIR
analysis reports produced by Clam's `--ocrab` option**, i.e. files created by

```
clam.py prog.c --crab-check=assert --ocrab=prog.crabir MORE_OPTIONS
```

See [`../vscode-mode/README.md`](../vscode-mode/README.md) for the VS Code
counterpart, which covers the same language and offers the same commands.

## What is being highlighted

An `--ocrab` file is not source code, it is an analysis report: CrabIR
instructions interleaved with the annotations Clam prints around them.

1. **CrabIR instructions** — control (`assume`, `goto`, `unreachable`),
   verification (`assert`, `havoc`, `crab_intrinsic`), region and reference
   statements (`region_init`, `region_copy`, `region_cast`, `make_ref`,
   `remove_ref`, `gep_ref`, `load_from_ref`, `store_to_ref`, `ref_to_int`,
   `int_to_ref`), array statements (`array_init`, `array_store`, `array_load`,
   `array_assign`), casts (`trunc`, `sext`, `zext`), and types (`bool`,
   `int<N>`, `real`, `ref`, `arr(...)`, `region(...)`, `unknown`, `void`).

   crab prints binary operators as symbols (`+ - * / /_u % %_u & | ^ << >>_l
   >>_r`), not as mnemonics, so there is no `add`/`sub` keyword to highlight.

2. **`/** INVARIANTS: ... **/` blocks** — the abstract state inferred for each
   basic block, plus a `VARIABLES-OF-INFLUENCE:` section under
   `--crab-print-voi=true`. Shown with `crab-invariant-face`.

3. **`// ... Result:` comments** — printed above every `assert`:

   ```
   // loc(file=t.c line=12 col=3) id=1 Result:  OK
   // loc(file=t.c line=13 col=3) id=2 Result:  FAIL -- num of warnings=1
   ```

   `OK` uses `crab-check-ok-face` (green) and `FAIL ...` uses
   `crab-check-fail-face` (red). These are the lines you actually open the
   report for.

Keyword and type lists are kept in sync with crab's statement printers in
`crab/include/crab/cfg/cfg.hpp` and `crab/include/crab/types/variable.hpp`.

## Commands

| Key | Command | Description |
| --- | --- | --- |
| `C-c C-n` | `crab-next-check` | Next `Result:` comment |
| `C-c C-p` | `crab-previous-check` | Previous `Result:` comment |
| `C-c C-f` | `crab-next-failed-check` | Next assertion Clam could not prove |
| `C-c C-b` | `crab-previous-failed-check` | Previous unproven assertion |
| `C-c C-i` | `crab-toggle-invariants` | Hide/show the invariant blocks |

Invariants dominate an `--ocrab` file by volume, so `C-c C-i` is usually the
first thing you want: it collapses every `/** INVARIANTS: ... **/` block and
leaves the CrabIR on its own.

`imenu` indexes both the declared functions and the basic block labels, so
`M-x imenu` (or `which-function-mode`, `counsel-imenu`, ...) jumps to either.

## Installing

```elisp
(add-to-list 'load-path "/path/to/clam/emacs-mode")
(require 'crab-mode)
```

Files ending in `.crabir` — the conventional name for `--ocrab` output — open
in `crab-mode` automatically.

## See also

* [`../scripts/read_results.py`](../scripts/read_results.py) — summarizes the
  same `Result:` comments from the command line.
* [`../scripts/debug_assertion.py`](../scripts/debug_assertion.py) — inspects a
  single assertion using the invariants and variables-of-influence annotations.
