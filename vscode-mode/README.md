# CrabIR mode for VS Code

Syntax highlighting and navigation for **CrabIR analysis reports produced by
Clam's `--ocrab` option**, i.e. files created by

```
clam.py prog.c --crab-check=assert --ocrab=prog.crabir MORE_OPTIONS
```

This is the VS Code counterpart of [`../emacs-mode/crab-mode.el`](../emacs-mode/crab-mode.el);
both cover exactly the same language and offer the same commands.

## What is being highlighted

An `--ocrab` file is not source code, it is an analysis report: CrabIR
instructions interleaved with the annotations Clam prints around them. The
grammar treats all three parts as first-class citizens.

1. **CrabIR instructions** — `assume`, `assert`, `havoc`, `goto`, `ite`,
   `crab_intrinsic`, the region/reference statements (`region_init`,
   `region_copy`, `region_cast`, `make_ref`, `remove_ref`, `gep_ref`,
   `load_from_ref`, `store_to_ref`, `ref_to_int`, `int_to_ref`), the array
   statements (`array_init`, `array_store`, `array_load`, `array_assign`),
   the casts (`trunc`, `sext`, `zext`), and the types (`bool`, `int<N>`,
   `real`, `ref`, `arr(...)`, `region(...)`, `unknown`, `void`).

   Note that crab prints binary operators as symbols (`+ - * / /_u % %_u
   & | ^ << >>_l >>_r`), not as mnemonics, so there is no `add`/`sub`
   keyword to highlight.

2. **`/** INVARIANTS: ... **/` blocks** — the abstract state inferred for
   each basic block, plus the `VARIABLES-OF-INFLUENCE:` section when
   `--crab-print-voi=true` is used. Rendered as documentation comments.

3. **`// ... Result:` comments** — printed above every `assert`, saying
   whether Clam discharged the assertion:

   ```
   // loc(file=t.c line=12 col=3) id=1 Result:  OK
   // loc(file=t.c line=13 col=3) id=2 Result:  FAIL -- num of warnings=1
   ```

   `OK` is scoped `markup.inserted` and `FAIL ...` is scoped
   `markup.deleted`, so they render green and red in essentially every
   theme. These are the lines you actually open the report for.

The keyword and type lists are kept in sync with crab's statement printers in
`crab/include/crab/cfg/cfg.hpp` and `crab/include/crab/types/variable.hpp`.

## Commands

| Command | Keybinding | Description |
| --- | --- | --- |
| `CrabIR: Next Check Result` | `Ctrl-C Ctrl-N` | Jump to the next `Result:` comment |
| `CrabIR: Previous Check Result` | `Ctrl-C Ctrl-P` | Jump to the previous `Result:` comment |
| `CrabIR: Next Failed Check` | `Ctrl-C Ctrl-F` | Jump to the next assertion Clam could not prove |
| `CrabIR: Previous Failed Check` | `Ctrl-C Ctrl-B` | Jump to the previous unproven assertion |
| `CrabIR: Toggle INVARIANTS Annotations` | `Ctrl-C Ctrl-I` | Dim/undim the invariant blocks |

On macOS the bindings use `Cmd` instead of `Ctrl`.

Additionally:

* The **Outline** view lists every declared function with its basic blocks as
  children (`Ctrl-Shift-O` to jump).
* **Folding** is provided per function and for multi-line invariant blocks.

Invariants dominate an `--ocrab` file by volume. Unlike Emacs overlays, VS Code
decorations cannot remove lines from the view, so `Toggle INVARIANTS
Annotations` dims them rather than hiding them; combine it with folding
(`Ctrl-K Ctrl-0`) to read the CrabIR on its own.

## Installing

The extension is plain JavaScript with no dependencies and no build step, so it
is enough to make it visible to VS Code:

```sh
ln -s "$(pwd)/vscode-mode" ~/.vscode/extensions/crabir
```

Then fully quit and reopen VS Code — a manually added extension is not always
picked up by `Developer: Reload Window` alone. Files ending in `.crabir` — the
conventional name for `--ocrab` output — are picked up automatically. For a
file with a different extension, use `Change Language Mode` and select
`CrabIR`.

To build a redistributable `.vsix` instead:

```sh
npx @vscode/vsce package
```

### Conflict with other CrabIR extensions

Other tools in the crab family ship their own VS Code extension for their own
`.crabir` dialect — notably `crabber`, whose extension is published as
`crab.crabir`. It declares the same language id (`crabir`) and the same
TextMate scope (`source.crabir`) as this one.

VS Code registers **one grammar per scope name and one language per id**, so
the two extensions silently override each other: whichever loses the race
contributes nothing, and `.crabir` files may end up with no highlighting at
all. Install only one of them at a time:

```sh
code --list-extensions | grep crabir     # expect exactly one
code --uninstall-extension crab.crabir   # or clam.crabir, whichever you are not using
```

If you ever need both at once, give this extension a private identity: change
the language `id` to `clam-crabir` and the `scopeName` to
`source.clam-crabir` in `package.json`, change `scopeName` to match in
`syntaxes/crabir.tmLanguage.json`, change the `languageId` check in
`extension.js`, and add

```json
{ "files.associations": { "*.crabir": "clam-crabir" } }
```

to this repository's `.vscode/settings.json` so `.crabir` files here open in
the Clam mode rather than the other one.

## See also

* [`../emacs-mode/crab-mode.el`](../emacs-mode/crab-mode.el) — the same mode for Emacs.
* [`../scripts/read_results.py`](../scripts/read_results.py) — summarizes the
  same `Result:` comments from the command line.
* [`../scripts/debug_assertion.py`](../scripts/debug_assertion.py) — inspects a
  single assertion using the invariants and variables-of-influence annotations.
