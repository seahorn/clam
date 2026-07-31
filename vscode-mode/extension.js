// CrabIR support for VS Code.
//
// The language handled here is the CrabIR *analysis report produced by Clam's
// `--ocrab' option*, i.e. the file created by
//
//     clam.py prog.c --crab-check=assert --ocrab=prog.crabir MORE_OPTIONS
//
// Such a file is not source code: it is CrabIR interleaved with the annotations
// Clam prints, namely `/** INVARIANTS: ... **/' blocks and
// `// loc(...) id=N Result:  OK | FAIL -- ...' comments above each assertion.
// Everything below exists to make those annotations easy to navigate and to get
// out of the way, which is the counterpart of what emacs-mode/crab-mode.el does.

const vscode = require('vscode');

/** A check-result comment emitted by --ocrab above an assert. */
const CHECK_RE = /^\s*\/\/.*\bResult:/;
/** A check-result comment for an assertion Clam could not discharge. */
const FAILED_CHECK_RE = /^\s*\/\/.*\bResult:\s*FAIL/;
/** The opening line of an INVARIANTS annotation block. */
const INVARIANT_START_RE = /^\s*\/\*\*\s*INVARIANTS:/;
/** The closing delimiter of any `/** ... **\/' annotation block. */
const ANNOTATION_END_RE = /\*\*\//;
/** A CrabIR function declaration, e.g. `@V_1:int32 declare main()'. */
const DECLARE_RE = /\bdeclare\b\s+([^\s(]+)/;
/** A basic block label, which always occupies a whole line. */
const LABEL_RE = /^([^\s:]+):\s*$/;

const invariantDecoration = vscode.window.createTextEditorDecorationType({
  opacity: '0.35',
});

let invariantsDimmed = false;

/**
 * Line ranges covered by INVARIANTS annotation blocks, which may span several
 * lines when --crab-print-voi adds a VARIABLES-OF-INFLUENCE section.
 */
function invariantRanges(document) {
  const ranges = [];
  for (let line = 0; line < document.lineCount; line++) {
    if (!INVARIANT_START_RE.test(document.lineAt(line).text)) {
      continue;
    }
    let end = line;
    while (end < document.lineCount &&
           !ANNOTATION_END_RE.test(document.lineAt(end).text)) {
      end++;
    }
    end = Math.min(end, document.lineCount - 1);
    ranges.push(new vscode.Range(line, 0, end, document.lineAt(end).text.length));
    line = end;
  }
  return ranges;
}

function refreshDecorations(editor) {
  if (!editor || editor.document.languageId !== 'crabir') {
    return;
  }
  editor.setDecorations(
    invariantDecoration,
    invariantsDimmed ? invariantRanges(editor.document) : []);
}

function refreshAllDecorations() {
  vscode.window.visibleTextEditors.forEach(refreshDecorations);
}

/**
 * Move the cursor to the next/previous line matching `regexp', wrapping is not
 * performed so that repeated invocations terminate at the ends of the file.
 */
function jumpToLine(regexp, forward, what) {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    return;
  }
  const document = editor.document;
  const from = editor.selection.active.line;
  const step = forward ? 1 : -1;
  for (let line = from + step; line >= 0 && line < document.lineCount; line += step) {
    if (!regexp.test(document.lineAt(line).text)) {
      continue;
    }
    const position = new vscode.Position(line, document.lineAt(line).firstNonWhitespaceCharacterIndex);
    editor.selection = new vscode.Selection(position, position);
    editor.revealRange(new vscode.Range(line, 0, line, 0),
                       vscode.TextEditorRevealType.InCenterIfOutsideViewport);
    return;
  }
  vscode.window.setStatusBarMessage(`No more ${what}`, 3000);
}

/**
 * Outline view: functions declared in the report, with their basic blocks as
 * children. This is the counterpart of crab-mode's imenu index.
 */
const symbolProvider = {
  provideDocumentSymbols(document) {
    const symbols = [];
    let current = null;

    const closeCurrent = (endLine) => {
      if (current) {
        current.symbol.range = new vscode.Range(
          current.startLine, 0, endLine, document.lineAt(endLine).text.length);
      }
    };

    for (let line = 0; line < document.lineCount; line++) {
      const text = document.lineAt(line).text;

      const declare = DECLARE_RE.exec(text);
      if (declare) {
        closeCurrent(Math.max(0, line - 1));
        const range = new vscode.Range(line, 0, line, text.length);
        const symbol = new vscode.DocumentSymbol(
          declare[1], '', vscode.SymbolKind.Function, range, range);
        symbols.push(symbol);
        current = { symbol, startLine: line };
        continue;
      }

      const label = LABEL_RE.exec(text);
      if (label && current) {
        const range = new vscode.Range(line, 0, line, text.length);
        current.symbol.children.push(new vscode.DocumentSymbol(
          label[1], '', vscode.SymbolKind.Key, range, range));
      }
    }
    closeCurrent(Math.max(0, document.lineCount - 1));
    return symbols;
  },
};

/** Fold whole functions and multi-line annotation blocks. */
const foldingProvider = {
  provideFoldingRanges(document) {
    const ranges = [];
    let functionStart = -1;

    for (let line = 0; line < document.lineCount; line++) {
      const text = document.lineAt(line).text;

      if (DECLARE_RE.test(text)) {
        if (functionStart >= 0 && line - 1 > functionStart) {
          ranges.push(new vscode.FoldingRange(functionStart, line - 1));
        }
        functionStart = line;
        continue;
      }

      if (INVARIANT_START_RE.test(text) && !ANNOTATION_END_RE.test(text)) {
        let end = line;
        while (end < document.lineCount &&
               !ANNOTATION_END_RE.test(document.lineAt(end).text)) {
          end++;
        }
        end = Math.min(end, document.lineCount - 1);
        if (end > line) {
          ranges.push(new vscode.FoldingRange(line, end,
                                              vscode.FoldingRangeKind.Comment));
          line = end;
        }
      }
    }

    if (functionStart >= 0 && document.lineCount - 1 > functionStart) {
      ranges.push(new vscode.FoldingRange(functionStart, document.lineCount - 1));
    }
    return ranges;
  },
};

function activate(context) {
  const selector = { language: 'crabir' };

  context.subscriptions.push(
    vscode.languages.registerDocumentSymbolProvider(selector, symbolProvider),
    vscode.languages.registerFoldingRangeProvider(selector, foldingProvider),

    vscode.commands.registerCommand('crabir.nextCheck',
      () => jumpToLine(CHECK_RE, true, 'check results')),
    vscode.commands.registerCommand('crabir.previousCheck',
      () => jumpToLine(CHECK_RE, false, 'check results')),
    vscode.commands.registerCommand('crabir.nextFailedCheck',
      () => jumpToLine(FAILED_CHECK_RE, true, 'failed checks')),
    vscode.commands.registerCommand('crabir.previousFailedCheck',
      () => jumpToLine(FAILED_CHECK_RE, false, 'failed checks')),
    vscode.commands.registerCommand('crabir.toggleInvariants', () => {
      invariantsDimmed = !invariantsDimmed;
      refreshAllDecorations();
      vscode.window.setStatusBarMessage(
        `CrabIR invariants ${invariantsDimmed ? 'dimmed' : 'shown'}`, 3000);
    }),

    vscode.window.onDidChangeActiveTextEditor(refreshDecorations),
    vscode.window.onDidChangeVisibleTextEditors(refreshAllDecorations),
    vscode.workspace.onDidChangeTextDocument((event) => {
      vscode.window.visibleTextEditors
        .filter((editor) => editor.document === event.document)
        .forEach(refreshDecorations);
    }),

    invariantDecoration);

  refreshAllDecorations();
}

function deactivate() {}

module.exports = { activate, deactivate };
