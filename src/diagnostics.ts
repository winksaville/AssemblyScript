import * as ts from "byots";

// this is basically copy-pasted from tsc (compiler/program.ts)

const redForegroundEscapeSequence = "\u001b[91m";
const yellowForegroundEscapeSequence = "\u001b[93m";
const blueForegroundEscapeSequence = "\u001b[93m";
const gutterStyleSequence = "\u001b[100;30m";
const gutterSeparator = " ";
const resetEscapeSequence = "\u001b[0m";
const ellipsis = "...";

function getCategoryFormat(category: ts.DiagnosticCategory): string {
  switch (category) {
    case ts.DiagnosticCategory.Warning: return yellowForegroundEscapeSequence;
    case ts.DiagnosticCategory.Error: return redForegroundEscapeSequence;
    case ts.DiagnosticCategory.Message: return blueForegroundEscapeSequence;
  }
}

function formatAndReset(text: string, formatStyle: string) {
  return formatStyle + text + resetEscapeSequence;
}

function padLeft(s: string, length: number) {
  while (s.length < length) {
    s = " " + s;
  }
  return s;
}

export const defaultFormatDiagnosticsHost: ts.FormatDiagnosticsHost = {
  getCurrentDirectory: () => ts.sys.getCurrentDirectory(),
  getNewLine: () => ts.sys.newLine,
  getCanonicalFileName: ts.createGetCanonicalFileName(ts.sys.useCaseSensitiveFileNames)
};

export function formatDiagnostics(diagnostics: ts.Diagnostic[], host?: ts.FormatDiagnosticsHost): string {
  if (!host) host = defaultFormatDiagnosticsHost;
  let output = "";

  for (const diagnostic of diagnostics) {
    if (diagnostic.file) {
      const { line, character } = ts.getLineAndCharacterOfPosition(diagnostic.file, diagnostic.start);
      const fileName = diagnostic.file.fileName;
      const relativeFileName = ts.convertToRelativePath(fileName, host.getCurrentDirectory(), fileName => host.getCanonicalFileName(fileName));
      output += `${relativeFileName}(${line + 1},${character + 1}): `;
    }

    const category = ts.DiagnosticCategory[diagnostic.category].toLowerCase();
    output += `${category} TS${diagnostic.code}: ${ts.flattenDiagnosticMessageText(diagnostic.messageText, host.getNewLine())}${host.getNewLine()}`;
  }
  return output;
}

export function formatDiagnosticsWithColorAndContext(diagnostics: ts.Diagnostic[], host?: ts.FormatDiagnosticsHost): string {
  if (!host) host = defaultFormatDiagnosticsHost;
  let output = "";
  for (const diagnostic of diagnostics) {
    if (diagnostic.file) {
      const { start, length, file } = diagnostic;
      const { line: firstLine, character: firstLineChar } = ts.getLineAndCharacterOfPosition(file, start);
      const { line: lastLine, character: lastLineChar } = ts.getLineAndCharacterOfPosition(file, start + length);
      const lastLineInFile = ts.getLineAndCharacterOfPosition(file, file.text.length).line;
      const relativeFileName = host ? ts.convertToRelativePath(file.fileName, host.getCurrentDirectory(), fileName => host.getCanonicalFileName(fileName)) : file.fileName;

      const hasMoreThanFiveLines = (lastLine - firstLine) >= 4;
      let gutterWidth = (lastLine + 1 + "").length;
      if (hasMoreThanFiveLines) {
        gutterWidth = Math.max(ellipsis.length, gutterWidth);
      }

      output += ts.sys.newLine;
      for (let i = firstLine; i <= lastLine; i++) {
        // If the error spans over 5 lines, we'll only show the first 2 and last 2 lines,
        // so we'll skip ahead to the second-to-last line.
        if (hasMoreThanFiveLines && firstLine + 1 < i && i < lastLine - 1) {
          output += formatAndReset(padLeft(ellipsis, gutterWidth), gutterStyleSequence) + gutterSeparator + ts.sys.newLine;
          i = lastLine - 1;
        }

        const lineStart = ts.getPositionOfLineAndCharacter(file, i, 0);
        const lineEnd = i < lastLineInFile ? ts.getPositionOfLineAndCharacter(file, i + 1, 0) : file.text.length;
        let lineContent = file.text.slice(lineStart, lineEnd);
        lineContent = lineContent.replace(/\s+$/g, "");  // trim from end
        lineContent = lineContent.replace("\t", " ");    // convert tabs to single spaces

        // Output the gutter and the actual contents of the line.
        output += formatAndReset(padLeft(i + 1 + "", gutterWidth), gutterStyleSequence) + gutterSeparator;
        output += lineContent + ts.sys.newLine;

        // Output the gutter and the error span for the line using tildes.
        output += formatAndReset(padLeft("", gutterWidth), gutterStyleSequence) + gutterSeparator;
        output += redForegroundEscapeSequence;
        if (i === firstLine) {
          // If we're on the last line, then limit it to the last character of the last line.
          // Otherwise, we'll just squiggle the rest of the line, giving 'slice' no end position.
          const lastCharForLine = i === lastLine ? lastLineChar : undefined;

          output += lineContent.slice(0, firstLineChar).replace(/\S/g, " ");
          output += lineContent.slice(firstLineChar, lastCharForLine).replace(/./g, "~");
        }
        else if (i === lastLine) {
          output += lineContent.slice(0, lastLineChar).replace(/./g, "~");
        }
        else {
          // Squiggle the entire line.
          output += lineContent.replace(/./g, "~");
        }
        output += resetEscapeSequence;

        output += ts.sys.newLine;
      }

      output += ts.sys.newLine;
      output += `${relativeFileName}(${firstLine + 1},${firstLineChar + 1}): `;
    }

    const categoryColor = getCategoryFormat(diagnostic.category);
    const category = ts.DiagnosticCategory[diagnostic.category].toLowerCase();
    output += `${formatAndReset(category, categoryColor)} TS${diagnostic.code}: ${ts.flattenDiagnosticMessageText(diagnostic.messageText, ts.sys.newLine)}`;
  }
  return output;
}

export function createDiagnosticForNode(node: ts.Node, category: ts.DiagnosticCategory, message: string, arg1?: string) {
  let realMessage = message;
  if (arg1 != null)
    realMessage += ": " + arg1;
  return ts.createDiagnosticForNode(node, {
    key: message.toLowerCase().replace(/\s+/g, "_").replace(/[^\w]/g, ""),
    category: category,
    code: <any>"-AS",
    message: realMessage
  });
}

export function printDiagnostic(diagnostic: ts.Diagnostic): void {
  if (diagnostic.category == ts.DiagnosticCategory.Message)
    process.stderr.write(formatDiagnostics([ diagnostic ]));
  else
    process.stderr.write(formatDiagnosticsWithColorAndContext([ diagnostic ]) + "\n");
}
