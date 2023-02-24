/* eslint-disable @typescript-eslint/naming-convention */

import { NotebookData, NotebookCellData, NotebookCellKind } from "vscode";


/**
 * The style of a literate Haskell file.
 * A literate Haskell file must be written 
 * in either code blocks or bird tracks style.
 */
enum LiterateHaskellStyle {
  CodeBlocks,
  BirdTracks
}


/**
 * An extension of {@link NotebookData} that represents 
 * the notebook data of a Literate Haskell file.
 */
interface LHSNotebookData extends NotebookData {
  /**
   * Metadata of the Literate Haskell notebook.
   */
  metadata: {
    /**
     * The style of the Literate Haskell file.
     */
    style: LiterateHaskellStyle
  },
}


/**
 * Parses the content of a Literate Haskell file to determine
 * which {@link LiterateHaskellStyle} it is in.
 * 
 * If the content contains at least one code block, it is considered to be in the {@link LiterateHaskellStyle.CodeBlocks code blocks} style.
 * Otherwise, it is considered to be in {@link LiterateHaskellStyle.BirdTracks bird tracks} style.
 * 
 * @param content The content to be parsed
 */
async function identifyStyle(content: string): Promise<LiterateHaskellStyle> {
  const lines = content.split("\n");

  let foundBeginCode = false;

  for (let line of lines) {
    line = line.trim();

    if (line === "\\begin{code}") {
      if (foundBeginCode) {
        return Promise.reject("Unclosed code block found.");
      } else {
        foundBeginCode = true;

      }
    } else if (line === "\\end{code}") {
      if (foundBeginCode) {
        return LiterateHaskellStyle.CodeBlocks;
      } else {
        return Promise.reject("Unopened code block found.");
      }
    }
  }

  return LiterateHaskellStyle.BirdTracks;
}


/**
 * Parses the content of a Literate Haskell file written in 
 * {@link LiterateHaskellStyle.CodeBlocks code blocks} style
 * into a list of {@link NotebookCellData}.
 * 
 * @param content The content to be parsed
 */
async function parseCodeBlocks(content: string): Promise<NotebookCellData[]> {
  const lines = content.split('\n'); // The lines of the file 
  const cells: NotebookCellData[] = [];// The list of cells of the notebook

  /**
   * The last cell of the notebook, 
   * which has not yet been added to the cells list
   */
  let lastCell: NotebookCellData | null = null;

  // The cell kind of the next cell to be added
  let nextKind = NotebookCellKind.Markup;

  for (let line of lines) {
    let lineTrimmed = line.trim(); // Trim whitespace

    if (lineTrimmed === "\\begin{code}") {
      // Last cell must be Markup cell to begin new code block
      if (lastCell?.kind === NotebookCellKind.Code) {
        return Promise.reject("Unclosed code block found.");
      }

      // Last cell is a Markup cell
      if (lastCell) {
        cells.push(lastCell);
        lastCell = null;
      }

      nextKind = NotebookCellKind.Code;
    } else if (lineTrimmed === "\\end{code}") {
      // Last cell must be Code cell to begin new code block
      if (lastCell?.kind === NotebookCellKind.Markup) {
        return Promise.reject("No corresponding begin code for end code.");
      }

      // Last cell is Code cell
      if (lastCell) {
        cells.push(lastCell);
        lastCell = null;
      }

      nextKind = NotebookCellKind.Markup;
    } else {
      /**
       * If no code block marker, then the current line is cell content
       * and we should append the current line to the last cell.
       */

      // Create cell if no current cell
      if (!lastCell) {
        lastCell = {
          kind: nextKind,
          value: line,
          languageId: nextKind === NotebookCellKind.Markup ? "markdown" : "haskell"
        };
        continue;
      }

      // Append to current cell
      lastCell.value += "\n" + line;
    }
  }

  if (lastCell) {
    if (lastCell.kind === NotebookCellKind.Markup) {
      cells.push(lastCell);
    } else {
      return Promise.reject("Unclosed code block at EOF.");
    }
  }

  return cells;
}


/**
 * Parses the content of a Literate Haskell file written in 
 * {@link LiterateHaskellStyle.BirdTracks bird tracks} style
 * into a list of {@link NotebookCellData}.
 * 
 * @param content The content to be parsed
 */
async function parseBirdTracks(content: string): Promise<NotebookCellData[]> {
  const lines = content.split('\n'); // The lines of the file 
  const cells: NotebookCellData[] = [];// The list of cells of the notebook

  let lastCell: NotebookCellData | null = null;

  console.log(lines);

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    // Check if line is code
    if (line.startsWith("> ")) {
      /**
       * Line is code
       */
      const lineCode = line.slice(2);

      if (lastCell) {
        if (lastCell.kind === NotebookCellKind.Code) {
          // Last cell is code
          lastCell.value += "\n" + lineCode;
          continue;
        } else {
          // Last cell is text
          if (i >= 1) {
            if (lines[i - 1].length === 0) {
              lastCell.value = lastCell.value.slice(0, lastCell.value.length - 1); // Cut 1 \n from the last cell
              if (lastCell.value.length > 0) {
                cells.push(lastCell);
              }
            } else {
              return Promise.reject("Missing empty line before bird-tracked code line.");
            }
          }
        }
      }

      lastCell = {
        kind: NotebookCellKind.Code,
        languageId: "haskell",
        value: lineCode
      };

      continue;
    }

    /**
     * Line is text
     */
    if (lastCell) {
      if (lastCell.kind === NotebookCellKind.Markup) {
        // Last line is text
        lastCell.value += "\n" + line;
        continue;
      } else {
        // Last line is code
        cells.push(lastCell);
        if (line.length === 0) {
          lastCell = null;
          continue;
        } else {
          return Promise.reject("Missing empty line after bird-tracked code line.");
        }
      }
    }

    lastCell = {
      kind: NotebookCellKind.Markup,
      languageId: "markdown",
      value: line
    };
  }

  if (lastCell) {
    cells.push(lastCell);
  }

  return cells;
}


/**
 * Parses the content of a Literate Haskell file into {@link LHSNotebookData}.
 * Will throw an error if there is an issue with the content.
 * 
 * @param content The content to be parsed
 */
async function parse(content: string): Promise<LHSNotebookData> {
  return identifyStyle(content).then(style => {
    let cells: Promise<NotebookCellData[]>;

    switch (style) {
      case LiterateHaskellStyle.CodeBlocks:
        cells = parseCodeBlocks(content);
        break;
      case LiterateHaskellStyle.BirdTracks:
        cells = parseBirdTracks(content);
        break;
      default:
        cells = Promise.reject();
    }

    return Promise.all([style, cells]);
  }).then(([style, cells]) => {
    return {
      cells: cells,
      metadata: {
        style: style
      }
    };
  }).catch(err => Promise.reject(err));
}

const md = ["#", ">"];

/**
 * Fixes certain Markdown tags that throw warnings/errors
 * when written to Literate Haskell files.
 * 
 * This function is intended to be used when 
 * serializing a Markdown cell to a Literate Haskell file.
 * 
 * @param text The Markdown text to be fixed
 * @returns A fixed version of the Markdown text that was passed
 */
function fixMarkdown(text: string): string {
  return text.split("\n").map(line => {
    if (md.some(c => line.startsWith(c))) {
      return " " + line;
    } else {
      return line;
    }
  }).join("\n");
}


/**
 * Serializes a list of cells into Literate Haskell in {@link LiterateHaskellStyle.CodeBlocks code blocks} style.
 * @param cells The cells to be serialized
 */
async function stringifyCodeBlocks(cells: NotebookCellData[]): Promise<string> {
  let content = ""; // The file contents as a string

  /**
   * Convert each cell
   */
  for (let cell of cells) {
    if (cell.kind === NotebookCellKind.Markup) {
      // Convert text cell
      content += fixMarkdown(cell.value) + "\n";
    } else {
      // Convert code cell
      content += "\\begin{code}\n" + cell.value + "\n\\end{code}\n";
    }
  }

  return content;
}


/**
 * Serializes a list of cells into Literate Haskell in {@link LiterateHaskellStyle.BirdTracks bird tracks} style.
 * @param cells The cells to be serialized
 */
async function stringifyBirdTracks(cells: NotebookCellData[]): Promise<string> {
  let content = "";

  for (let cell of cells) {
    if (cell.kind === NotebookCellKind.Markup) {
      content += fixMarkdown(cell.value) + "\n";
    } else {
      // Add bird tracks to the code lines
      content += "\n" + cell.value.split("\n").map(line => "> " + line).join("\n") + "\n\n";
    }
  }

  return content;
}


/**
 * Serializes {@link LHSNotebookData} into a string 
 * that can be saved to a Literate Haskell file.
 * 
 * @param data The {@link LHSNotebookData} to be serialized
 */
async function stringify(data: LHSNotebookData): Promise<string> {
  switch (data.metadata.style) {
    case LiterateHaskellStyle.BirdTracks:
      return stringifyBirdTracks(data.cells);
    case LiterateHaskellStyle.CodeBlocks:
      return stringifyCodeBlocks(data.cells);
    default:
      return Promise.reject("Style stringify not implemented.");
  }
}


export default { parse: parse, stringify: stringify };
export { LHSNotebookData };