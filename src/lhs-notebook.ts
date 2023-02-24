/* eslint-disable @typescript-eslint/naming-convention */
import { TextDecoder, TextEncoder } from "util";
import * as vscode from "vscode";

/**
 * The style of a literate Haskell file.
 * A literate Haskell file must be written 
 * in either code blocks or bird tracks style.
 */
enum LiterateHaskellStyle {
  CodeBlocks,
  BirdTracks
}

interface LHSNotebookData extends vscode.NotebookData {
  /**
   * Metadata of the literate haskell notebook.
   */
  metadata: {
    /**
     * The style of the literate haskell file.
     */
    style: LiterateHaskellStyle
  },
}

class LHSSerializer implements vscode.NotebookSerializer {
  /**
   * Converts an .lhs file's contents to {@link LHSNotebookData}
   * for use by the VSCode Notebook.
   */
  async deserializeNotebook(
    content: Uint8Array,
    token: vscode.CancellationToken
  ): Promise<LHSNotebookData> {
    return new Promise((resolve, reject) => {
      // String decoded version of file's Uint8Array content
      const contents = new TextDecoder().decode(content);

      // The lines of the file 
      const lines = contents.split("\n");

      // The list of cells of the notebook
      const cells: vscode.NotebookCellData[] = [];

      /**
       * The last cell of the notebook, 
       * which has not yet been added to the cells list
       */ 
      let lastCell: vscode.NotebookCellData | null = null;

      // The cell kind of the next cell to be added
      let nextKind = vscode.NotebookCellKind.Markup;
      
      for (let i = 0; i < lines.length; i++) {
        // Trim whitespace
        let line = lines[i];
        let lineTrimmed = line.trim();

        if (lineTrimmed === "\\begin{code}") {
          // Last cell must be Markup cell to begin new code block
          if (lastCell?.kind === vscode.NotebookCellKind.Code) {
            reject("Unclosed code block found.");
            return;
          }

          // Last cell is a Markup cell
          if (lastCell) {
            cells.push(lastCell);
            lastCell = null;
          }

          nextKind = vscode.NotebookCellKind.Code;
        } else if (lineTrimmed === "\\end{code}") {
          // Last cell must be Code cell to begin new code block
          if (lastCell?.kind === vscode.NotebookCellKind.Markup) {
            reject("No corresponding begin code for end code.");
            return;
          }

          // Last cell is Code cell
          if (lastCell) {
            cells.push(lastCell);
            lastCell = null;
          }

          nextKind = vscode.NotebookCellKind.Markup;
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
              languageId: nextKind === vscode.NotebookCellKind.Markup ? "markdown" : "haskell"
            };
            continue;
          }

          // Append to current cell
          lastCell.value = lastCell.value + "\n" + line;
        }
      }

      if (lastCell) {
        if (lastCell.kind === vscode.NotebookCellKind.Markup) {
          cells.push(lastCell);
        } else {
          reject("Unclosed code block at EOF.");
          return;
        }
      }

      
      resolve({
        cells: cells, 
        metadata: {
          style: LiterateHaskellStyle.CodeBlocks
        }
      });
    });
  }

  /**
   * Converts {@link LHSNotebookData} to a {@link Uint8Array}
   * which is written to the .lhs file. 
   */
  async serializeNotebook(
    data: LHSNotebookData,
    token: vscode.CancellationToken
  ): Promise<Uint8Array> {
    /**
     * The file contents as a string
     */ 
    let content = "";
    
    /**
     * Convert each cell
     */
    for (let cell of data.cells) {
      console.log(cell.value);
      if (cell.kind === vscode.NotebookCellKind.Markup) {
        // Convert text cell
        content += cell.value + "\n";
      } else {
        // Convert code cell
        content += "\\begin{code}\n" + cell.value + "\n\\end{code}\n";
      }
    }

    // Encode content string to Uint8Array
    return new TextEncoder().encode(content);
  }
}


export default LHSSerializer;