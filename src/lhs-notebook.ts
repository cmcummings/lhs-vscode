import { TextDecoder, TextEncoder } from "util";
import * as vscode from "vscode";
import LHS, { LHSNotebookData } from "./lhs";

class LHSSerializer implements vscode.NotebookSerializer {
  /**
   * Converts an .lhs file's contents to {@link LHSNotebookData}
   * for use by the VSCode Notebook.
   */
  async deserializeNotebook(
    content: Uint8Array,
    token: vscode.CancellationToken
  ): Promise<LHSNotebookData> {
    const contents = new TextDecoder().decode(content);

    // Parse content
    return LHS.parse(contents).then(
      data => data
    ).catch(err => {
        vscode.window.showErrorMessage(err);
        return Promise.reject("Failed to open .lhs file as notebook: " + err);
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
    return LHS.stringify(data).then((content => {
      // Encode content string to Uint8Array
      return new TextEncoder().encode(content);
    })).catch(err => {
      vscode.window.showErrorMessage(err);
      return Promise.reject("Failed to save .lhs file from notebook: " + err);
    });
  }
}


export default LHSSerializer;