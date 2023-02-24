import { TextDecoder, TextEncoder } from "util";
import * as vscode from "vscode";
import { LHSNotebookData, parse, stringify } from "./lhs";

/**
 * Provides methods for converting to and from 
 * Literate Haskell files and Notebooks 
 */
class LHSSerializer implements vscode.NotebookSerializer {
  /**
   * Converts an Literate Haskell file's contents 
   * to {@link LHSNotebookData} for use by the Notebook.
   */
  async deserializeNotebook(
    content: Uint8Array,
    token: vscode.CancellationToken
  ): Promise<LHSNotebookData> {
    const contents = new TextDecoder().decode(content);

    // Parse content
    return parse(contents).then(
      data => data
    ).catch(err => {
        vscode.window.showErrorMessage(err);
        return Promise.reject("Failed to open Literate Haskell file as notebook: " + err);
    });
  }

  /**
   * Converts {@link LHSNotebookData} to a {@link Uint8Array}
   * which is written to the Literate Haskell file. 
   */
  async serializeNotebook(
    data: LHSNotebookData,
    token: vscode.CancellationToken
  ): Promise<Uint8Array> {
    return stringify(data).then((content => {
      // Encode content string to Uint8Array
      return new TextEncoder().encode(content);
    })).catch(err => {
      vscode.window.showErrorMessage(err);
      return Promise.reject("Failed to save Literate Haskell file from notebook: " + err);
    });
  }
}


export default LHSSerializer;