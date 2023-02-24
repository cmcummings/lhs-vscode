import { TextDecoder } from "util";
import * as vscode from "vscode";
import { convertStyle, LiterateHaskellStyle } from "./lhs";

function getFocusedFileURI() {
  if (vscode.window.activeTextEditor) {
    return vscode.window.activeTextEditor.document.uri;
  }

  if (vscode.window.activeNotebookEditor) {
    return vscode.window.activeNotebookEditor.notebook.uri;
  }
}

function convert(style: LiterateHaskellStyle) {
  const uri = getFocusedFileURI();
  if (!uri) {
    vscode.window.showInformationMessage("Could not find the focused file.");
    return;
  }

  vscode.workspace.fs.readFile(uri).then(bytes => {
    const content = new TextDecoder().decode(bytes);
    return convertStyle(content, style);
  }).then(content => {
    vscode.workspace.openTextDocument({
      content: content,
      language: "literate haskell",
    });
  });
}

export function convertToBirdTracks() {
  convert(LiterateHaskellStyle.BirdTracks);
}


export function convertToCodeBlocks() {
  convert(LiterateHaskellStyle.CodeBlocks);
}