import * as vscode from 'vscode';

export default class LHSLiveTeXView {
  private panel: vscode.WebviewPanel;
  private document?: vscode.TextDocument;
  private extContext: vscode.ExtensionContext;
  private disposables: vscode.Disposable[];
  private closed: boolean = false; // If the panel has been disposed of

  constructor(extContext: vscode.ExtensionContext) {
    this.extContext = extContext;

    this.panel = vscode.window.createWebviewPanel(
      "live-lhs",
      "live-lhs",
      vscode.ViewColumn.One,
      {}
    );

    this.disposables = [];

    this.disposables.push(
      vscode.window.onDidChangeActiveTextEditor((editor: vscode.TextEditor | undefined) => {
        if (editor) {
          this.setDocument(editor.document);
        }
      })
    );

    this.disposables.push(
      vscode.workspace.onDidSaveTextDocument((document: vscode.TextDocument) => {
        if (document === this.document) {
          this.update();
        }
      })
    );

    // Cleanup on panel disposal
    this.panel.onDidDispose(() => {
      for (let disposable of this.disposables) {
        disposable.dispose();
      }
      this.closed = true;
      console.log("[live-lhs] Panel was closed.");
    });

    console.log("[live-lhs] Panel was opened.");
  }

  public dispose() {
    this.panel.dispose();
  }

  public isClosed() {
    return this.closed;
  }

  private update() {
    if (!this.document) { 
      return; 
    }

    this.panel.webview.html = this.document.getText();
  }

  private setDocument(document: vscode.TextDocument) {
    this.document = document;
    this.panel.title = this.document.fileName;
    this.update();
  }

  private clear() {
    this.panel.webview.html = "";
  }
}