// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import LHSSerializer from './lhs-notebook';
import LHSWebView from './lhs-webview';

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
	console.log("live-lhs loaded!");

	/** 
	 * The current LHSWebView.
	 * Maintain one LHSWebView at any time.
	 */
	let view: LHSWebView;

	const start = () => {
		if (!view || view.isClosed()) {
			view = new LHSWebView(context);
		}
	};

	context.subscriptions.push(
		vscode.commands.registerCommand('live-lhs.start', start)
	);

	/**
	 * Register lhs-notebook
	 */
	context.subscriptions.push(
		vscode.workspace.registerNotebookSerializer('lhs-notebook', new LHSSerializer())
	);
}

// This method is called when your extension is deactivated
export function deactivate() {}
