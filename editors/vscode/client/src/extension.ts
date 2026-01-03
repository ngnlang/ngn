import * as vscode from 'vscode';
import * as path from 'path';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

let client: LanguageClient;
const outputChannel = vscode.window.createOutputChannel('ngn');

export function activate(context: vscode.ExtensionContext) {
    outputChannel.appendLine('ngn extension activating...');

    // Path: editors/vscode -> editors -> ngn2 -> target/release
    const serverModule = path.join(
        context.extensionPath,
        '..',   // editors/
        '..',   // ngn2/
        'target',
        'release',
        process.platform === 'win32' ? 'ngn-lsp.exe' : 'ngn-lsp'
    );

    outputChannel.appendLine(`Extension path: ${context.extensionPath}`);
    outputChannel.appendLine(`Server module path: ${serverModule}`);

    const serverOptions: ServerOptions = {
        run: { command: serverModule, transport: TransportKind.stdio },
        debug: { command: serverModule, transport: TransportKind.stdio },
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'ngn' }],
        outputChannel: outputChannel,
    };

    client = new LanguageClient('ngn', 'NGN Language Server', serverOptions, clientOptions);

    outputChannel.appendLine('Starting language client...');
    client.start().then(() => {
        outputChannel.appendLine('Language client started successfully!');
    }).catch((error) => {
        outputChannel.appendLine(`Failed to start language client: ${error}`);
    });
}

export function deactivate(): Thenable<void> | undefined {
    outputChannel.appendLine('ngn extension deactivating...');
    return client ? client.stop() : undefined;
}
