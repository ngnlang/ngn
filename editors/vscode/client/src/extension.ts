import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import * as https from 'https';
import { execFile } from 'child_process';
import { promisify } from 'util';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

let client: LanguageClient;
const outputChannel = vscode.window.createOutputChannel('ngn');
const execFileAsync = promisify(execFile);
const weekMs = 7 * 24 * 60 * 60 * 1000;

type RemoteAssetMeta = {
    etag?: string;
    lastModified?: string;
};

async function pathExists(filePath: string): Promise<boolean> {
    try {
        await fs.promises.access(filePath, fs.constants.F_OK);
        return true;
    } catch {
        return false;
    }
}

function expandHomeDir(filePath: string): string {
    if (filePath.startsWith('~/')) {
        const home = process.env.HOME;
        if (home) {
            return path.join(home, filePath.slice(2));
        }
    }
    return filePath;
}

async function findWorkspaceServerBinary(): Promise<string | undefined> {
    const folders = vscode.workspace.workspaceFolders ?? [];
    for (const folder of folders) {
        const debugPath = path.join(folder.uri.fsPath, 'target', 'debug', 'ngn-lsp');
        if (await pathExists(debugPath)) {
            return debugPath;
        }

        const releasePath = path.join(folder.uri.fsPath, 'target', 'release', 'ngn-lsp');
        if (await pathExists(releasePath)) {
            return releasePath;
        }
    }
    return undefined;
}

async function resolveConfiguredServerPath(configuredPath: string): Promise<string | undefined> {
    const expanded = expandHomeDir(configuredPath);
    if (path.isAbsolute(expanded)) {
        return (await pathExists(expanded)) ? expanded : undefined;
    }

    const folders = vscode.workspace.workspaceFolders ?? [];
    for (const folder of folders) {
        const candidate = path.resolve(folder.uri.fsPath, expanded);
        if (await pathExists(candidate)) {
            return candidate;
        }
    }

    const fromCwd = path.resolve(expanded);
    if (await pathExists(fromCwd)) {
        return fromCwd;
    }

    return undefined;
}

async function downloadFile(url: string, destination: string): Promise<void> {
    await new Promise<void>((resolve, reject) => {
        const request = https.get(url, (response) => {
            if (response.statusCode && response.statusCode >= 300 && response.statusCode < 400 && response.headers.location) {
                response.resume();
                const redirected = new URL(response.headers.location, url).toString();
                downloadFile(redirected, destination).then(resolve).catch(reject);
                return;
            }

            if (response.statusCode !== 200) {
                response.resume();
                reject(new Error(`Download failed with status ${response.statusCode}`));
                return;
            }

            const fileStream = fs.createWriteStream(destination);
            response.pipe(fileStream);
            fileStream.on('finish', () => fileStream.close(() => resolve()));
            fileStream.on('error', (err) => {
                fileStream.close(() => reject(err));
            });
        });

        request.on('error', reject);
    }).catch(async (error) => {
        await fs.promises.unlink(destination).catch(() => undefined);
        throw error;
    });
}

async function fetchAssetMeta(url: string): Promise<RemoteAssetMeta> {
    return new Promise<RemoteAssetMeta>((resolve, reject) => {
        const request = https.request(url, { method: 'HEAD' }, (response) => {
            if (response.statusCode && response.statusCode >= 300 && response.statusCode < 400 && response.headers.location) {
                response.resume();
                const redirected = new URL(response.headers.location, url).toString();
                fetchAssetMeta(redirected).then(resolve).catch(reject);
                return;
            }

            if (response.statusCode !== 200) {
                response.resume();
                reject(new Error(`Metadata request failed with status ${response.statusCode}`));
                return;
            }

            const etag = response.headers.etag ? String(response.headers.etag) : undefined;
            const lastModified = response.headers['last-modified']
                ? String(response.headers['last-modified'])
                : undefined;
            response.resume();
            resolve({ etag, lastModified });
        });

        request.on('error', reject);
        request.end();
    });
}

export function activate(context: vscode.ExtensionContext) {
    outputChannel.appendLine('ngn extension activating...');

    if (process.platform !== 'linux') {
        const message = 'ngn language server is only available on Linux (including WSL).';
        outputChannel.appendLine(message);
        vscode.window.showErrorMessage(message);
        return;
    }

    let arch: 'x86_64';
    if (process.arch === 'x64') {
        arch = 'x86_64';
    } else {
        const message = `ngn language server is not available for architecture ${process.arch}.`;
        outputChannel.appendLine(message);
        vscode.window.showErrorMessage(message);
        return;
    }

    const config = vscode.workspace.getConfiguration('ngn');
    const configuredPath = (config.get<string>('languageServer.path') ?? '').trim();
    const preferWorkspaceBinary = config.get<boolean>('languageServer.preferWorkspaceBinary', true);

    const serverDir = path.join(context.globalStorageUri.fsPath, 'server', `linux-${arch}`);
    const downloadedServerBinary = path.join(serverDir, 'ngn-lsp');
    const assetName = `ngn-lsp-latest-linux-${arch}.tar.gz`;
    const downloadUrl = `https://github.com/ngnlang/ngn/releases/latest/download/${assetName}`;
    const archivePath = path.join(serverDir, assetName);

    const resolveServerBinary = async (): Promise<{ command: string; source: string }> => {
        if (configuredPath.length > 0) {
            const resolved = await resolveConfiguredServerPath(configuredPath);
            if (!resolved) {
                throw new Error(`Configured ngn.languageServer.path does not exist: ${configuredPath}`);
            }
            return { command: resolved, source: 'configured path' };
        }

        if (preferWorkspaceBinary) {
            const workspaceBinary = await findWorkspaceServerBinary();
            if (workspaceBinary) {
                return { command: workspaceBinary, source: 'workspace binary' };
            }
        }

        return { command: downloadedServerBinary, source: 'downloaded release binary' };
    };

    const ensureServer = async () => {
        await fs.promises.mkdir(serverDir, { recursive: true });
        const serverExists = await pathExists(downloadedServerBinary);
        const now = Date.now();
        const lastCheck = context.globalState.get<number>('ngn.lsp.lastCheck', 0);
        const shouldCheck = now - lastCheck > weekMs;

        if (serverExists && !shouldCheck) {
            return;
        }

        const storedEtag = context.globalState.get<string>('ngn.lsp.etag');
        const storedLastModified = context.globalState.get<string>('ngn.lsp.lastModified');
        let needsDownload = !serverExists;
        let latestEtag: string | undefined;
        let latestLastModified: string | undefined;
        let metaChecked = false;

        if (!needsDownload && shouldCheck) {
            try {
                const meta = await fetchAssetMeta(downloadUrl);
                metaChecked = true;
                latestEtag = meta.etag;
                latestLastModified = meta.lastModified;
                const etagChanged = meta.etag && meta.etag !== storedEtag;
                const lastModifiedChanged = meta.lastModified && meta.lastModified !== storedLastModified;
                if (etagChanged || lastModifiedChanged) {
                    needsDownload = true;
                    outputChannel.appendLine('Newer language server detected; downloading update.');
                }
            } catch (error) {
                outputChannel.appendLine(`Failed to check language server metadata: ${error}`);
            }
        }

        if (!needsDownload) {
            if (latestEtag) {
                await context.globalState.update('ngn.lsp.etag', latestEtag);
            }
            if (latestLastModified) {
                await context.globalState.update('ngn.lsp.lastModified', latestLastModified);
            }
            if (metaChecked) {
                await context.globalState.update('ngn.lsp.lastCheck', now);
            }
            return;
        }

        outputChannel.appendLine(`Downloading language server from ${downloadUrl}`);
        await downloadFile(downloadUrl, archivePath);
        outputChannel.appendLine(`Extracting ${assetName}`);
        await execFileAsync('tar', ['-xzf', archivePath, '-C', serverDir]);
        await fs.promises.chmod(downloadedServerBinary, 0o755);
        await fs.promises.unlink(archivePath).catch(() => undefined);
        await context.globalState.update('ngn.lsp.lastCheck', now);
        if (latestEtag) {
            await context.globalState.update('ngn.lsp.etag', latestEtag);
        }
        if (latestLastModified) {
            await context.globalState.update('ngn.lsp.lastModified', latestLastModified);
        }
    };

    (async () => {
        const server = await resolveServerBinary();
        if (server.source === 'downloaded release binary') {
            await ensureServer();
        }

        outputChannel.appendLine(`Server binary path: ${server.command} (${server.source})`);
        const serverOptions: ServerOptions = {
            run: { command: server.command, transport: TransportKind.stdio },
            debug: { command: server.command, transport: TransportKind.stdio },
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
    })().catch((error) => {
        const message = `Failed to prepare ngn language server: ${error}`;
        outputChannel.appendLine(message);
        vscode.window.showErrorMessage(message);
    });
}

export function deactivate(): Thenable<void> | undefined {
    outputChannel.appendLine('ngn extension deactivating...');
    return client ? client.stop() : undefined;
}
