/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using System.Text;
using BSLib;
using GKCore;
using ZLMKit;
using ZLMKit.Database;
using ZLMKit.MCP;
using ZLMKit.Services;

namespace GKMCPPlugin.Utilities;

public class RuntimeContext : IRuntimeContext
{
    private BaseContext fBaseContext;
    private readonly FileSystemService fFileSystem;
    private readonly IMCPServer fMCPServer;

    public BaseContext BaseContext
    {
        get { return fBaseContext; }
        set { fBaseContext = value; }
    }

    public string DefaultTimeFormat { get { return "yyyy-MM-dd HH:mm:ss"; } }

    public bool MemoryEnabled { get; private set; }

    public bool ProfileEnabled { get; private set; }

    public bool TasksEnabled { get; private set; }

    public bool FTSEnabled { get; private set; }

    public IMCPServer MCPServer
    {
        get { return fMCPServer; }
    }

    public RuntimeContext(IMCPServer mcpServer)
    {
        bool dbEnabled = true;
        var dbPath = Path.Combine(AppHost.GetAppDataPathStatic(), "gkrag.db");
        if (dbEnabled) LLMDatabase.SetDBPath(dbPath);

        var allowedDirectories = "D:\\TEMP\\mem".Split(';');

        MemoryEnabled = true;
        ProfileEnabled = true;
        TasksEnabled = true;
        FTSEnabled = false;

        fMCPServer = mcpServer;
        fFileSystem = new FileSystemService(allowedDirectories);
    }

    public T Get<T>() where T : class
    {
        var typeToResolve = typeof(T);

        if (typeToResolve == typeof(IMCPServer)) {
            return fMCPServer as T;
        } else
        if (typeToResolve == typeof(IFileSystem)) {
            return fFileSystem as T;
        } else
        if (typeToResolve == typeof(ILogger)) {
            return Logger.GetLogger() as T;
        } else
        if (typeToResolve == typeof(BaseContext)) {
            return fBaseContext as T;
        }

        return null;
    }

    public static void Initialize()
    {
        // Without this, an attempt to save a file with a non-Latin name
        // resulted in the name appearing in 866 encoding (system default).
        try {
            Console.InputEncoding = Encoding.UTF8;
            Console.OutputEncoding = Encoding.UTF8;
        } catch (IOException) {
            // The console is redirected or unavailable
        }

        Logger.Init(Path.Combine(AppHost.GetAppDataPathStatic(), "GKMCPPlugin.log"));
        ZLMKit.MCP.MCPServer.SetLogger(Logger.GetLogger());
    }
}
