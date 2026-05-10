/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKcli.Commands;
using GKcli.Features;
using GKcli.MCP;
using GKcli.RAG;
using GKCore;
using GKUI.Platform;

namespace GKcli;

internal class Program
{
    static void Main(string[] args)
    {
        // Here are some important settings for working in the terminal.
        CLIAppHost.Startup(args);

        AppHost.InitSettings();
        try {
            AppHost.Instance.Init(args, false);

            // Check if running in MCP mode
            bool mcpMode = Array.IndexOf(args, "--mcp") >= 0;

            // Check if running in pure GEDCOM mode
            bool pureMode = Array.IndexOf(args, "--pure") >= 0;

            // Check if running in `Tool Discovery & Execution` mode
            bool tdeMode = Array.IndexOf(args, "--tde") >= 0;

            // Check if running in `Retrieval-Augmented Generation` mode
            bool ragMode = Array.IndexOf(args, "--rag") >= 0;

            // Common for all modes
            RAGHelper.SetAppDataPath(AppHost.GetAppDataPathStatic());

            if (mcpMode) {
                RunMCPServer(pureMode, tdeMode, ragMode);
            } else {
                RunInteractiveMode();
            }
        } finally {
            AppHost.DoneSettings();
        }
    }

    private static void RunMCPServer(bool pureMode, bool tdeMode, bool ragMode)
    {
        try {
            MCPController.InitFeatures(pureMode, tdeMode, ragMode);

            var server = new MCPServer();
            server.Run();
        } catch (Exception ex) {
            MCPServer.Log($"Fatal error during initialization: {ex}");
            Environment.Exit(1);
        }
    }

    private static void RunInteractiveMode()
    {
        CommandController.InitCommands();

        Console.Clear();
        PromptHelper.WriteMarkupLine("\n[darkcyan]GEDKeeper CLI[/]");
        while (true) {
            var selected = CommandController.SelectCommand(CommandCategory.Application, false, CLILangMan.LS(CLS.SelectCommand));
            if (selected == CommandController.CMD_EXIT) break;
        }
    }
}
