/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKcli.Commands;
using GKcli.MCP;
using GKCore;
using GKUI.Platform;

namespace GKcli;


internal class Program
{
    static void Main(string[] args)
    {
        CLIAppHost.Startup(args);

        AppHost.InitSettings();
        try {
            AppHost.Instance.Init(args, false);

            // Check if running in MCP mode
            bool mcpMode = Array.IndexOf(args, "--mcp") >= 0;
            if (mcpMode) {
                RunMCPServer();
            } else {
                RunInteractiveMode();
            }
        } finally {
            AppHost.DoneSettings();
        }
    }

    private static void RunMCPServer()
    {
        try {
            var server = new MCPServer();
            server.Run();
        } catch (Exception ex) {
            MCPServer.Log($"Fatal error during initialization: {ex}");
            Environment.Exit(1);
        }
    }

    private static void RunInteractiveMode()
    {
        Console.Clear();
        Console.WriteLine();
        PromptHelper.WriteMarkupLine("[darkcyan]GEDKeeper CLI[/]");
        while (true) {
            var selected = CommandController.SelectCommand(CommandCategory.Application, false, CLILangMan.LS(CLS.SelectCommand));
            if (selected == CommandController.CMD_EXIT) break;
        }
    }
}
