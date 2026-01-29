/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore;
using GKUI.Commands;
using GKUI.Platform;
using Sharprompt;

namespace GKcli;

internal class Program
{
    private static BaseContext baseContext = new BaseContext(null);

    static void Main(string[] args)
    {
        CLIAppHost.ConfigureBootstrap();

        Console.Clear();
        Console.WriteLine("GEDKeeper CLI");

        while (true) {
            Console.WriteLine();

            var cmdList = CommandController.Instance.GetCommands(CommandCategory.Application);
            var selected = Prompt.Select($"Select a command", cmdList);
            CommandController.Instance.ExecuteCommand(selected, baseContext, null);

            if (selected == CommandCenter.CMD_EXIT) break;
        }
    }
}
