/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKcli.Commands;
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

            Console.Clear();
            Console.WriteLine();
            PromptHelper.WriteMarkupLine("[darkcyan]GEDKeeper CLI[/]");

            while (true) {
                var selected = CommandController.SelectCommand(CommandCategory.Application, false, CLILangMan.LS(CLS.SelectCommand));
                if (selected == CommandController.CMD_EXIT) break;
            }
        } finally {
            AppHost.DoneSettings();
        }
    }
}
