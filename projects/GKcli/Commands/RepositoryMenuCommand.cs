/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Locales;
using Sharprompt;

namespace GKUI.Commands;

internal class RepositoryMenuCommand : BaseCommand
{
    public RepositoryMenuCommand() : base("repository", LSID.RPRepositories, CommandCategory.Application)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var cmdList = CommandController.Instance.GetCommands(CommandCategory.Repository, true);
        var selected = Prompt.Select($"Select a repository operation", cmdList);
        if (selected != "return")
            CommandController.Instance.ExecuteCommand(selected, baseContext, obj);
    }
}
