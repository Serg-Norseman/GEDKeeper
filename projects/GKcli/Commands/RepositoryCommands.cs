/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore;
using GKCore.Locales;

namespace GKcli.Commands;

internal class RepositoryMenuCommand : BaseCommand
{
    public RepositoryMenuCommand() : base("repositories", LangMan.LS(LSID.RPRepositories), CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.Instance.SelectCommand(CommandCategory.Repository, true, "Select a repository operation");
    }
}


internal class RepositoryListCommand : BaseCommand
{
    public RepositoryListCommand() : base("list_repositories", LangMan.LS(LSID.Find), CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var selected = CommandController.SelectRecord(baseContext, GDMRecordType.rtRepository, "Select a repository", "Repository: {0}", "No records.");
    }
}
