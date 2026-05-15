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
using GKUI.Platform;

namespace GKcli.Commands;

internal class RepositoryMenuCommand : BaseCommand
{
    public RepositoryMenuCommand() : base("repositories", LSID.RPRepositories, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Repository, true, "Select a repository operation");
    }
}


internal class RepositoryListCommand : BaseCommand
{
    public RepositoryListCommand() : base("repository_list", LSID.Find, CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var selected = PromptHelper.SelectRecord(baseContext, GDMRecordType.rtRepository, "Select a repository", "Repository: {0}", "No records.");
    }
}


internal class RepositoryAddCommand : BaseCommand
{
    public RepositoryAddCommand() : base("repository_add", null, CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class RepositoryEditCommand : BaseCommand
{
    public RepositoryEditCommand() : base("repository_edit", null, CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class RepositoryDeleteCommand : BaseCommand
{
    public RepositoryDeleteCommand() : base("repository_delete", null, CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
