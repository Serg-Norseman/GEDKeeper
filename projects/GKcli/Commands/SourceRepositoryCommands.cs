/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;


internal class SourceListRepositoriesCommand : BaseCommand
{
    public SourceListRepositoriesCommand() : base("source_list_repositories", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class SourceAddRepositoryCommand : BaseCommand
{
    public SourceAddRepositoryCommand() : base("source_add_repository", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class SourceDeleteRepositoryCommand : BaseCommand
{
    public SourceDeleteRepositoryCommand() : base("source_delete_repository", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
