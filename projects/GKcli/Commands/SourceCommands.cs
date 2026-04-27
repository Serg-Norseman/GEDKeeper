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

internal class SourceMenuCommand : BaseCommand
{
    public SourceMenuCommand() : base("sources", LSID.RPSources, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Source, true, "Select a source operation");
    }
}


internal class SourceListCommand : BaseCommand
{
    public SourceListCommand() : base("source_list", LSID.Find, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtSource, "Select a source", "Source: {0}", "No records.");
    }
}


internal class SourceAddCommand : BaseCommand
{
    public SourceAddCommand() : base("source_add", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class SourceEditCommand : BaseCommand
{
    public SourceEditCommand() : base("source_edit", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class SourceDeleteCommand : BaseCommand
{
    public SourceDeleteCommand() : base("source_delete", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
