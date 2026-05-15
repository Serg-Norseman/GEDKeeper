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

internal class ResearchMenuCommand : BaseCommand
{
    public ResearchMenuCommand() : base("researches", LSID.RPResearches, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Research, true, "Select a research operation");
    }
}


internal class ResearchListCommand : BaseCommand
{
    public ResearchListCommand() : base("research_list", LSID.Find, CommandCategory.Research) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtResearch, "Select a research", "Research: {0}", "No records.");
    }
}


internal class ResearchAddCommand : BaseCommand
{
    public ResearchAddCommand() : base("research_add", null, CommandCategory.Research) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class ResearchEditCommand : BaseCommand
{
    public ResearchEditCommand() : base("research_edit", null, CommandCategory.Research) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class ResearchDeleteCommand : BaseCommand
{
    public ResearchDeleteCommand() : base("research_delete", null, CommandCategory.Research) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
