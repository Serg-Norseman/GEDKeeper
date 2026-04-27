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

internal class FamMenuCommand : BaseCommand
{
    public FamMenuCommand() : base("families", LSID.RPFamilies, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Family, true, "Select a family operation");
    }
}


internal class FamListCommand : BaseCommand
{
    public FamListCommand() : base("family_list", LSID.Find, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtFamily, "Select a family", "Family: {0}", "No records.");
    }
}


internal class FamAddCommand : BaseCommand
{
    public FamAddCommand() : base("family_add", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class FamEditCommand : BaseCommand
{
    public FamEditCommand() : base("family_edit", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class FamDeleteCommand : BaseCommand
{
    public FamDeleteCommand() : base("family_delete", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
