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

internal class GroupMenuCommand : BaseCommand
{
    public GroupMenuCommand() : base("groups", LSID.RPGroups, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Group, true, "Select a group operation");
    }
}


internal class GroupListCommand : BaseCommand
{
    public GroupListCommand() : base("group_list", LSID.Find, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtGroup, "Select a group", "Group: {0}", "No records.");
    }
}


internal class GroupAddCommand : BaseCommand
{
    public GroupAddCommand() : base("group_add", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class GroupEditCommand : BaseCommand
{
    public GroupEditCommand() : base("group_edit", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class GroupDeleteCommand : BaseCommand
{
    public GroupDeleteCommand() : base("group_delete", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
