/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;

internal class GroupListMembersCommand : BaseCommand
{
    public GroupListMembersCommand() : base("group_list_members", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class GroupAddMemberCommand : BaseCommand
{
    public GroupAddMemberCommand() : base("group_add_member", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class GroupDeleteMemberCommand : BaseCommand
{
    public GroupDeleteMemberCommand() : base("group_delete_member", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
