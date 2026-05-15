/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;

internal class RecordListUserRefsCommand : BaseCommand
{
    public RecordListUserRefsCommand() : base("record_list_userrefs", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class RecordAddUserRefCommand : BaseCommand
{
    public RecordAddUserRefCommand() : base("record_add_userref", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class RecordDeleteUserRefCommand : BaseCommand
{
    public RecordDeleteUserRefCommand() : base("record_delete_userref", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
