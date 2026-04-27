/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;

internal class FamListChildrenCommand : BaseCommand
{
    public FamListChildrenCommand() : base("family_list_children", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class FamAddChildCommand : BaseCommand
{
    public FamAddChildCommand() : base("family_add_child", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class FamDeleteChildCommand : BaseCommand
{
    public FamDeleteChildCommand() : base("family_delete_child", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
