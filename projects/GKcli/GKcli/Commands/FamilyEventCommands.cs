/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;


internal class FamListEventsCommand : BaseCommand
{
    public FamListEventsCommand() : base("family_list_events", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class FamAddEventCommand : BaseCommand
{
    public FamAddEventCommand() : base("family_add_event", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class FamEditEventCommand : BaseCommand
{
    public FamEditEventCommand() : base("family_edit_event", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class FamDeleteEventCommand : BaseCommand
{
    public FamDeleteEventCommand() : base("family_delete_event", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
