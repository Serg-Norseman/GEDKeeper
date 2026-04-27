/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;


internal class IndiListEventsCommand : BaseCommand
{
    public IndiListEventsCommand() : base("individual_list_events", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiAddEventCommand : BaseCommand
{
    public IndiAddEventCommand() : base("individual_add_event", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiEditEventCommand : BaseCommand
{
    public IndiEditEventCommand() : base("individual_edit_event", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiDeleteEventCommand : BaseCommand
{
    public IndiDeleteEventCommand() : base("individual_delete_event", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
