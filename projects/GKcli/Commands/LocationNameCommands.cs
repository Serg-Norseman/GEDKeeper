/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;

internal class LocationListNamesCommand : BaseCommand
{
    public LocationListNamesCommand() : base("location_list_names", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class LocationAddNameCommand : BaseCommand
{
    public LocationAddNameCommand() : base("location_add_name", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class LocationEditNameCommand : BaseCommand
{
    public LocationEditNameCommand() : base("location_edit_name", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class LocationDeleteNameCommand : BaseCommand
{
    public LocationDeleteNameCommand() : base("location_delete_name", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
