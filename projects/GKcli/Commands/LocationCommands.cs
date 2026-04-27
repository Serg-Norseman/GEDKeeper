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

internal class LocationMenuCommand : BaseCommand
{
    public LocationMenuCommand() : base("locations", LSID.RPLocations, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Location, true, "Select a location operation");
    }
}


internal class LocationListCommand : BaseCommand
{
    public LocationListCommand() : base("location_list", LSID.Find, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtLocation, "Select a location", "Location: {0}", "No records.");
    }
}


internal class LocationAddCommand : BaseCommand
{
    public LocationAddCommand() : base("location_add", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class LocationEditCommand : BaseCommand
{
    public LocationEditCommand() : base("location_edit", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class LocationDeleteCommand : BaseCommand
{
    public LocationDeleteCommand() : base("location_delete", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
