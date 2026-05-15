/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;

internal class IndiListPersonalNamesCommand : BaseCommand
{
    public IndiListPersonalNamesCommand() : base("individual_list_personal_names", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiAddPersonalNameCommand : BaseCommand
{
    public IndiAddPersonalNameCommand() : base("individual_add_personal_name", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiEditPersonalNameCommand : BaseCommand
{
    public IndiEditPersonalNameCommand() : base("individual_edit_personal_name", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiDeletePersonalNameCommand : BaseCommand
{
    public IndiDeletePersonalNameCommand() : base("individual_delete_personal_name", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
