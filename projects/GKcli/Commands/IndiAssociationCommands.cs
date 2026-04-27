/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;

internal class IndiListAssociationsCommand : BaseCommand
{
    public IndiListAssociationsCommand() : base("individual_list_associations", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiAddAssociationCommand : BaseCommand
{
    public IndiAddAssociationCommand() : base("individual_add_association", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiEditAssociationCommand : BaseCommand
{
    public IndiEditAssociationCommand() : base("individual_edit_association", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiDeleteAssociationCommand : BaseCommand
{
    public IndiDeleteAssociationCommand() : base("individual_delete_association", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
