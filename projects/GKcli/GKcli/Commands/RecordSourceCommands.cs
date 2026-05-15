/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;

internal class RecordListSourceCitationsCommand : BaseCommand
{
    public RecordListSourceCitationsCommand() : base("record_list_sources", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class RecordAddSourceCitationCommand : BaseCommand
{
    public RecordAddSourceCitationCommand() : base("record_add_source", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class RecordDeleteSourceCitationCommand : BaseCommand
{
    public RecordDeleteSourceCitationCommand() : base("record_delete_source", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
