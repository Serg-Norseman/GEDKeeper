/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Locales;

namespace GKcli.Commands;

internal class RecordListCommand : BaseCommand
{
    public RecordListCommand() : base("record_list", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not applicable for MCP
    }
}


internal class RecordSearchCommand : BaseCommand
{
    public RecordSearchCommand() : base("record_search", LSID.Search, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class RecordInfoCommand : BaseCommand
{
    public RecordInfoCommand() : base("record_info", null, CommandCategory.Service) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class RecordDeleteCommand : BaseCommand
{
    public RecordDeleteCommand() : base("record_delete", null, CommandCategory.Service) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not applicable for MCP
    }
}


internal class RecordSetRestrictionCommand : BaseCommand
{
    public RecordSetRestrictionCommand() : base("record_set_restriction", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not applicable for MCP
    }
}
