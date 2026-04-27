/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;

internal class RecordListNotesCommand : BaseCommand
{
    public RecordListNotesCommand() : base("record_list_notes", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class RecordAddNoteCommand : BaseCommand
{
    public RecordAddNoteCommand() : base("record_add_note", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class RecordDeleteNoteCommand : BaseCommand
{
    public RecordDeleteNoteCommand() : base("record_delete_note", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
