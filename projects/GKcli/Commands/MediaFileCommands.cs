/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;

internal class MediaListFilesCommand : BaseCommand
{
    public MediaListFilesCommand() : base("multimedia_list_files", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class MediaAddFileCommand : BaseCommand
{
    public MediaAddFileCommand() : base("multimedia_add_file", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class MediaEditFileCommand : BaseCommand
{
    public MediaEditFileCommand() : base("multimedia_edit_file", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class MediaDeleteFileCommand : BaseCommand
{
    public MediaDeleteFileCommand() : base("multimedia_delete_file", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
