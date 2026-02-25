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

namespace GKcli.Commands;

internal class MediaMenuCommand : BaseCommand
{
    public MediaMenuCommand() : base("media", LSID.RPMultimedia, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Multimedia, true, "Select a multimedia operation");
    }
}


internal class MediaListCommand : RecordCommand
{
    public MediaListCommand() : base("list_multimedia", LSID.Find, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        SelectRecord(baseContext, GDMRecordType.rtMultimedia, "Select a multimedia", "Multimedia: {0}", "No records.");
    }
}
