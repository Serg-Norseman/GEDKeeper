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

internal class SourceListCommand : BaseCommand
{
    public SourceListCommand() : base("list_sources", LangMan.LS(LSID.Find), CommandCategory.Source)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectRecord(baseContext, GDMRecordType.rtSource, "Select a source", "Source: {0}", "No records.");
    }
}
