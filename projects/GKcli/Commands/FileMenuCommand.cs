/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Locales;

namespace GKUI.Commands;

internal class FileMenuCommand : BaseCommand
{
    public FileMenuCommand() : base("gedcom_files", LangMan.LS(LSID.MIFile), CommandCategory.Application)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.Instance.SelectCommand(CommandCategory.File, true, "Select a file operation", baseContext);
    }
}
