/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Locales;
using GKUI.Platform;

namespace GKUI.Commands;

internal class FileLoadCommand : BaseCommand
{
    public FileLoadCommand() : base("load_gedcom", LangMan.LS(LSID.MIFileLoad), CommandCategory.File)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        string selectedFile = PromptHelper.SelectFile(GKUtils.GetAppPath(), ".ged");
        CommandController.LoadFile(baseContext, selectedFile);
    }
}
