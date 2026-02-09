/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.IO;
using GKCore;
using GKCore.Locales;
using GKUI.Platform;
using Sharprompt;

namespace GKcli.Commands;

internal class FileSaveAsCommand : BaseCommand
{
    public FileSaveAsCommand() : base("saveas_gedcom", LangMan.LS(LSID.MIFileSaveAs), CommandCategory.File)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        string selectedFolder = PromptHelper.SelectFolder(GKUtils.GetAppPath());
        var fileName = Prompt.Input<string>("Enter a new file name (.ged)");
        CommandController.SaveFile(baseContext, Path.Combine(selectedFolder, fileName + ".ged"));
    }
}
