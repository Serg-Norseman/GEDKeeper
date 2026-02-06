/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Linq;
using GKCore;
using GKCore.Locales;
using Sharprompt;

namespace GKUI.Commands;

internal class FileLoadRecentCommand : BaseCommand
{
    public FileLoadRecentCommand() : base("recent_gedcom", LangMan.LS(LSID.MIMRUFiles), CommandCategory.File)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var files = AppHost.Options.MRUFiles.Select(f => f.FileName).ToList();
        if (files.Count > 0) {
            var selectedFile = Prompt.Select("Select a recent file", files, pageSize: 10);
            CommandController.LoadFile(baseContext, selectedFile);
        } else {
            CommandController.WriteLine("No recent files.");
        }
    }
}
