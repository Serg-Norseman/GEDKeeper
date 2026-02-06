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

internal class FileNewCommand : BaseCommand
{
    public FileNewCommand() : base("new_gedcom", LangMan.LS(LSID.MIFileNew), CommandCategory.File)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        baseContext.Clear();
        CommandController.WriteLine("Database created. Records: {0}.", baseContext.Tree.RecordsCount);
    }
}
