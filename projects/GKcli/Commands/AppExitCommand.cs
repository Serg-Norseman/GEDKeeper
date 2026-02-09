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

internal class AppExitCommand : BaseCommand
{
    public AppExitCommand() : base(CommandController.CMD_EXIT, LangMan.LS(LSID.MIExit), CommandCategory.Application)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        if (baseContext.Modified) {
            CommandController.WriteLine("The file has been modified.");
        }
    }
}


internal class MenuReturnCommand : BaseCommand
{
    public MenuReturnCommand() : base(CommandController.CMD_RETURN, LangMan.LS(LSID.Backward), CommandCategory.None)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}
