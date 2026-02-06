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

internal class FamMenuCommand : BaseCommand
{
    public FamMenuCommand() : base("families", LangMan.LS(LSID.RPFamilies), CommandCategory.Application)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.Instance.SelectCommand(CommandCategory.Family, true, "Select a family operation", baseContext);
    }
}
