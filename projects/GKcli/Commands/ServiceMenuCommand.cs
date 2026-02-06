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

internal class ServiceMenuCommand : BaseCommand
{
    public ServiceMenuCommand() : base("service", LangMan.LS(LSID.MIService), CommandCategory.Application)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.Instance.SelectCommand(CommandCategory.Service, true, "Select a service operation", baseContext);
    }
}
