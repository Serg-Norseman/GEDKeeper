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

internal class IndiMenuCommand : BaseCommand
{
    public IndiMenuCommand() : base("individuals", LangMan.LS(LSID.RPIndividuals), CommandCategory.Application)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.Instance.SelectCommand(CommandCategory.Individual, true, "Select a individual operation", baseContext);
    }
}
