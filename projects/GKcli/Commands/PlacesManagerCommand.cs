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

internal class PlacesManagerCommand : BaseCommand
{
    public PlacesManagerCommand() : base("places_manager", LangMan.LS(LSID.PlacesManager), CommandCategory.Tools)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}
