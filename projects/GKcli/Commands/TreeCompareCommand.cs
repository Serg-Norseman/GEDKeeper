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

internal class TreeCompareCommand : BaseCommand
{
    public TreeCompareCommand() : base("tree_compare", LangMan.LS(LSID.TreeCompare), CommandCategory.Tools)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
