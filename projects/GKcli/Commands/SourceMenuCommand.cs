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

internal class SourceMenuCommand : BaseCommand
{
    public SourceMenuCommand() : base("sources", LangMan.LS(LSID.RPSources), CommandCategory.Application)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.Instance.SelectCommand(CommandCategory.Source, true, "Select a source operation", baseContext);
    }
}
