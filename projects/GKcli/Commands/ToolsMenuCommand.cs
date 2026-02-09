/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Locales;
using Sharprompt;

namespace GKcli.Commands;

internal class ToolsMenuCommand : BaseCommand
{
    public ToolsMenuCommand() : base("tools", LangMan.LS(LSID.MITreeTools), CommandCategory.Service)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.Instance.SelectCommand(CommandCategory.Tools, true, "Select a tool", baseContext);
    }
}
