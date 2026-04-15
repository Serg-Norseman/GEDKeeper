/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using GKcli.MCP;
using GKcli.Resources;
using GKCore;
using GKCore.Locales;

namespace GKcli.Commands;

internal class AppExitCommand : BaseCommand
{
    public AppExitCommand() : base(CommandController.CMD_EXIT, LSID.MIExit, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        if (!baseContext.Modified) return;

        var result = CommandController.GetConfirm(LangMan.LS(LSID.FileSaveQuery));
        if (!result) return;

        var fsCmd = new FileSaveCommand();
        fsCmd.Execute(baseContext, obj);
    }
}


internal class MenuReturnCommand : BaseCommand
{
    public MenuReturnCommand() : base(CommandController.CMD_RETURN, LSID.Backward, CommandCategory.None) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class GEDCOMDateSpecCommand : BaseCommand
{
    public GEDCOMDateSpecCommand() : base("gedcom_date_spec", LSID.None, CommandCategory.Service) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Get GEDCOM date specification",
            InputSchema = MCPToolInputSchema.Empty
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        return MCPContent.CreateSimpleContent(GEDCOMDateSpecResource.GetSpec());
    }
}
