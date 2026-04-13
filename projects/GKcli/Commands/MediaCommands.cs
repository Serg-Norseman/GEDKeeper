/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using GDModel;
using GKcli.MCP;
using GKCore;
using GKCore.Locales;
using GKUI.Platform;

namespace GKcli.Commands;

internal class MediaMenuCommand : BaseCommand
{
    public MediaMenuCommand() : base("media", LSID.RPMultimedia, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Multimedia, true, "Select a multimedia operation");
    }
}


internal class MediaListCommand : BaseCommand
{
    public MediaListCommand() : base("multimedia_list", LSID.Find, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtMultimedia, "Select a multimedia", "Multimedia: {0}", "No records.");
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all multimedia records in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtMultimedia);
        return MCPHelper.PageableTable("multimedia", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Title | Type | File |\n|---|---|---|---|";
            } else {
                var rec = (GDMMultimediaRecord)recList[index];
                var fileRef = rec.FileReferences.Count > 0 ? rec.FileReferences[0] : null;
                if (fileRef == null) return string.Empty;

                string title = fileRef.Title;
                string mediaType = LangMan.LS(GKData.MediaTypes[(int)fileRef.MediaType]);
                string file = fileRef.StringValue;
                return $"|{rec.XRef}|{title}|{mediaType}|{file}|";
            }
        });
    }
}
