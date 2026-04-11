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

namespace GKcli.Commands;

internal class LocationListCommand : BaseCommand
{
    public LocationListCommand() : base("location_list", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all locations in the database with pagination support (20 items per page)",
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
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtLocation);
        return MCPHelper.PageableTable("locations", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Name | Lati | Long |\n|---|---|---|---|";
            } else {
                var rec = (GDMLocationRecord)recList[index];
                string name = rec.GetNameByDate(null, true);
                string lat = rec.Map.Lati.ToString();
                string lng = rec.Map.Long.ToString();
                return $"|{rec.XRef}|{name}|{lat}|{lng}|";
            }
        });
    }
}
