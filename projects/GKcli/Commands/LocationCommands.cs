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
        // Not implemented yet
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


internal class LocationAddCommand : BaseCommand
{
    public LocationAddCommand() : base("location_add", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a new location record to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Name of the location item" },
                    ["lati"] = new MCPToolProperty { Type = "number", Description = "Latitude" },
                    ["long"] = new MCPToolProperty { Type = "number", Description = "Longitude" },
                },
                Required = new List<string> { "name" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string name = MCPHelper.GetRequiredArgument(args, "name");
        double lat = MCPHelper.GetDoubleArgument(args, "lati", 0);
        double lng = MCPHelper.GetDoubleArgument(args, "long", 0);

        var locRec = baseContext.Tree.CreateLocation();
        locRec.LocationName = name;
        locRec.Map.Lati = lat;
        locRec.Map.Long = lng;

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Location record added: {locRec.XRef} - \"{name}\"");
    }
}


internal class LocationDeleteCommand : BaseCommand
{
    public LocationDeleteCommand() : base("location_delete", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a location record from the database by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the location (e.g., 'LOC1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(xref);
        if (locRec == null)
            return MCPContent.CreateSimpleContent($"Location not found with XRef: {xref}");

        baseContext.DeleteRecord(locRec);

        return MCPContent.CreateSimpleContent($"Location deleted: {xref}");
    }
}
