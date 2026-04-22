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

/// <summary>
/// For console use only (for MCP - see <see cref="RecordListCommand"/>).
/// </summary>
internal class LocationListCommand : BaseCommand
{
    public LocationListCommand() : base("location_list", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
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
        string name = MCPHelper.GetRequiredStr(args, "name");
        double lat = MCPHelper.GetOptionalDbl(args, "lati", 0);
        double lng = MCPHelper.GetOptionalDbl(args, "long", 0);

        var locRec = baseContext.Tree.CreateLocation();
        locRec.LocationName = name;
        locRec.Map.Lati = lat;
        locRec.Map.Long = lng;

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Location record added: {locRec.XRef} - \"{name}\"");
    }
}


internal class LocationEditCommand : BaseCommand
{
    public LocationEditCommand() : base("location_edit", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Edit an existing location record to the database. Only provided fields will be updated. Use 'xref' to identify the record to modify.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "New name of the location item" },
                    ["lati"] = new MCPToolProperty { Type = "number", Description = "New latitude" },
                    ["long"] = new MCPToolProperty { Type = "number", Description = "New longitude" },
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredStr(args, "xref");

        var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(xref);
        if (locRec == null)
            return MCPContent.CreateSimpleContent($"Location not found with XRef: {xref}");

        string name = MCPHelper.GetOptionalStr(args, "name", null);
        if (name != null) {
            locRec.LocationName = name;
        }

        if (MCPHelper.HasArg(args, "lati")) {
            double lat = MCPHelper.GetOptionalDbl(args, "lati", 0);
            locRec.Map.Lati = lat;
        }

        if (MCPHelper.HasArg(args, "long")) {
            double lng = MCPHelper.GetOptionalDbl(args, "long", 0);
            locRec.Map.Long = lng;
        }

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Location record updated: {locRec.XRef} - \"{name}\"");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordDeleteCommand"/>).
/// </summary>
internal class LocationDeleteCommand : BaseCommand
{
    public LocationDeleteCommand() : base("location_delete", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
