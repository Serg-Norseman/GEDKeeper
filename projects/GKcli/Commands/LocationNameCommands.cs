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

internal class LocationListNamesCommand : BaseCommand
{
    public LocationListNamesCommand() : base("location_list_names", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all names of a location by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the location (e.g., 'L1')" }
                },
                Required = new List<string> { "location_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string locationXRef = MCPHelper.GetRequiredStr(args, "location_xref");

        var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
        if (locRec == null)
            return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");

        if (locRec.Names.Count <= 0)
            return MCPContent.CreateSimpleContent($"Location '{locationXRef}' has no names.");

        var rows = new List<string> {
            $"Names for location '{locationXRef}' ({locRec.Names.Count}):",
            "| Index | Name | Short name | Date |",
            "|---|---|---|---|"
        };
        for (int i = 0; i < locRec.Names.Count; i++) {
            var name = locRec.Names[i];
            var nameDate = GKUtils.GetDateDisplayString(name.Date.Value);
            rows.Add($"|{i}|{name.StringValue}|{name.Abbreviation}|{nameDate}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class LocationAddNameCommand : BaseCommand
{
    public LocationAddNameCommand() : base("location_add_name", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a name to a location record",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the location (e.g., 'L1')" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "The name to add to the location" },
                    ["short_name"] = new MCPToolProperty { Type = "string", Description = "The short name to add to the location" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = "Date string. Follow the GEDCOM date specification (from gedcom_date_spec)" }
                },
                Required = new List<string> { "location_xref", "name" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string locationXRef = MCPHelper.GetRequiredStr(args, "location_xref");
        string name = MCPHelper.GetRequiredStr(args, "name");
        string shortName = MCPHelper.GetOptionalStr(args, "short_name", string.Empty);
        string date = MCPHelper.GetOptionalStr(args, "date", string.Empty);

        var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
        if (locRec == null)
            return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");

        var locationName = new GDMLocationName();
        locationName.StringValue = name;
        locationName.Abbreviation = shortName;
        locationName.Date.ParseString(date);

        locRec.Names.Add(locationName);

        locRec.SortNames();
        baseContext.SetModified();

        int nameIndex = locRec.Names.IndexOf(locationName);
        return MCPContent.CreateSimpleContent($"Name added to location '{locationXRef}' at index {nameIndex}: '{name}'");
    }
}


internal class LocationEditNameCommand : BaseCommand
{
    public LocationEditNameCommand() : base("location_edit_name", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Edit a name of a location by location XRef and name index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the location (e.g., 'L1')" },
                    ["name_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the name in the location's name list" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "New value for the name" },
                    ["short_name"] = new MCPToolProperty { Type = "string", Description = "New short name" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = "New date string. Follow the GEDCOM date specification (from gedcom_date_spec)" }
                },
                Required = new List<string> { "location_xref", "name_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string locationXRef = MCPHelper.GetRequiredStr(args, "location_xref");
        int nameIndex = MCPHelper.GetOptionalInt(args, "name_index", -1);

        var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
        if (locRec == null)
            return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");

        if (locRec.Names.Count <= 0)
            return MCPContent.CreateSimpleContent($"Location '{locationXRef}' has no names.");

        if (nameIndex < 0 || nameIndex >= locRec.Names.Count)
            return MCPContent.CreateSimpleContent($"Invalid name index {nameIndex} for location '{locationXRef}' (has {locRec.Names.Count} names).");

        var locationName = locRec.Names[nameIndex];

        string newName = MCPHelper.GetOptionalStr(args, "name", null);
        if (newName != null) {
            locationName.StringValue = newName;
        }

        string shortName = MCPHelper.GetOptionalStr(args, "short_name", null);
        if (shortName != null) {
            locationName.Abbreviation = shortName;
        }

        string date = MCPHelper.GetOptionalStr(args, "date", null);
        if (date != null) {
            locationName.Date.ParseString(date);
        }

        locRec.SortNames();
        baseContext.SetModified();

        string nameInfo = $"'{locationName.StringValue}'";
        return MCPContent.CreateSimpleContent($"Name updated for location '{locationXRef}' at index {nameIndex}: {nameInfo}");
    }
}


internal class LocationDeleteNameCommand : BaseCommand
{
    public LocationDeleteNameCommand() : base("location_delete_name", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a name from a location by location XRef and name index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the location (e.g., 'L1')" },
                    ["name_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the name in the location's name list" }
                },
                Required = new List<string> { "location_xref", "name_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string locationXRef = MCPHelper.GetRequiredStr(args, "location_xref");
        int nameIndex = MCPHelper.GetOptionalInt(args, "name_index", -1);

        var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
        if (locRec == null)
            return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");

        if (locRec.Names.Count <= 0)
            return MCPContent.CreateSimpleContent($"Location '{locationXRef}' has no names.");

        if (nameIndex < 0 || nameIndex >= locRec.Names.Count)
            return MCPContent.CreateSimpleContent($"Invalid name index {nameIndex} for location '{locationXRef}' (has {locRec.Names.Count} names).");

        var locationName = locRec.Names[nameIndex];
        string nameInfo = $"'{locationName.StringValue}'";

        locRec.Names.RemoveAt(nameIndex);

        locRec.SortNames();
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Name removed from location '{locationXRef}' at index {nameIndex}: {nameInfo}");
    }
}
