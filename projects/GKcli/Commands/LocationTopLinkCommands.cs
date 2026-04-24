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

internal class LocationListTopLinksCommand : BaseCommand
{
    public LocationListTopLinksCommand() : base("location_list_top_links", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all top-level links of a location by its XRef identifier",
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

        if (locRec.TopLevels.Count <= 0)
            return MCPContent.CreateSimpleContent($"Location '{locationXRef}' has no top-level links.");

        var rows = new List<string> {
            $"Top-level links for location '{locationXRef}' ({locRec.TopLevels.Count}):",
            "| Index | Top Level XRef | Top Level Name | Date |",
            "|---|---|---|---|"
        };
        for (int i = 0; i < locRec.TopLevels.Count; i++) {
            var topLink = locRec.TopLevels[i];
            var toplevLoc = baseContext.Tree.GetPtrValue<GDMLocationRecord>(topLink);
            var toplevName = (toplevLoc == null) ? string.Empty : toplevLoc.GetNameByDate(topLink.Date.Value, true);
            var toplevelDate = GKUtils.GetDateDisplayString(topLink.Date.Value);
            rows.Add($"|{i}|{topLink.XRef}|{toplevName}|{toplevelDate}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class LocationAddTopLinkCommand : BaseCommand
{
    public LocationAddTopLinkCommand() : base("location_add_top_link", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a top-level link to a location record",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the location (e.g., 'L1')" },
                    ["top_link_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the top-level location to link to" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = RuntimeData.GEDCOMDateFormatDirective }
                },
                Required = new List<string> { "location_xref", "top_link_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string locationXRef = MCPHelper.GetRequiredStr(args, "location_xref");
        string topLinkXRef = MCPHelper.GetRequiredStr(args, "top_link_xref");
        string date = MCPHelper.GetOptionalStr(args, "date", string.Empty);

        var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
        if (locRec == null)
            return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");

        var targetLocRec = baseContext.Tree.FindXRef<GDMLocationRecord>(topLinkXRef);
        if (targetLocRec == null)
            return MCPContent.CreateSimpleContent($"Top-level location not found with XRef: {topLinkXRef}");

        var topLink = new GDMLocationLink();
        topLink.XRef = topLinkXRef;
        topLink.Date.ParseString(date);
        locRec.TopLevels.Add(topLink);

        locRec.SortTopLevels();
        baseContext.SetModified();

        int topLinkIndex = locRec.TopLevels.IndexOf(topLink);
        return MCPContent.CreateSimpleContent($"Top-level link added to location '{locationXRef}' at index {topLinkIndex}: '{topLinkXRef}'");
    }
}


internal class LocationEditTopLinkCommand : BaseCommand
{
    public LocationEditTopLinkCommand() : base("location_edit_top_link", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Edit a top-level link of a location by location XRef and top-link index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the location (e.g., 'L1')" },
                    ["top_link_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the top-level link in the location's top-links list" },
                    ["top_link_xref"] = new MCPToolProperty { Type = "string", Description = "New XRef identifier for the top-level location" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = RuntimeData.GEDCOMDateFormatDirective }
                },
                Required = new List<string> { "location_xref", "top_link_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string locationXRef = MCPHelper.GetRequiredStr(args, "location_xref");
        int topLinkIndex = MCPHelper.GetOptionalInt(args, "top_link_index", -1);

        var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
        if (locRec == null)
            return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");

        if (locRec.TopLevels.Count <= 0)
            return MCPContent.CreateSimpleContent($"Location '{locationXRef}' has no top-level links.");

        if (topLinkIndex < 0 || topLinkIndex >= locRec.TopLevels.Count)
            return MCPContent.CreateSimpleContent($"Invalid top-link index {topLinkIndex} for location '{locationXRef}' (has {locRec.TopLevels.Count} top-links).");

        var topLink = locRec.TopLevels[topLinkIndex];

        string newTopLinkXRef = MCPHelper.GetOptionalStr(args, "top_link_xref", null);
        if (newTopLinkXRef != null) {
            var targetLocRec = baseContext.Tree.FindXRef<GDMLocationRecord>(newTopLinkXRef);
            if (targetLocRec == null)
                return MCPContent.CreateSimpleContent($"Top-level location not found with XRef: {newTopLinkXRef}");

            topLink.XRef = newTopLinkXRef;
        }

        string date = MCPHelper.GetOptionalStr(args, "date", null);
        if (date != null) {
            topLink.Date.ParseString(date);
        }

        locRec.SortTopLevels();
        baseContext.SetModified();

        string topLinkInfo = $"'{topLink.StringValue}'";
        return MCPContent.CreateSimpleContent($"Top-level link updated for location '{locationXRef}' at index {topLinkIndex}: {topLinkInfo}");
    }
}


internal class LocationDeleteTopLinkCommand : BaseCommand
{
    public LocationDeleteTopLinkCommand() : base("location_delete_top_link", null, CommandCategory.Location) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a top-level link from a location by location XRef and top-link index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the location (e.g., 'L1')" },
                    ["top_link_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the top-level link in the location's top-links list" }
                },
                Required = new List<string> { "location_xref", "top_link_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string locationXRef = MCPHelper.GetRequiredStr(args, "location_xref");
        int topLinkIndex = MCPHelper.GetOptionalInt(args, "top_link_index", -1);

        var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
        if (locRec == null)
            return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");

        if (locRec.TopLevels.Count <= 0)
            return MCPContent.CreateSimpleContent($"Location '{locationXRef}' has no top-level links.");

        if (topLinkIndex < 0 || topLinkIndex >= locRec.TopLevels.Count)
            return MCPContent.CreateSimpleContent($"Invalid top-link index {topLinkIndex} for location '{locationXRef}' (has {locRec.TopLevels.Count} top-links).");

        var topLink = locRec.TopLevels[topLinkIndex];
        string topLinkInfo = $"'{topLink.StringValue}'";

        locRec.TopLevels.RemoveAt(topLinkIndex);

        locRec.SortTopLevels();
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Top-level link removed from location '{locationXRef}' at index {topLinkIndex}: {topLinkInfo}");
    }
}
