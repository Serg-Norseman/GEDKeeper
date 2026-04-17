/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using BSLib;
using GDModel;
using GKcli.MCP;
using GKCore;
using GKCore.Design;
using GKCore.Locales;
using GKCore.Utilities;
using GKUI.Platform;

namespace GKcli.Commands;

internal class RecordSearchCommand : BaseCommand
{
    public RecordSearchCommand() : base("record_search", LSID.Search, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = "record_search",
            Description = "Search for any records by name/title using fuzzy matching. Available record types: Individual, Family, Note, Source, Repository, Multimedia, Group, Task, Research, Communication, Location.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_type"] = new MCPToolProperty { Type = "string", Description = "Record type (e.g., 'Individual', 'Family', 'Note', 'Source', 'Repository', 'Multimedia', 'Group', 'Task', 'Research', 'Communication', 'Location')" },
                    ["search_text"] = new MCPToolProperty { Type = "string", Description = "Text to search for (name, title, etc.)" },
                    ["threshold"] = new MCPToolProperty { Type = "number", Description = "Fuzzy match threshold (0.0-1.0, default: 0.15). Lower = stricter match." }
                },
                Required = new List<string> { "record_type", "search_text" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordTypeStr = MCPHelper.GetRequiredArgument(args, "record_type");
        string searchText = MCPHelper.GetRequiredArgument(args, "search_text");
        double threshold = MCPHelper.GetDoubleArgument(args, "threshold", 0.15);

        if (!RuntimeData.RecordTypeMap.TryGetValue(recordTypeStr, out GDMRecordType recordType)) {
            string availableTypes = string.Join(", ", RuntimeData.RecordTypeMap.Keys);
            return MCPContent.CreateSimpleContent($"Unknown record type: '{recordTypeStr}'. Available types: {availableTypes}");
        }

        var recList = baseContext.Tree.GetRecords(recordType);
        if (recList.Count == 0)
            return MCPContent.CreateSimpleContent($"No {recordTypeStr.ToLower()} records in database.");

        var matches = new List<string>();
        foreach (var rec in recList) {
            string recordName = GKUtils.GetRecordName(baseContext.Tree, rec, false);
            int diff = SysUtils.GetDiffIndex(searchText, recordName);
            double matchThreshold = recordName.Length * threshold;

            if (diff <= matchThreshold) {
                matches.Add($"|{rec.XRef}|{recordName}|{diff}|");
            }
        }

        if (matches.Count == 0)
            return MCPContent.CreateSimpleContent($"No matches found for '{searchText}' in {recordTypeStr.ToLower()} records.");

        var lines = new List<string> {
            $"Search results for '{searchText}' in {recordTypeStr} records ({matches.Count} matches):",
            "| XRef | Name | Diff |",
            "|---|---|---|"
        };
        lines.AddRange(matches);
        return MCPContent.CreateSimpleContent(string.Join("\n", lines));
    }
}


internal class RecordInfoCommand : BaseCommand
{
    public RecordInfoCommand() : base("record_info", null, CommandCategory.Service) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Provides complete information about a record by its XRef identifier.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record (e.g., 'I1', 'F1', 'S1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var record = baseContext.Tree.FindXRef<GDMRecord>(xref);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {xref}");

        StringList ctx = new StringList();
        GKUtils.GetRecordContent(baseContext, record, ctx, RecordContentType.Quick);
        string text = BbCodeConverter.ToMarkdown(ctx.Text);

        return MCPContent.CreateSimpleContent(text);
    }
}
