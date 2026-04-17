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

internal class RecordListSourceCitationsCommand : BaseCommand
{
    public RecordListSourceCitationsCommand() : base("record_list_sources", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all source citations of a record by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record (e.g., 'I1', 'F1', 'N2')" }
                },
                Required = new List<string> { "record_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordXRef = MCPHelper.GetRequiredArgument(args, "record_xref");

        var record = baseContext.Tree.FindXRef<GDMRecord>(recordXRef);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {recordXRef}");

        if (!record.HasSourceCitations)
            return MCPContent.CreateSimpleContent($"Record '{recordXRef}' has no source citations.");

        var rows = new List<string> {
            $"Source citations for record '{recordXRef}' ({record.SourceCitations.Count}):",
            "| Index | Source XRef | Source short title | Page | Certainty |",
            "|---|---|---|---|---|"
        };
        static string CertaintyLabel(int val) => val switch {
            0 => "unreliable",
            1 => "questionable",
            2 => "secondary",
            3 => "primary",
            _ => "unknown"
        };
        for (int i = 0; i < record.SourceCitations.Count; i++) {
            var citation = record.SourceCitations[i];
            var sourceRec = baseContext.Tree.GetPtrValue<GDMSourceRecord>(citation);

            rows.Add($"|{i}|{citation.XRef}|{sourceRec.ShortTitle}|{citation.Page}|{CertaintyLabel(citation.GetValidCertaintyAssessment())}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class RecordAddSourceCitationCommand : BaseCommand
{
    public RecordAddSourceCitationCommand() : base("record_add_source", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a source citation to any record by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record (e.g., 'I1', 'F1', 'N2')" },
                    ["source_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the source (e.g., 'S1')" },
                    ["page"] = new MCPToolProperty { Type = "string", Description = "Page or reference within the source" },
                    ["certainty"] = new MCPToolProperty { Type = "integer", Description = "Certainty assessment level (0 – unreliable, 1 – questionable, 2 – secondary, 3 – primary)" }
                },
                Required = new List<string> { "record_xref", "source_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordXRef = MCPHelper.GetRequiredArgument(args, "record_xref");
        string sourceXRef = MCPHelper.GetRequiredArgument(args, "source_xref");
        string page = MCPHelper.GetStringArgument(args, "page", string.Empty);
        int certainty = MCPHelper.GetIntArgument(args, "certainty", -1);

        var record = baseContext.Tree.FindXRef<GDMRecord>(recordXRef);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {recordXRef}");

        if (!record.GetAccessibleSubstructures().HasFlag(GDMStructureType.SourceCitation))
            return MCPContent.CreateSimpleContent($"Record type '{recordXRef}' ({record.RecordType}) does not support source citations.");

        var sourceRec = baseContext.Tree.FindXRef<GDMSourceRecord>(sourceXRef);
        if (sourceRec == null)
            return MCPContent.CreateSimpleContent($"Source not found with XRef: {sourceXRef}");

        var citation = new GDMSourceCitation();
        citation.XRef = sourceXRef;
        citation.Page = page;
        citation.CertaintyAssessment = certainty;
        record.SourceCitations.Add(citation);
        baseContext.SetModified();

        int citIndex = record.SourceCitations.IndexOf(citation);
        return MCPContent.CreateSimpleContent($"Source citation added to record '{recordXRef}' at index {citIndex}: source '{sourceXRef}', page '{page}', certainty {certainty}");
    }
}


internal class RecordDeleteSourceCitationCommand : BaseCommand
{
    public RecordDeleteSourceCitationCommand() : base("record_delete_source", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a source citation from a record by record XRef and citation index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record (e.g., 'I1', 'F1', 'N2')" },
                    ["citation_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the source citation in the record's citation list" }
                },
                Required = new List<string> { "record_xref", "citation_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordXRef = MCPHelper.GetRequiredArgument(args, "record_xref");
        int citationIndex = MCPHelper.GetIntArgument(args, "citation_index", -1);

        var record = baseContext.Tree.FindXRef<GDMRecord>(recordXRef);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {recordXRef}");

        if (!record.HasSourceCitations)
            return MCPContent.CreateSimpleContent($"Record '{recordXRef}' has no source citations.");

        if (citationIndex < 0 || citationIndex >= record.SourceCitations.Count)
            return MCPContent.CreateSimpleContent($"Invalid citation index {citationIndex} for record '{recordXRef}' (has {record.SourceCitations.Count} citations).");

        var citation = record.SourceCitations[citationIndex];
        string citInfo = $"source '{citation.XRef}', page '{citation.Page}', certainty {citation.CertaintyAssessment}";

        record.SourceCitations.RemoveAt(citationIndex);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Source citation removed from record '{recordXRef}' at index {citationIndex}: {citInfo}");
    }
}
