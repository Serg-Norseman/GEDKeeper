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

internal class RecordListMultimediaCommand : BaseCommand
{
    public RecordListMultimediaCommand() : base("record_list_multimedia", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all multimedia links of a record by its XRef identifier",
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

        if (!record.HasMultimediaLinks)
            return MCPContent.CreateSimpleContent($"Record '{recordXRef}' has no multimedia links.");

        var rows = new List<string> {
            $"Multimedia links for record '{recordXRef}' ({record.MultimediaLinks.Count}):",
            "| Index | Media XRef | Primary | Title |",
            "|---|---|---|---|"
        };
        for (int i = 0; i < record.MultimediaLinks.Count; i++) {
            var mmLink = record.MultimediaLinks[i];
            var mmRec = baseContext.Tree.GetPtrValue<GDMMultimediaRecord>(mmLink);
            if (mmRec.FileReferences.Count <= 0) continue;

            var fileRef = mmRec.FileReferences[0];

            rows.Add($"|{i}|{mmLink.XRef}|{mmLink.IsPrimary}|{fileRef.Title}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class RecordAddMultimediaLinkCommand : BaseCommand
{
    public RecordAddMultimediaLinkCommand() : base("record_add_multimedia", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a multimedia link to any record by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record (e.g., 'I1', 'F1', 'N2')" },
                    ["multimedia_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the multimedia record (e.g., 'M1')" },
                    ["is_primary"] = new MCPToolProperty { Type = "boolean", Description = "Whether this is the primary multimedia (default: false)" }
                },
                Required = new List<string> { "record_xref", "multimedia_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordXRef = MCPHelper.GetRequiredArgument(args, "record_xref");
        string multimediaXRef = MCPHelper.GetRequiredArgument(args, "multimedia_xref");
        bool isPrimary = MCPHelper.GetBoolArgument(args, "is_primary", false);

        var record = baseContext.Tree.FindXRef<GDMRecord>(recordXRef);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {recordXRef}");

        if (!record.GetAccessibleSubstructures().HasFlag(GDMStructureType.MultimediaLink))
            return MCPContent.CreateSimpleContent($"Record type '{recordXRef}' ({record.RecordType}) does not support multimedia links.");

        var mmRec = baseContext.Tree.FindXRef<GDMMultimediaRecord>(multimediaXRef);
        if (mmRec == null)
            return MCPContent.CreateSimpleContent($"Multimedia record not found with XRef: {multimediaXRef}");

        var mmLink = new GDMMultimediaLink();
        mmLink.XRef = multimediaXRef;
        mmLink.IsPrimary = isPrimary;
        record.MultimediaLinks.Add(mmLink);
        baseContext.SetModified();

        int linkIndex = record.MultimediaLinks.IndexOf(mmLink);
        return MCPContent.CreateSimpleContent($"Multimedia link added to record '{recordXRef}' at index {linkIndex}: multimedia '{multimediaXRef}', primary {isPrimary}");
    }
}


internal class RecordDeleteMultimediaLinkCommand : BaseCommand
{
    public RecordDeleteMultimediaLinkCommand() : base("record_delete_multimedia", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a multimedia link from a record by record XRef and link index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record (e.g., 'I1', 'F1', 'N2')" },
                    ["link_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the multimedia link in the record's multimedia list" }
                },
                Required = new List<string> { "record_xref", "link_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordXRef = MCPHelper.GetRequiredArgument(args, "record_xref");
        int linkIndex = MCPHelper.GetIntArgument(args, "link_index", -1);

        var record = baseContext.Tree.FindXRef<GDMRecord>(recordXRef);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {recordXRef}");

        if (!record.HasMultimediaLinks)
            return MCPContent.CreateSimpleContent($"Record '{recordXRef}' has no multimedia links.");

        if (linkIndex < 0 || linkIndex >= record.MultimediaLinks.Count)
            return MCPContent.CreateSimpleContent($"Invalid multimedia link index {linkIndex} for record '{recordXRef}' (has {record.MultimediaLinks.Count} links).");

        var mmLink = record.MultimediaLinks[linkIndex];
        string linkInfo = $"multimedia '{mmLink.XRef}', title '{mmLink.Title}', primary {mmLink.IsPrimary}";

        record.MultimediaLinks.RemoveAt(linkIndex);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Multimedia link removed from record '{recordXRef}' at index {linkIndex}: {linkInfo}");
    }
}
