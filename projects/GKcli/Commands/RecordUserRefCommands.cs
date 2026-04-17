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

internal class RecordListUserRefsCommand : BaseCommand
{
    public RecordListUserRefsCommand() : base("record_list_userrefs", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all user references (custom notes) of a record by its XRef identifier",
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

        if (!record.HasUserReferences)
            return MCPContent.CreateSimpleContent($"Record '{recordXRef}' has no user references.");

        var rows = new List<string> {
            $"User references for record '{recordXRef}' ({record.UserReferences.Count}):",
            "| Index | Value | Type |",
            "|---|---|---|"
        };
        for (int i = 0; i < record.UserReferences.Count; i++) {
            var userRef = record.UserReferences[i];
            rows.Add($"|{i}|{userRef.StringValue}|{userRef.ReferenceType}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class RecordAddUserRefCommand : BaseCommand
{
    public RecordAddUserRefCommand() : base("record_add_userref", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a user reference (custom note) to any record by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record (e.g., 'I1', 'F1', 'N2')" },
                    ["string_value"] = new MCPToolProperty { Type = "string", Description = "The text value of the user reference" },
                    ["reference_type"] = new MCPToolProperty { Type = "string", Description = "The type/category of the reference (e.g., 'AFN', 'RFN', custom)" }
                },
                Required = new List<string> { "record_xref", "string_value" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordXRef = MCPHelper.GetRequiredArgument(args, "record_xref");
        string stringValue = MCPHelper.GetRequiredArgument(args, "string_value");
        string referenceType = MCPHelper.GetStringArgument(args, "reference_type", string.Empty);

        var record = baseContext.Tree.FindXRef<GDMRecord>(recordXRef);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {recordXRef}");

        if (!record.GetAccessibleSubstructures().HasFlag(GDMStructureType.UserReference))
            return MCPContent.CreateSimpleContent($"Record type '{recordXRef}' ({record.RecordType}) does not support user references.");

        var userRef = new GDMUserReference();
        userRef.StringValue = stringValue;
        userRef.ReferenceType = referenceType;
        record.UserReferences.Add(userRef);
        baseContext.SetModified();

        int refIndex = record.UserReferences.IndexOf(userRef);
        return MCPContent.CreateSimpleContent($"User reference added to record '{recordXRef}' at index {refIndex}: \"{stringValue}\" ({referenceType})");
    }
}


internal class RecordDeleteUserRefCommand : BaseCommand
{
    public RecordDeleteUserRefCommand() : base("record_delete_userref", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a user reference (custom note) from a record by record XRef and reference index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record (e.g., 'I1', 'F1', 'N2')" },
                    ["reference_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the user reference in the record's reference list" }
                },
                Required = new List<string> { "record_xref", "reference_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordXRef = MCPHelper.GetRequiredArgument(args, "record_xref");
        int referenceIndex = MCPHelper.GetIntArgument(args, "reference_index", -1);

        var record = baseContext.Tree.FindXRef<GDMRecord>(recordXRef);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {recordXRef}");

        if (!record.HasUserReferences)
            return MCPContent.CreateSimpleContent($"Record '{recordXRef}' has no user references.");

        if (referenceIndex < 0 || referenceIndex >= record.UserReferences.Count)
            return MCPContent.CreateSimpleContent($"Invalid reference index {referenceIndex} for record '{recordXRef}' (has {record.UserReferences.Count} references).");

        var userRef = record.UserReferences[referenceIndex];
        string refInfo = $"\"{userRef.StringValue}\" ({userRef.ReferenceType})";

        record.UserReferences.RemoveAt(referenceIndex);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"User reference removed from record '{recordXRef}' at index {referenceIndex}: {refInfo}");
    }
}
