/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Text.Json;
using GDModel;
using GKcli.MCP;
using GKCore;
using GKCore.Locales;
using GKCore.Utilities;
using GKUI.Platform;
using Sharprompt;

namespace GKcli.Commands;

internal abstract class RecordCommand : BaseCommand
{
    public RecordCommand(string sign, Enum lsid, CommandCategory category) : base(sign, lsid, category) { }

    protected static GDMRecord SelectRecord(BaseContext baseContext, GDMRecordType recordType, string prompt, string yesMsg, string noMsg)
    {
        GDMRecord result = null;

        var recList = baseContext.Tree.GetRecords(recordType);
        if (recList.Count > 0) {
            result = Prompt.Select(prompt, recList,
                pageSize: 10,
                textSelector: (GDMRecord r) => { return GKUtils.GetRecordName(baseContext.Tree, r, false); });

            PromptHelper.WriteLine(string.Format(yesMsg, GKUtils.GetRecordName(baseContext.Tree, result, false)));
        } else {
            PromptHelper.WriteLine(noMsg);
        }

        return result;
    }

    protected static void DeleteRecord<T>(BaseContext baseContext, string expectedMsg) where T : GDMRecord
    {
        var rec = CommandController.GetVariable<T>("selectedObj");
        if (rec == null) {
            PromptHelper.WriteLine(expectedMsg);
            return;
        }

        bool result = CommandController.GetConfirm("Удалить запись");
        if (result) {
            baseContext.DeleteRecord(rec);
            CommandController.SetVariable("selectedObj", null);
        }
    }
}


internal class RecordSearchCommand : RecordCommand
{
    public RecordSearchCommand() : base("record_search", LSID.Search, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    private static readonly Dictionary<string, GDMRecordType> RecordTypeMap = new Dictionary<string, GDMRecordType>(StringComparer.OrdinalIgnoreCase) {
        ["Individual"] = GDMRecordType.rtIndividual,
        ["Family"] = GDMRecordType.rtFamily,
        ["Note"] = GDMRecordType.rtNote,
        ["Source"] = GDMRecordType.rtSource,
        ["Repository"] = GDMRecordType.rtRepository,
        ["Multimedia"] = GDMRecordType.rtMultimedia,
        ["Group"] = GDMRecordType.rtGroup,
        ["Task"] = GDMRecordType.rtTask,
        ["Research"] = GDMRecordType.rtResearch,
        ["Communication"] = GDMRecordType.rtCommunication,
        ["Location"] = GDMRecordType.rtLocation
    };

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

        if (!RecordTypeMap.TryGetValue(recordTypeStr, out GDMRecordType recordType)) {
            string availableTypes = string.Join(", ", RecordTypeMap.Keys);
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


internal class RecordAddUserRefCommand : BaseCommand
{
    public RecordAddUserRefCommand() : base("record_add_userref", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
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
        // Empty for interactive mode
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


internal class RecordAddSourceCitationCommand : BaseCommand
{
    public RecordAddSourceCitationCommand() : base("record_add_source", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
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
        // Empty for interactive mode
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
