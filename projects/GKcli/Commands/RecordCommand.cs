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


internal class RecordAddMultimediaLinkCommand : BaseCommand
{
    public RecordAddMultimediaLinkCommand() : base("record_add_multimedia", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
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
        // Empty for interactive mode
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


internal class RecordAddNoteCommand : BaseCommand
{
    public RecordAddNoteCommand() : base("record_add_note", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a note link to any record by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record (e.g., 'I1', 'F1', 'N2')" },
                    ["note_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the note record (e.g., 'N1')" }
                },
                Required = new List<string> { "record_xref", "note_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordXRef = MCPHelper.GetRequiredArgument(args, "record_xref");
        string noteXRef = MCPHelper.GetRequiredArgument(args, "note_xref");

        var record = baseContext.Tree.FindXRef<GDMRecord>(recordXRef);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {recordXRef}");

        if (!record.GetAccessibleSubstructures().HasFlag(GDMStructureType.NoteLink))
            return MCPContent.CreateSimpleContent($"Record type '{recordXRef}' ({record.RecordType}) does not support note links.");

        var noteRec = baseContext.Tree.FindXRef<GDMNoteRecord>(noteXRef);
        if (noteRec == null)
            return MCPContent.CreateSimpleContent($"Note record not found with XRef: {noteXRef}");

        var noteLink = new GDMNotes();
        noteLink.XRef = noteXRef;
        record.Notes.Add(noteLink);
        baseContext.SetModified();

        int noteIndex = record.Notes.IndexOf(noteLink);
        return MCPContent.CreateSimpleContent($"Note link added to record '{recordXRef}' at index {noteIndex}: note '{noteXRef}'");
    }
}


internal class RecordDeleteNoteCommand : BaseCommand
{
    public RecordDeleteNoteCommand() : base("record_delete_note", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a note link from a record by record XRef and note index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record (e.g., 'I1', 'F1', 'N2')" },
                    ["note_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the note link in the record's notes list" }
                },
                Required = new List<string> { "record_xref", "note_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordXRef = MCPHelper.GetRequiredArgument(args, "record_xref");
        int noteIndex = MCPHelper.GetIntArgument(args, "note_index", -1);

        var record = baseContext.Tree.FindXRef<GDMRecord>(recordXRef);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {recordXRef}");

        if (!record.HasNotes)
            return MCPContent.CreateSimpleContent($"Record '{recordXRef}' has no notes.");

        if (noteIndex < 0 || noteIndex >= record.Notes.Count)
            return MCPContent.CreateSimpleContent($"Invalid note index {noteIndex} for record '{recordXRef}' (has {record.Notes.Count} notes).");

        var noteLink = record.Notes[noteIndex];
        string noteInfo = $"note '{noteLink.XRef}'";

        record.Notes.RemoveAt(noteIndex);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Note link removed from record '{recordXRef}' at index {noteIndex}: {noteInfo}");
    }
}
