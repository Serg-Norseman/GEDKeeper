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

internal class RecordListNotesCommand : BaseCommand
{
    public RecordListNotesCommand() : base("record_list_notes", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all note links of a record by its XRef identifier",
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
        string recordXRef = MCPHelper.GetRequiredStr(args, "record_xref");

        var record = baseContext.Tree.FindXRef<GDMRecord>(recordXRef);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {recordXRef}");

        if (!record.HasNotes)
            return MCPContent.CreateSimpleContent($"Record '{recordXRef}' has no notes.");

        var rows = new List<string> {
            $"Note links for record '{recordXRef}' ({record.Notes.Count}):",
            "| Index | Note XRef | Text preview |",
            "|---|---|---|"
        };
        for (int i = 0; i < record.Notes.Count; i++) {
            var noteLink = record.Notes[i];
            var noteRec = baseContext.Tree.GetPtrValue<GDMNoteRecord>(noteLink);

            string preview = noteRec.Lines.Text;
            if (preview != null && preview.Length > 80)
                preview = preview.Substring(0, 80) + "...";

            rows.Add($"|{i}|{noteLink.XRef}|{preview}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class RecordAddNoteCommand : BaseCommand
{
    public RecordAddNoteCommand() : base("record_add_note", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
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
        string recordXRef = MCPHelper.GetRequiredStr(args, "record_xref");
        string noteXRef = MCPHelper.GetRequiredStr(args, "note_xref");

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
        // Not implemented yet
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
        string recordXRef = MCPHelper.GetRequiredStr(args, "record_xref");
        int noteIndex = MCPHelper.GetOptionalInt(args, "note_index", -1);

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
