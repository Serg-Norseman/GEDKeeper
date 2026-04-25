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
using GKCore.Locales;
using GKUI.Platform;
using Sharprompt;

namespace GKcli.Commands;

internal class NoteMenuCommand : BaseCommand
{
    public NoteMenuCommand() : base("notes", LSID.RPNotes, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        while (true) {
            var selected = CommandController.SelectCommand(CommandCategory.Note, true, "Select a note operation");
            if (selected == CommandController.CMD_RETURN) break;
        }
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordListCommand"/>).
/// </summary>
internal class NoteListCommand : BaseCommand
{
    public NoteListCommand() : base("note_list", LSID.Find, CommandCategory.Note) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var selected = PromptHelper.SelectRecord(baseContext, GDMRecordType.rtNote, "Select a note", "Note: {0}", "No records.");
        if (selected != null)
            CommandController.SetVariable("selectedObj", selected);
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="NoteUpsertCommand"/>).
/// </summary>
internal class NoteAddCommand : BaseCommand
{
    public NoteAddCommand() : base("note_add", LSID.MIRecordAdd, CommandCategory.Note) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var text = Prompt.Input<string>("Enter the note's text");
        var noteRec = baseContext.Tree.CreateNote();
        noteRec.Lines.Text = text;
        baseContext.SetModified();
        PromptHelper.WriteLine("Note: {0}", text);
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="NoteUpsertCommand"/>).
/// </summary>
internal class NoteEditCommand : BaseCommand
{
    public NoteEditCommand() : base("note_edit", LSID.MIRecordEdit, CommandCategory.Note) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var noteRec = CommandController.GetVariable<GDMNoteRecord>("selectedObj");
        if (noteRec == null) {
            PromptHelper.WriteLine("Error: Expected an note record");
            return;
        }

        var text = Prompt.Input<string>("Enter the note's text", defaultValue: noteRec.Lines.Text);
        noteRec.Lines.Text = text;
        baseContext.SetModified();
        PromptHelper.WriteLine("Note: {0}", text);
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordDeleteCommand"/>).
/// </summary>
internal class NoteDeleteCommand : BaseCommand
{
    public NoteDeleteCommand() : base("note_delete", LSID.MIRecordDelete, CommandCategory.Note) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.DeleteRecord<GDMNoteRecord>(baseContext, "[darkred]Error:[/] [red]Expected an note record[/]");
    }
}


/// <summary>
/// For MCP use only.
/// </summary>
internal class NoteUpsertCommand : BaseCommand
{
    public NoteUpsertCommand() : base("note_upsert", null, CommandCategory.None) { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add new note or update existing. Provide 'xref' to edit; omit 'xref' to create. 'text' required for new notes.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit (omit for new)" },
                    ["text"] = new MCPToolProperty { Type = "string", Description = "Note text content" },
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetOptionalStr(args, "xref", null);
        string text = MCPHelper.GetOptionalStr(args, "text", null);

        bool isEdit = !string.IsNullOrEmpty(xref);
        if (isEdit) {
            var noteRec = baseContext.Tree.FindXRef<GDMNoteRecord>(xref);
            if (noteRec == null)
                return MCPContent.CreateSimpleContent($"❌ Note record not found: '{xref}'.");

            if (text != null) noteRec.Lines.Text = text;

            baseContext.SetModified();
            string preview = (text != null && text.Length > 10) ? text.Substring(0, 10) + "..." : text;
            return MCPContent.CreateSimpleContent($"✅ Note updated: {preview} with XRef `{noteRec.XRef}`");
        } else {
            if (string.IsNullOrEmpty(text))
                return MCPContent.CreateSimpleContent("❌ 'text' required for new note");

            var noteRec = baseContext.Tree.CreateNote();
            noteRec.Lines.Text = text;

            baseContext.SetModified();
            string preview = (text != null && text.Length > 10) ? text.Substring(0, 10) + "..." : text;
            return MCPContent.CreateSimpleContent($"✅ Note added: {text} with XRef `{noteRec.XRef}`");
        }
    }
}
