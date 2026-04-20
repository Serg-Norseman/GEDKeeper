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

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a new note to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["text"] = new MCPToolProperty { Type = "string", Description = "Note text content" }
                },
                Required = new List<string> { "text" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string text = MCPHelper.GetRequiredArgument(args, "text");

        var noteRec = baseContext.Tree.CreateNote();
        noteRec.Lines.Text = text;
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Note added: {text} with XRef `{noteRec.XRef}`");
    }
}


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

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Edit an existing note record in the database. Only provided fields will be updated. Use 'xref' to identify the record to modify.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit" },
                    ["text"] = new MCPToolProperty { Type = "string", Description = "New text content of note record" },
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var noteRec = baseContext.Tree.FindXRef<GDMNoteRecord>(xref);
        if (noteRec == null)
            return MCPContent.CreateSimpleContent($"Note record not found: '{xref}'.");

        string text = MCPHelper.GetStringArgument(args, "text", null);
        if (text != null) {
            noteRec.Lines.Text = text;
        }

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Note updated: {text} with XRef `{noteRec.XRef}`");
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
