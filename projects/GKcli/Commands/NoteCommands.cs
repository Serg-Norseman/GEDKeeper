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


internal class NoteListCommand : RecordCommand
{
    public NoteListCommand() : base("note_list", LSID.Find, CommandCategory.Note) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var selected = SelectRecord(baseContext, GDMRecordType.rtNote, "Select a note", "Note: {0}", "No records.");
        if (selected != null)
            CommandController.SetVariable("selectedObj", selected);
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all notes in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtNote);
        return MCPHelper.PageableTable("notes", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Text preview |\n|---|---|";
            } else {
                var rec = (GDMNoteRecord)recList[index];
                string preview = rec.Lines.Text;
                if (preview != null && preview.Length > 80)
                    preview = preview.Substring(0, 80) + "...";
                return $"|{rec.XRef}|{preview}|";
            }
        });
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

        return MCPContent.CreateSimpleContent($"Note added: {text}");
    }
}


internal class NoteEditCommand : BaseCommand
{
    public NoteEditCommand() : base("edit_note", LSID.MIRecordEdit, CommandCategory.Note) { }

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


internal class NoteDeleteCommand : RecordCommand
{
    public NoteDeleteCommand() : base("note_delete", LSID.MIRecordDelete, CommandCategory.Note) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        DeleteRecord<GDMNoteRecord>(baseContext, "[darkred]Error:[/] [red]Expected an note record[/]");
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a note from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the note (e.g., 'N1')" }
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
            return MCPContent.CreateSimpleContent($"Note not found with XRef: {xref}");

        baseContext.DeleteRecord(noteRec);

        return MCPContent.CreateSimpleContent($"Note deleted: {xref}");
    }
}
